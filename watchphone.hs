#! /usr/bin/env nix-shell
#! nix-shell -p "(import ./watchphone.nix)" -i runhaskell
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -threaded #-}

import Reflex
import Control.Lens
import Reflex.Vty

import Control.Monad.Trans (lift)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (toJSON, eitherDecode)
import Data.Aeson.TH (deriveFromJSON, deriveToJSON, defaultOptions, fieldLabelModifier)
import Data.Bifunctor (first)
import Data.Functor (void)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.List (drop)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Time (getCurrentTime)
import qualified Graphics.Vty as V
import Network.Wreq (postWith, defaults, header, responseBody)

tshow :: Show a => a -> T.Text
tshow = T.pack . show

data Req = Req
  { _ShipmentId :: Text }
deriveToJSON (defaultOptions { fieldLabelModifier = drop 1 }) ''Req

data Res = Res
  { _Description :: Text
  , _Event_Description :: Text
  , _event_date_time :: Text
  , _location_name :: Text
  , _shipment_number :: Text
  } deriving Show
deriveFromJSON (defaultOptions { fieldLabelModifier = drop 1 }) ''Res

url :: String
url = "http://tracking.linexsolutions.com/Shipment/GetTrackingLIst"

getStatus :: (Either String [Res] -> IO ()) -> IO ()
getStatus cb = do
  r <- postWith
    (defaults
      & header "Origin" .~ ["http://tracking.linexsolutions.com"]
      & header "Accept" .~ ["application/json"]
      )
    url
    (toJSON $ Req "SIHK1320037000278")
  let body = r ^. responseBody
  cb $ first (\e -> e <> " from body " <> (TL.unpack $ TL.decodeUtf8 body)) (eitherDecode body)

main :: IO ()
main = mainWidget $ do
  inp <- input
  ePb <- getPostBuild
  ePostBuildTime <- performEvent $ liftIO getCurrentTime <$ ePb
  eTick <- tickLossyFrom' $ (10,) <$> ePostBuildTime

  eRes <- performEventAsync $ (liftIO . getStatus) <$ (ePb <> void eTick)

  let eErr = fmapMaybe (^?_Left) eRes
  let eOk = fmapMaybe (^?_Right.to (Map.fromList . fmap (\r -> (_event_date_time r <> _Description r, r)))) eRes
  bLastUpdated <- hold Nothing (Just <$> (leftmost [ePostBuildTime,_tickInfo_lastUTC <$> eTick]))

  dOk <- holdDyn Map.empty eOk
  col $ do
    fixed 1 $ row $ do
      fixed 40 $ text "Description"
      fixed 30 $ text "Event"
      fixed 30 $ text "Time"
      fixed 20 $ text "Location"
      fixed 20 $ text "Shipment Num"
    fixed 1 $ fill '='

    listWithKey dOk $ \_ res -> do
      fixed 1 $ row $ do
        fixed 40 $ text $ current $ _Description <$> res
        fixed 30 $ text $ current $ _Event_Description <$> res
        fixed 30 $ text $ current $ _event_date_time <$> res
        fixed 20 $ text $ current $ _location_name <$> res
        fixed 20 $ text $ current $ _shipment_number <$> res
      fixed 1 $ fill '-'
    stretch blank
    fixed 1 $ row $ do
      stretch $ text "Still in Memphis..."
      fixed 14 $ text "Last Checked: "
      fixed 34 $ text $ maybe "" tshow <$> bLastUpdated

  pure $ fforMaybe inp $ \case
    V.EvKey (V.KChar 'c') [V.MCtrl] -> Just ()
    _ -> Nothing
