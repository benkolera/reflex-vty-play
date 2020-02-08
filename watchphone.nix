let
 rp = import ( builtins.fetchTarball {
   url = "https://github.com/reflex-frp/reflex-platform/archive/8f7a2cdab0bd46b54bf00ae4e60d2ca3a70e86eb.tar.gz";
   sha256 = "12rzbyi5qq2svf16b03r3j3j08jp0qy2p6jhx2sara3fhhc00f89";
 }) {};
in (rp.ghc.override {
  overrides = self: super: {
    reflex-vty = self.callCabal2nix "reflex-vty" (rp.hackGet ./deps/reflex-vty) {};
  };
}).ghcWithPackages (pkgs: with pkgs;
 [ reflex-vty wreq ]
)
