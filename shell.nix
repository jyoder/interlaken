let
  pkgs = import <nixpkgs> {
    overlays = [pkgsOverlay];
  };

  pkgsOverlay = (nixSelf: nixSuper: {
    projectHaskellPkgs = nixSelf.haskellPackages.override (oldHaskellPkgs: {
      overrides = nixSelf.lib.composeExtensions (oldHaskellPkgs.overrides or (_: _: {}))  projectHaskellPkgsOverlay;
    });
  });

  projectHaskellPkgsOverlay = (hSelf: hSuper: {
    interlaken = hSelf.callCabal2nix "interlaken" ./. {};
  });
  
  devTools = with pkgs; [
    cabal-install
    dbmate
    haskellPackages.cabal-fmt
    haskellPackages.ghcid
    haskellPackages.haskell-language-server
    haskellPackages.hspec-discover
    sqlite-interactive
  ];

  shellHook = ''
    export DATABASE_URL="sqlite3:db/data/database.sqlite3"
    alias repl="cabal new-repl"
  '';
in
pkgs.projectHaskellPkgs.interlaken.env.overrideAttrs (oldEnv: {
  nativeBuildInputs = oldEnv.nativeBuildInputs ++ devTools;
  shellHook = shellHook;
})
