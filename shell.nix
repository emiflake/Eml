let

  ormolu-overlay = import ./nix/ormolu-overlay.nix;
  pkgs = import <nixpkgs> { overlays = [ ormolu-overlay ]; };
  sources = import ./nix/sources.nix;
  all-hies = import sources.all-hies { };
  ormolu = import sources.ormolu { };

in pkgs.mkShell {
  buildInputs = with pkgs; [

    nixfmt
    stack
    hlint
    ghcid
    haskellPackages.ormolu

    gnumake
    ghc
    ispell
    haskellPackages.hindent

    # helpful
    bat
    nodejs
  ];
}
