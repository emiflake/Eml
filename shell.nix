let

  ormolu-overlay = import ./nix/ormolu-overlay.nix;
  pkgs = import <nixpkgs> { overlays = [ ormolu-overlay ]; };
  sources = import ./nix/sources.nix;
  all-hies = import sources.all-hies { };
  ormolu = import sources.ormolu { };

in pkgs.mkShell {
  buildInputs = [

    pkgs.hello
    pkgs.nixfmt
    pkgs.stack
    (all-hies.selection { selector = p: { inherit (p) ghc882; }; })
    ormolu.ormolu

  ];
}
