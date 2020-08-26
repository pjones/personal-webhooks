# Load an interactive environment:
{ sources ? import ./nix/sources.nix
, pkgs ? import sources.nixpkgs { config = { allowBroken = true; }; }
, ghc ? "default"
}:
let
  stan = (pkgs.haskellPackages.override (orig: {
    overrides = pkgs.lib.composeExtensions
      (orig.overrides or (_: _: { }))
      (self: super: with pkgs.haskell.lib; {
        microaeson = doJailbreak super.microaeson;
      });
  })).stan;
in
(import ./. {
  inherit ghc;
}).interactive.overrideAttrs (orig: {
  buildInputs =
    (orig.buildInputs or [ ])
    ++ [ stan ];
})
