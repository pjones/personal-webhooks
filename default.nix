{ sources ? import nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, nix-hs ? import sources.nix-hs { inherit pkgs; }
, ghc ? "default"
}:

nix-hs {
  cabal = ./personal-webhooks.cabal;
  compiler = ghc;

  overrides = lib: self: super: with lib; {
    table-layout = unBreak super.table-layout;
  };
}
