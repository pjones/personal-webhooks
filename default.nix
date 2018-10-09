let
  # If no package source is specified then default to this URL:
  commit  = "5b8a24a40ce11bbe1cb6ebf33ddb5adaaebbd43a";
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs-channels/tarball/${commit}";

  # Helper function to override Haskell packages:
  haskellOverride = pkgs:
    pkgs.haskellPackages.override (orig: {
      overrides = pkgs.lib.composeExtensions
                    (orig.overrides or (_: _: {}))
                    (self: super: with pkgs.haskell.lib; {
                      table-layout = dontCheck (doJailbreak super.table-layout);
                    });
      });

in

{ pkgs ? (import nixpkgs {}).pkgs
}:

(haskellOverride pkgs).callPackage ./personal-webhooks.nix { }
