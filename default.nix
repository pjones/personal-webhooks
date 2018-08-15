let
  # If no package source is specified then default to this URL:
  commit  = "5b8a24a40ce11bbe1cb6ebf33ddb5adaaebbd43a";
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs-channels/tarball/${commit}";
in

{ pkgs ? (import nixpkgs {}).pkgs
}:

pkgs.haskellPackages.callPackage ./personal-webhooks.nix { }
