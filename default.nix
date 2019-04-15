{ pkgs ? (import <nixpkgs> {})
}:

let
  # Remove the broken flag:
  unBreak = drv: pkgs.haskell.lib.overrideCabal drv (drv: { broken = false; });

  # Helper function to override Haskell packages:
  haskellOverride =
    pkgs.haskellPackages.override (orig: {
      overrides = pkgs.lib.composeExtensions
                    (orig.overrides or (_: _: {}))
                    (self: super: with pkgs.haskell.lib; {
                      table-layout = unBreak (dontCheck (doJailbreak super.table-layout));
                    });
      });

in

haskellOverride.callPackage ./personal-webhooks.nix { }
