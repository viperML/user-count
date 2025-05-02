{
  haskellPackages,
  lib,
}:
haskellPackages.callCabal2nix "user-count" (lib.cleanSource ./.) { }
