with import <nixpkgs> { };
{
  regular = mkShell {
    packages = [
      haskellPackages.cabal-install
      haskellPackages.haskell-language-server
      haskellPackages.fast-tags
      haskellPackages.threadscope
    ];
  };

  pkgShell = haskellPackages.developPackage {
    root = lib.cleanSource ./.;
    returnShellEnv = true;
  };
}
