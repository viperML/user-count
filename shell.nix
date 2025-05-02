with import <nixpkgs> {
  config.allowUnfree = true;
};
{
  regular = mkShell {
    packages = [
      haskellPackages.cabal-install
      haskellPackages.haskell-language-server
      haskellPackages.fast-tags
      haskellPackages.threadscope
      nomad
    ];
  };

  pkgShell = haskellPackages.developPackage {
    root = lib.cleanSource ./.;
    returnShellEnv = true;
  };
}
