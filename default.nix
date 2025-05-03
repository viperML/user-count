let
  sources = import ./npins;
  pkgs = import sources.nixpkgs { 
    config.allowUnfree = true;
  };
  inherit (pkgs) lib;
in
lib.fix (final: {
  package = pkgs.haskellPackages.callCabal2nix "user-count" (lib.cleanSource ./.) { };
  shells = with pkgs; {
    regular = mkShell {
      packages = [
        haskellPackages.cabal-install
        haskellPackages.haskell-language-server
        haskellPackages.fast-tags
        haskellPackages.threadscope
        nomad
      ];
    };
    package = haskellPackages.developPackage {
      root = lib.cleanSource ./.;
      returnShellEnv = true;
    };
  };
  streamImage = pkgs.dockerTools.streamLayeredImage (
    let
      port = "8080";
    in
    {
      name = "user-count";
      tag = "latest";
      contents = [
        final.package
      ];
      config = {
        Cmd = [
          "/bin/user-count"
        ];
        Env = [
          "PORT=${port}"
          "HOSTNAME=::"
        ];
        ExposedPorts = {
          "${port}/tcp" = { };
        };
      };
    }
  );
})
