let
  pkgs = import <nixpkgs> { };
  inherit (pkgs) lib;
in
lib.fix (final: {
  package = pkgs.callPackage ./package.nix { };
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
