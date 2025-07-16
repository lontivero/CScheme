# shell.nix

with import /home/lontivero/Projects/nixpkgs { config.allowUnfree = true; };
let
  libs = [
    xorg.libX11
    xorg.libXrandr
    xorg.libX11.dev
    xorg.libICE
    xorg.libSM
    pkgs.zlib
    fontconfig.lib
  ];
  dependencies = [
    dotnet-sdk_8
    jetbrains.rider
  ];

in
mkShell {
  name = "dotnet-env";
  packages = dependencies;
  buildInputs = libs;

  DOTNET_ROOT = "${dotnet-sdk_8}";
  DOTNET_GLOBAL_TOOLS_PATH = "${builtins.getEnv "HOME"}/.dotnet/tools";
  shellHook = ''
    export PATH="$PATH:$DOTNET_GLOBAL_TOOLS_PATH"
  '';
}
