# nix-build --dry-run --show-trace shell-vscode.nix
let
  stdpkgs = import <nixpkgs> {};

  vscode_ext =
      let
        overrideBuildVscodeMarketplaceExtension =
          self: super: {
            # pkgs/misc/vscode-extensions/vscode-utils.nix
            vscode-utils = super.vscode-utils // {
              buildVscodeMarketplaceExtension = 
                # save extension version to _refver for checking update
                # !!! pass the whole mktplcRef will fail to build the derivation !!!
                a@{...}: super.vscode-utils.buildVscodeMarketplaceExtension 
                            (a // { _refver = a.mktplcRef.version; });
            };
          };
      in
        (import <nixpkgs> { overlays = [ overrideBuildVscodeMarketplaceExtension ]; }).vscode-extensions;

  vscode_defver = stdpkgs.vscode.version;
  vscode_newver = "1.54.1";
  vscode_new = super: version: plat: archive_fmt: {
                 version = version;
                 src = super.fetchurl {
                   name = "VSCode_${version}_${plat}.${archive_fmt}";
                   url = "https://vscode-update.azurewebsites.net/${version}/${plat}/stable";
                   sha256 = "e1876605d5ce21a899ded01f86a2ac910a84bb4ded13eb2bf4d5cd14421e3c13";
                 };
               };

  pkgs = let
           vscode_override = version: builtins.trace "install vscode ${version}" (
                      self: super: {
                        vscode = super.vscode.overrideAttrs (old:
                            # pkgs/applications/editors/vscode/vscode.nix
                            let
                              inherit (super.stdenv.hostPlatform) system;

                              plat = {
                                x86_64-linux = "linux-x64";
                                x86_64-darwin = "darwin";
                                aarch64-linux = "linux-arm64";
                                armv7l-linux = "linux-armhf";
                              }.${system};

                              archive_fmt = if system == "x86_64-darwin" then "zip" else "tar.gz";
                            in
                              vscode_new super version plat archive_fmt);
                      });
           vscode = if builtins.trace "default vscode = ${vscode_defver}" vscode_defver < vscode_newver then
                      [ (vscode_override vscode_newver) ]
                    else
                      [];

           overrides = vscode;
         in
           if builtins.length overrides > 0 then
              import <nixpkgs> { overlays = overrides; }
           else
              stdpkgs;

  /* Only derivation made by buildVscodeMarketplaceExtension can be installed with extensionFromVscodeMarketplace.
     Need to check pkgs/misc/vscode-extensions/default.nix for how the derivation is made.
     Derivation made by callPackage has extra dependencies. e.g. cpptools, python, remote-ssh */

  default_ver = name: default: builtins.trace "default ${name} = ${default._refver}" default._refver;

  get_extension = name: default: newver: override: attrs:
                    if (isNull default) || ((default_ver name default) < newver) then
                      override attrs
                    else
                      default;

  nix-ide = get_extension "nix-ide" vscode_ext.jnoortheen.nix-ide "0.1.7"
            pkgs.vscode-utils.buildVscodeMarketplaceExtension
            {
                mktplcRef = {
                  name = "nix-ide";
                  publisher = "jnoortheen";
                  version = "0.1.7";
                  sha256 = "1bw4wyq9abimxbhl7q9g8grvj2ax9qqq6mmqbiqlbsi2arvk0wrm";
                };
            };

  pylance = get_extension "pylance" vscode_ext.ms-python.vscode-pylance "2021.3.0"
            pkgs.vscode-utils.buildVscodeMarketplaceExtension
            {
                mktplcRef = {
                  name = "vscode-pylance";
                  publisher = "MS-python";
                  version = "2021.3.0";
                  sha256 = "6477e160cab5a7a9eea99504ac1d87d10f9c4e8cc3b6a86e730e3bb5c785fe2e";
                };
                buildInputs = [ pkgs.nodePackages.pyright ];
            };

  remote-wsl = get_extension "remote-wsl" (vscode_ext.ms-vscode-remote.remote-wsl or null) "0.54.2"
               pkgs.vscode-utils.buildVscodeMarketplaceExtension
               {
                   mktplcRef = {
                       name = "remote-wsl";
                       publisher = "ms-vscode-remote";
                       version = "0.54.2";
                       sha256 = "59571cc3719bfe1dfae2a0e19b2f74e2b4993bfb0fc2d66b1ed5384ad954bd02";
                   };
               };

  # required by ms-python
  jupyter = get_extension "Jupyter" (vscode_ext.ms-toolsai.jupyter or null) "2021.3.619093157"
               pkgs.vscode-utils.buildVscodeMarketplaceExtension
               {
                   mktplcRef = {
                       name = "jupyter";
                       publisher = "ms-toolsai";
                       version = "2021.3.619093157";
                       sha256 = "0a2ed681ebc47049745dcbe2cea3258bb53cf9ba473e49c9ba678f8de4962f21";
                   };
               };

  # cpptools = pkgs.callPackage <nixpkgs/pkgs/misc/vscode-extensions/cpptools> {};
  cpptools = get_extension "cpptools" vscode_ext.ms-vscode.cpptools "1.2.2"
             pkgs.vscode-extensions.ms-vscode.cpptools.override
             {
               fetchurl = (a@{...}: pkgs.fetchurl {
                 name = "ms-vscode-cpptools.zip";
                 url = "https://github.com/microsoft/vscode-cpptools/releases/download/1.2.2/cpptools-linux.vsix";
                 sha256 = "f1a9f97c55af8c18d7e2d40f388da77f53061dfbb971191872f154ea86b78d95";
               });
             };

  ms-python = get_extension "ms-python" vscode_ext.ms-python.python "2021.2.636928669"
             pkgs.vscode-extensions.ms-python.python.override
             {
               fetchurl = (a@{...}: pkgs.fetchurl {
                 name = "ms-python-python.zip";
                 url = "https://github.com/microsoft/vscode-python/releases/download/2021.2.636928669/ms-python-release.vsix";
                 sha256 = "bc3c42305befd8a2a3617fdc77800a18ef1d396b9b2c9a0425d65a4ca6d89846";
               });
             };

  remote-ssh = get_extension "remote-ssh" vscode_ext.ms-vscode-remote.remote-ssh "0.65.1"
             pkgs.vscode-extensions.ms-vscode-remote.remote-ssh.override
             {
               vscode-utils = pkgs.vscode-utils // {
                 buildVscodeMarketplaceExtension = (a@{...}:
                   let
                       mktplcRef = {
                         name = "remote-ssh";
                         publisher = "ms-vscode-remote";
                         version = "0.65.1";
                         sha256 = "3929fdba490d385c1756849d9b1644e0dc41d66930190b59a895a6437ab8e675";
                       };
                   in
                       pkgs.vscode-utils.buildVscodeMarketplaceExtension (a // { inherit mktplcRef; }));
               };
             };

  exts = [ nix-ide cpptools jupyter pylance ms-python remote-ssh remote-wsl ];
  extensions = map (e: builtins.trace "install ${e.name}" e) exts;

  vscode-with-extensions = pkgs.vscode-with-extensions.override {
                             vscodeExtensions = extensions;
                           };

  listPypkgs = (ps: with ps; [ pip numpy selenium ]);
  pythonEnv = pkgs.python38.withPackages listPypkgs;

in pkgs.mkShell {
  buildInputs = [
    pythonEnv
    vscode-with-extensions
    pkgs.rnix-lsp
  ];
}
