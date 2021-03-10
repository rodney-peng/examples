# nix-build --dry-run --show-trace shell-vscode.nix
let
  stdpkgs = import <nixpkgs> {};
  vscode_ver = stdpkgs.vscode.version;

  vscode_ext =
      let
        overrideBuildVscodeMarketplaceExtension =
          self: super: {
            # pkgs/misc/vscode-extensions/vscode-utils.nix
            vscode-utils = super.vscode-utils // {
              buildVscodeMarketplaceExtension = 
                # save extension version to _refver for checking update
                # !!! pass the whole mktplcRef will fail to build derivation !!!
                a@{...}: super.vscode-utils.buildVscodeMarketplaceExtension 
                            (a // { _refver = a.mktplcRef.version; });
            };
          };
      in
        (import <nixpkgs> { overlays = [ overrideBuildVscodeMarketplaceExtension ]; }).vscode-extensions;

  pkgs = if builtins.trace "default vscode = ${vscode_ver}" vscode_ver < "1.54" then
            let
              version = "1.54.1";
              vscode = self: super: {
                    # pkgs/applications/editors/vscode/vscode.nix
                    vscode = super.vscode.overrideAttrs (old:
                        let
                          inherit (super.stdenv.hostPlatform) system;

                          plat = {
                            x86_64-linux = "linux-x64";
                            x86_64-darwin = "darwin";
                            aarch64-linux = "linux-arm64";
                            armv7l-linux = "linux-armhf";
                          }.${system};

                          archive_fmt = if system == "x86_64-darwin" then "zip" else "tar.gz";
                        in {
                          inherit version;
                          src = super.fetchurl {
                            name = "VSCode_${version}_${plat}.${archive_fmt}";
                            url = "https://vscode-update.azurewebsites.net/${version}/${plat}/stable";
                            sha256 = "e1876605d5ce21a899ded01f86a2ac910a84bb4ded13eb2bf4d5cd14421e3c13";
                          };
                        });
                };
            in
              builtins.trace "install vscode ${version}"
                import <nixpkgs> { overlays = [ vscode ]; }
         else
            stdpkgs;

  get_extension = ext: mktplcRef:
                    if (builtins.trace "default ${mktplcRef.name} = ${ext._refver}" ext._refver) < mktplcRef.version then
                      pkgs.vscode-utils.extensionFromVscodeMarketplace mktplcRef
                    else
                      ext;

  nix-ide = get_extension vscode_ext.jnoortheen.nix-ide
            {
              name = "nix-ide";
              publisher = "jnoortheen";
              version = "0.1.7";
              sha256 = "1bw4wyq9abimxbhl7q9g8grvj2ax9qqq6mmqbiqlbsi2arvk0wrm";
            };

  ms-python = get_extension vscode_ext.ms-python.python
              {
                name = "python";
                publisher = "ms-python";
                version = "2021.2.633441544";
                sha256 = "11f89b561a0821b25d7cb612cad40c064e95676986497bcf658b0dca7f78e7e8";
              };
  
  pylance = get_extension vscode_ext.ms-python.vscode-pylance
            {
              name = "vscode-pylance";
              publisher = "MS-python";
              version = "2021.3.0";
              sha256 = "6477e160cab5a7a9eea99504ac1d87d10f9c4e8cc3b6a86e730e3bb5c785fe2e";
            };

  remote-ssh = get_extension vscode_ext.ms-vscode-remote.remote-ssh
               {
                  name = "remote-ssh";
                  publisher = "ms-vscode-remote";
                  version = "0.65.1";
                  sha256 = "3929fdba490d385c1756849d9b1644e0dc41d66930190b59a895a6437ab8e675";
               };

  exts = [ nix-ide ms-python pylance remote-ssh ];
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
