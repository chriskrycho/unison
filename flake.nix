{
  description = "Unison";
  inputs.haskellNix.url = "github:input-output-hk/haskell.nix";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachSystem [
      "x86_64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ] (system:
      let
        overlays = [
          haskellNix.overlay
          (final: prev: {
            unison-project = final.haskell-nix.project' {
              src = ./.;
              compiler-nix-name = "ghc8107";
              shell = {
                buildInputs = with pkgs; [ ];
                tools = let ormolu-ver = "0.4.0.0";
                in {
                  cabal = { };
                  ormolu = { version = ormolu-ver; };
                  haskell-language-server = {
                    version = "latest";
                    # specify flags via project file rather than a module override
                    # https://github.com/input-output-hk/haskell.nix/issues/1509
                    cabalProject = ''
                      packages: .
                      package haskell-language-server
                        flags: -brittany -fourmolu -stylishhaskell -hlint
                      constraints: ormolu == ${ormolu-ver}
                    '';
                  };
                };
              };
              branchMap = {
                "https://github.com/unisonweb/configurator.git"."e47e9e9fe1f576f8c835183b9def52d73c01327a" =
                  "unison";
                "https://github.com/unisonweb/shellmet.git"."2fd348592c8f51bb4c0ca6ba4bc8e38668913746" =
                  "topic/avoid-callCommand";
              };
            };
          })
        ];
        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };
        flake = pkgs.unison-project.flake { };
      in flake // { defaultPackage = flake.packages."unison-cli:exe:unison"; });
}
