{
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import "${nixpkgs}" {
          inherit system;
        };
        haskell = pkgs.haskell.packages.ghc945;
        setupEnvironment = ''
          export NIX_GHC=${haskell.ghc.withPackages (p: operational-mocks.buildInputs)}/bin/ghc
        '';
        operational-mocks =
          pkgs.haskell.lib.overrideCabal
            (haskell.callCabal2nix "operational-mocks" ./. { })
            (old: {
              preBuild = setupEnvironment;
              buildDepends = (old.buildDepends or [ ]) ++ [
                haskell.doctest
                pkgs.fd
                pkgs.just
                pkgs.nixpkgs-fmt
                pkgs.ormolu
              ];
              postCheck = ''
                just doctest
                just fmt-nix --check
                just fmt-haskell check
              '';
            });
      in
      {
        packages.default = operational-mocks;
        devShells = {
          default = pkgs.mkShell {
            shellHook = setupEnvironment;
            inputsFrom = [ self.packages.${system}.default ];
            nativeBuildInputs = with pkgs; [
              cabal-install
              ghcid
              haskell.cabal2nix
              (haskell.ghc.withPackages (p: operational-mocks.buildInputs))
              (haskell-language-server.override { supportedGhcVersions = [ "945" ]; })
              nil
            ];
          };
        };
      }
    );
}
