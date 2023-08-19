list:
  just --list

all: hpack fmt test

fmt: fmt-nix fmt-haskell

fmt-nix *args="":
  nixpkgs-fmt *.nix {{ args }}

fmt-haskell mode="inplace":
  ormolu \
    --mode {{ mode }} \
    $(fd '\.hs' src test)

hpack:
  hpack

test: hpack
  cabal test --test-show-details=streaming
  just doctest

doctest:
  doctest -isrc $(fd .hs src)

watch *args="": hpack
  ghcid \
    --command "cabal repl test:spec" \
    --test ':main {{ args }}' \
    --warnings \
    --max-messages=1
