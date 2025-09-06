build-rod:
    stack build --flag rybl-dot-net:dev

build-dev:
  stack build --flag rybl-dot-net:dev

run-dev: build-dev
  .stack-work/dist/aarch64-osx/ghc-9.10.1/build/rybl-dot-net-exe/rybl-dot-net-exe
