build:
  stack build

test:
  stack test

run: build
  .stack-work/dist/aarch64-osx/ghc-9.10.1/build/rybl-dot-net-exe/rybl-dot-net-exe

deploy: run
  bun gh-pages --nojekyll --dist site
