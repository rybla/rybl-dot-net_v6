build:
  stack build

test:
  stack test

run: build
  .stack-work/dist/aarch64-osx/ghc-9.10.1/build/generate-website/generate-website

deploy: run
  CACHE_DIR=. bunx gh-pages --nojekyll --dist site
