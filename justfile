build:
  stack build

test:
  stack test

generate: build
  .stack-work/dist/aarch64-osx/ghc-9.10.1/build/generate-website/generate-website

deploy:
  CACHE_DIR=. bunx gh-pages --nojekyll --dist site
