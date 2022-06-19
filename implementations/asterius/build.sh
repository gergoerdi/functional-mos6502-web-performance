mkdir -p _build
docker run -it --rm -v $(pwd):/workspace -w /workspace terrorjack/asterius \
  ahc-link --browser \
         --input-hs src/Driver.hs \
         --export-function run \
         --input-mjs=index.js --no-main \
         --output-directory _build
