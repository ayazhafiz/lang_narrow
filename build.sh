#!/bin/bash
# Run from repo root
dune build src/lang_narrow.js
cp \
 _build/default/src/lang_narrow.js \
 _build/default/src/lang_narrow.map \
 _build/default/src/js.ml \
 www
sed -i "s/CACHE_BUST/$(git rev-parse HEAD)/g" www/index.html
