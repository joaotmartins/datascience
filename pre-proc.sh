#!/bin/bash

RSCRIPT_BIN="/c/Program Files/R/R-3.3.1/bin/Rscript.exe"

"$RSCRIPT_BIN" downloadData.R
./processOriginal.sh
"$RSCRIPT_BIN" buildNgrams.R
./wordparser.sh
