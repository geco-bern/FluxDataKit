#!/bin/bash

echo "flux formatting"
bsub -W 72:00 -u $USER -J "grabbing flux data" -R "rusage[mem=48000]" "Rscript --vanilla ./data-raw/rscript_format_site_data.R"
