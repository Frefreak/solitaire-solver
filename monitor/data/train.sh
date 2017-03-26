#!/usr/bin/env bash

# currently use tensorflow's retrain.py to train the network,
# might change to simpler models in the future
cp -r dataset jpeg_dataset
find jpeg_dataset -name "*.png" -exec convert {} {}.jpg \;
python retrain.py --image_dir=jpeg_dataset --validation_percentage=15
# output_graph.pb, output_labels.txt generated
