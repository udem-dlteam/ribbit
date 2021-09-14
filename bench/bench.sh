#!/bin/zsh
./compare.sh --clean
./compare.sh
python3 ./aggregate.py >> aggregated.txt
