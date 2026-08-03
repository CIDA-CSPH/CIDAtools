#!/bin/zsh

R_RESOURCE_DIR="./R/inst/extdata/"
PYTHON_RESOURCE_DIR="./python/src/cidatools/resources"

rsync -av ./resources/ "$R_RESOURCE_DIR" && \
rsync -av ./resources/ "$PYTHON_RESOURCE_DIR" &&
git add "$R_RESOURCE_DIR" "$PYTHON_RESOURCE_DIR"
