#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Please specify the pine version"
    exit 1
fi
PINE_VERSION="$1"
echo "Pine version: $PINE_VERSION"
docker build . -t pine:$PINE_VERSION
