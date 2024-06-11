#!/usr/bin/env bash

if [ ! -f "VERSION" ]; then
    echo "VERSION file not found!"
    exit 1
fi

PINE_VERSION=$(cat VERSION)
echo "Pine version: $PINE_VERSION"

docker build . -t ahmadnazir/pine:$PINE_VERSION
