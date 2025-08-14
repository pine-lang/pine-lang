#!/usr/bin/env bash

set -e

echo "🌲 Starting Pine Playground..."
echo

# Function to clean up on exit
cleanup() {
    echo
    echo "🧹 Cleaning up..."
    docker compose -f playground.docker-compose.yml down
}

# Set up trap to clean up on script exit
trap cleanup EXIT

# Start the playground
echo "📦 Starting services (PostgreSQL + Pine + Init)..."
docker compose -f playground.docker-compose.yml up --build

echo
echo "🎉 Pine Playground has been shut down!" 