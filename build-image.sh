#!/usr/bin/env bash

if [ ! -f "VERSION" ]; then
    echo "VERSION file not found!"
    exit 1
fi

PINE_VERSION=$(cat VERSION)
IMAGE_REPO="ahmadnazir/pine"
IMAGE_NAME="$IMAGE_REPO:$PINE_VERSION"
echo "Pine version: $PINE_VERSION"
echo "Checking if image $IMAGE_NAME exists in the registry..."

# Function to check if the image exists in Docker Hub
image_exists() {
    TOKEN=$(curl -s "https://auth.docker.io/token?service=registry.docker.io&scope=repository:$IMAGE_REPO:pull" | jq -r .token)
    RESPONSE=$(curl -s -o /dev/null -w "%{http_code}" \
      -H "Authorization: Bearer $TOKEN" \
      "https://registry-1.docker.io/v2/$IMAGE_REPO/manifests/$PINE_VERSION")

    if [ "$RESPONSE" -eq 200 ]; then
        return 0
    else
        return 1
    fi
}

# Helper function to build and push the image
build_and_push_image() {
    echo "Image $IMAGE_NAME does not exist in the registry. Proceeding with build and push."

    # Create and use a new Buildx builder instance
    docker buildx create --use
    docker buildx inspect --bootstrap

    # Build and push multi-platform Docker images
    docker buildx build \
      --platform linux/amd64,linux/arm64 \
      -t $IMAGE_NAME --push .
}

# Check if the image already exists
if image_exists; then
    echo "Image $IMAGE_NAME already exists in the registry. Skipping build and push."
else
    build_and_push_image
fi
