#!/bin/bash

UID=${UID}  \
GID=${UID}  \
USR=${USER} \
   docker-compose -f docker-compose.test.yml up \
   --remove-orphans
