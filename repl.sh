#!/bin/bash

UID=${UID}  \
GID=${UID}  \
USR=${USER} \
   docker-compose -f docker-compose.repl.yml up \
   --remove-orphans
