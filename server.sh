#!/bin/bash

UID=${UID}  \
   GID=${UID}  \
   USR=${USER} \
   docker compose up --force-recreate --remove-orphans
