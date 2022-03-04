#!/bin/bash

DOCKER_COMPOSE_FILE=docker-compose.yml
if [[ $DEBUG == 1 ]]; then
    DOCKER_COMPOSE_FILE=docker-compose.dev.yml
fi

export DOCKER_COMPOSE_FILE
echo "Using $DOCKER_COMPOSE_FILE"
