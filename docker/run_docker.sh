#! /usr/bin/bash

. ./docker/setup.sh

docker-compose -f $DOCKER_COMPOSE_FILE up
