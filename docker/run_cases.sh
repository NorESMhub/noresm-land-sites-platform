#! /usr/bin/bash

. ./docker/setup.sh

docker-compose -f ${DOCKER_COMPOSE_FILE} run -e USER=user noresm_land_sites_platform python landsites_tools/simulation/run_cases.py $@