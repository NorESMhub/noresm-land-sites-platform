#! /usr/bin/bash

DOCKER_COMPOSE_FILE=docker-compose.yml
if [[ $DEBUG == 1 ]]; then
    DOCKER_COMPOSE_FILE=docker-compose.dev.yml
fi

echo "Using $DOCKER_COMPOSE_FILE"

docker-compose -f ${DOCKER_COMPOSE_FILE} run --user root noresm_land_sites_platform chown -R user:user /home/user/NorESM_LandSites_Platform/data /home/user/NorESM_LandSites_Platform/landsites_tools/custom_settings /home/user/NorESM_LandSites_Platform/notebooks
docker-compose -f $DOCKER_COMPOSE_FILE up
