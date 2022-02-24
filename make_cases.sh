#! /usr/bin/bash

docker-compose run -e USER=user noresm_land_sites_platform python landsites_tools/simulation/make_cases.py $@
