#!/usr/bin/env bash

set -e

if [[ ! -d /ctsm-api/resources/ctsm ]]; then
    cd /ctsm-api/resources/

    git clone https://github.com/NorESMhub/NorESM ctsm

    cd ctsm

    git checkout 112f21c43507a5802014677be7b71bd660987d4c

    cp ../overwrites/Externals.cfg .

    ./manage_externals/checkout_externals

    rsync -rv ../overwrites/ .
fi

source /ctsm-api/docker/entrypoint_setup.sh

sudo -s -E -u "$USER" bash <<EOF

cd /ctsm-api

./scripts/migrations_forward.sh

uvicorn app.main:app --host 0.0.0.0

EOF
