#!/usr/bin/env bash

set -e

if [[ ! -d /ctsm-api/resources/model ]]; then
    cd /ctsm-api/resources/

    git clone https://github.com/NorESMhub/NorESM model

    cd model

    git checkout a5e48a22879b4b324b12776a3cb0f087a21819d9

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
