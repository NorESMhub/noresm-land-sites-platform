#!/usr/bin/env bash

set -e

cd /ctsm-api/resources/

rm -rf ctsm

git clone https://github.com/NorESMhub/NorESM ctsm

cd ctsm

git checkout 6022f0e07ca4cf66a416f1007e77eed5d634d64e

cp -r ../overwrites/manage_externals .

./manage_externals/checkout_externals

rsync -rv ../overwrites/ .

source /ctsm-api/docker/entrypoint_setup.sh

sudo -s -E -u "$USER" bash <<EOF

cd /ctsm-api

./scripts/migrations_forward.sh

if [[ ${DEBUG:-0} == 1 ]]; then
  uvicorn app.main:app --reload --host 0.0.0.0
else
  uvicorn app.main:app --host 0.0.0.0
fi

EOF
