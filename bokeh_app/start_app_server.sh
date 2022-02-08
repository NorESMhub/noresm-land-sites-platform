#!/usr/bin/env bash

if [[ $(fuser 5006/tcp | wc -c) -ne 0 ]]; then
    echo "Server already running, shutting it down..."
    fuser -k 5006/tcp
fi

python3 -m bokeh serve --show make_settings & ssh -NfR 5006:localhost:5006 centos@158.39.201.200
