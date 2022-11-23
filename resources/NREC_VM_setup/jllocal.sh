#!/usr/bin/bash

# Get user and host from shell (port is optional)
user=$1
host=$2
port=${3:-8899}

# Open URL
url=http://localhost:$port/lab?
xdg-open $url
echo "Connect to "$url

# SSH to remote Jupyter Lab
ssh -CNL localhost:$port:localhost:$port $user@$host
