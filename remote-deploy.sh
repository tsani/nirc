#!/bin/bash

set -e

ssh j 'ssh nirc@znc-in-a-box bash' <<EOF
set -e

cd nirc
git fetch

git merge --ff-only
stack install

sudo systemctl restart nirc
sudo systemctl status nirc
EOF
