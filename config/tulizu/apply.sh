#!/usr/bin/env bash
set -euo pipefail
IFS=$'\n\t'

sudo tulizu make "./fedora.tizu"
sudo tulizu install
