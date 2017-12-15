#!/bin/sh

################################################################################
# Wrapper around the webhooks executable that points it at the correct
# data-dir so it can find its migrations and other necessary files.
set -e

################################################################################
export personal_webhooks_datadir=$(pwd)
dist/build/webhooks/webhooks "$@"
