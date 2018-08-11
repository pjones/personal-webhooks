#! /usr/bin/env nix-shell
#! nix-shell -i bash -p bash coreutils curl gnused
# shellcheck shell=bash

################################################################################
set -e
set -u

################################################################################
top="$(realpath "$(dirname "$0")"/..)"

################################################################################
port=8080

################################################################################
# Number of times to run the created hook.
runs=${WEBHOOKS_REQUESTS:-10000}

################################################################################
# Which binary to use:
bin=$top/dist/build/webhooks/webhooks

################################################################################
payload=$top/examples/epub.json

################################################################################
export personal_webhooks_datadir="$top"

################################################################################
# Step 1: Create a new webhook:
code=$($bin create --append /dev/null | cut -d: -f2 | sed 's/ //g')

################################################################################
# Step 2: Start the server:
$bin server --port "$port" +RTS "$@" &
pid=$!
sleep 2 # Give it a chance to start

################################################################################
# Step 3: Issue requests:
for _ in $(seq 1 "$runs"); do

  curl \
    --silent --output /dev/null \
    --data-binary @"$payload" \
    -H "Content-Type: application/json" \
    http://localhost:"$port"/hooks/"$code"
done

################################################################################
# Step 4: Kill the server:
kill -INT "$pid"

# Local Variables:
#   mode: sh
#   sh-shell: bash
# End:
