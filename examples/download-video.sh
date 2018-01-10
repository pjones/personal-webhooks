#!/bin/bash

################################################################################
#
# This file is part of the package personal-webhooks. It is subject to
# the license terms in the LICENSE file found in the top-level directory
# of this distribution and at:
#
#   git://git.devalot.com/personal-webhooks.git
#
# No part of this package, including this file, may be copied, modified,
# propagated, or distributed except according to the terms contained in
# the LICENSE file.
#
################################################################################
#
# Download a video using `youtube-dl'.
#
# Request JSON files are expected to have a `url' property containing
# a URL that can be given to `youtube-dl'.
#
################################################################################
set -e
option_download_dir="$HOME/download"

################################################################################
ytopts=("--no-progress")

################################################################################
usage () {
cat <<EOF
Usage: download-video.sh [options]

  -d DIR  The directory to save video files in
  -h      This message

EOF
}

################################################################################
while getopts "d:h" o; do
  case "${o}" in
    d) option_download_dir=$OPTARG
       ;;

    h) usage
       exit
       ;;

    *) exit 1
       ;;
  esac
done

shift $((OPTIND-1))

################################################################################
url=$(jq --raw-output ".url")

if [ -z "$url" ]; then
  >&2 echo "ERROR: JSON file does not have a 'url' property"
  exit 1
fi

mkdir -p "$option_download_dir"
cd "$option_download_dir"

youtube-dl "${ytopts[@]}" "$url"
