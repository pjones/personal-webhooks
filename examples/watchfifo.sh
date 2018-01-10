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
# Execute a command for each line read from a fifo.
#
# For example, to use in conjunction with the download-video.sh script:
#
# watchfifo.sh -f /tmp/download.fifo -- download-video.sh -d ~/Downloads
#
# Lines read from the fifo are piped into the given command's stdin.
#
################################################################################
set -e
option_fifo_file=""

################################################################################
usage () {
cat <<EOF
Usage: watchfifo.sh [options] -- command [arg, arg, ...]

  -f FILE The fifo file to create and manage
  -h      This message

EOF
}

################################################################################
while getopts "f:h" o; do
  case "${o}" in
    f) option_fifo_file=$OPTARG
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
die() {
  >&2 echo "ERROR: " "$@"
  exit 1
}

################################################################################
prepare() {
  if [ -z "$option_fifo_file" ]; then
    die "you must use -f to specify a file path"
  fi

  if [ -r "$option_fifo_file" ]; then
    die "fifo file exists, remove it first: $option_fifo_file"
  fi

  mkfifo -m 0622 "$option_fifo_file"
}

################################################################################
cleanup() {
  rm -f "$option_fifo_file"
}

################################################################################
export IFS=$'\n'
trap cleanup EXIT
prepare

while :; do
  read -r json < "$option_fifo_file"

  if ! echo "$json" | "$@"; then
    >&2 echo "ERROR: child process failed"
  fi
done
