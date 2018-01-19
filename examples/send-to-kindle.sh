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
set -e

################################################################################
TMPDIR=${TMPDIR:-/tmp}
SELF=$(basename "$0")

################################################################################
option_base_url="https://epub.press"
option_kindle_email=""
option_kindle_format="mobi"
option_wait_timeout=120
option_verbose=0

################################################################################
api_base="api/v1"
api_books="$api_base/books"

################################################################################
book_id=""
tempfile=""
temp_files=()
curl_options=("--fail" "--silent")

################################################################################
usage () {
cat <<EOF
Usage: send-to-kindle.sh [options]

  -b URL  Override epub.press base URL
  -e ADDR Send books to the given email address
  -f FMT  Send books in FMT format (mobi|epub) [$option_kindle_format]
  -h      This message
  -t NUM  Wait NUM seconds for the book to be published [$option_wait_timeout]
  -v      Enable verbose output

EOF
}

################################################################################
while getopts "b:e:f:ht:v" o; do
  case "${o}" in
    b) option_base_url=$OPTARG
       ;;

    e) option_kindle_email=$OPTARG
       ;;

    f) option_kindle_format=$OPTARG
       ;;

    h) usage
       exit
       ;;

    t) option_wait_timeout=$OPTARG
       ;;

    v) option_verbose=1
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
cleanup() {
  if [ "${#temp_files[@]}" -gt 0 ]; then
    rm -f "${temp_files[@]}"
  fi
}

################################################################################
safe_mktemp() {
  tempfile=$(mktemp "$TMPDIR/$SELF.XXXXXXXX")
  temp_files+=("$tempfile")
}

################################################################################
create_book() {
  safe_mktemp

  curl "${curl_options[@]}" \
       --output "$tempfile" \
       --data-binary @- \
       --header 'Accept: application/json' \
       --header 'Content-Type: application/json' \
       "$option_base_url/$api_books"

  book_id=$(jq --raw-output .id < "$tempfile")

  if [ -z "$book_id" ] || [ "$book_id" = "null" ]; then
    die "failed to create a book: no book ID in response"
  fi
}

################################################################################
wait_for_book() {
  local progress
  local start_time
  local now

  safe_mktemp
  start_time=$(date +%s)

  while :; do
    now=$(date +%s)

    if [ $((now - start_time)) -gt "$option_wait_timeout" ]; then
      die "timed out waiting for book to be published"
    fi

    curl "${curl_options[@]}" \
         --output "$tempfile" \
         --header 'Accept: application/json' \
         "$option_base_url/$api_books/$book_id/status"

    progress=$(jq --raw-output .progress < "$tempfile")

    if [ "$progress" -ge 100 ]; then
      return
    fi

    cat /dev/null > "$tempfile"
    sleep 3
  done
}

################################################################################
send_email() {
  safe_mktemp

  curl "${curl_options[@]}" \
       --output "$tempfile" \
       --header 'Accept: application/json' \
       --data-urlencode "email=$option_kindle_email" \
       --data-urlencode "filetype=$option_kindle_format" \
       --get "$option_base_url/$api_books/$book_id/email"

  cat "$tempfile"
  echo
}

################################################################################
if [ -z "$option_kindle_email" ]; then
  die "please provide an email address with -e"
fi

trap cleanup EXIT

if [ "$option_verbose" = 1 ]; then
  curl_options+=("--verbose")
  set -x
fi

create_book
wait_for_book
send_email
