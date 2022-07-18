#!/bin/bash

set -euo pipefail

function check_installed() {
  if [ -z "$(which "$1")" ]
  then
    echo "$1" is not uninstalled
    [ -z "$2" ] && echo "$2"
    exit 1
  fi
}

check_installed serve "You can install serve with npm or yarn"

BASE_DIR=$(dirname "$(realpath "$0")")/..
OUT_DIR=$BASE_DIR/_site
WEB_DIR=$BASE_DIR/web
WHERE="$OUT_DIR" make -C "$WEB_DIR" deploy
serve "$OUT_DIR"
