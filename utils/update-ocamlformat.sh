#!/bin/bash

if [ -z "$1" ]
then
  echo "Usage: $(basename "$0") VERSION"
  exit 1
fi

INSTALLED_VERSION=$(opam list --installed --columns=version --quiet --short ocamlformat)

if [ "$1" != "$INSTALLED_VERSION" ]
then
  echo "Warning: You have version $INSTALLED_VERSION on your machine"
fi

sed -i "s/version = .*\$/version = $1/" .ocamlformat
sed -i "s/ocamlformat=.*\$/ocamlformat=$1/" .github/workflows/main.yml
