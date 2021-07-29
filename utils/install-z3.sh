#!/bin/bash

set -euo pipefail

case "$(uname)" in
  Darwin)
    Z3_FILENAME=z3-4.8.10-x64-osx-10.15.7
    ;;
  Linux)
    # Assume Debian
    Z3_FILENAME=z3-4.8.10-x64-ubuntu-18.04
    ;;
  *)
    echo "Don't know which z3 to download"
    exit 1
    ;;
esac
curl -o z3.zip -L https://github.com/Z3Prover/z3/releases/download/z3-4.8.10/${Z3_FILENAME}.zip
unzip z3.zip
rm z3.zip
mv ${Z3_FILENAME} z3
sudo mv z3/bin/* /usr/local/bin
sudo mv z3/include/* /usr/local/include
rm -rf z3

