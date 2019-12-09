#!/bin/bash

AUTHORS=$(git shortlog -s | cut -f 2 | sed 's/^\(.*\)$/\"\1\"/' | paste -s -d ' ')

sed -i "s/^(authors .*)$/(authors $AUTHORS)/" dune-project
