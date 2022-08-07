#!/bin/bash

MINIFY="${MINIFY:-yarn run -s minify}"
FILES=$(find "$1" -regex ".*\.\(html\|js\|css\)")

for f in $FILES
do
    echo Minifying "$f"
    $MINIFY "$f" > "$f.min"
    mv "$f.min" "$f"
done

