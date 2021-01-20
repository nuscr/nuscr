#! /bin/bash
set -euo pipefail
# Generate the examples.ml file
EXAMPLE_DIR="../../../"
(echo 'let list = [';
cat ../../../web/examplelist.txt | while read file; do
    echo -n "\"$file\","
    echo -n '"'
    cat $EXAMPLE_DIR/$file | sed 's/"/\\"/g'
    echo '";';
done;
echo ']') > examples.ml
