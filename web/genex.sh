#! /bin/sh
# Generate the examples.ml file
(echo 'let list = [';
find ../examples -name "*.scr" -printf "%P\n" | while read file; do
    echo -n "\"$file\","
    echo -n '"'
    cat ../examples/$file | sed 's/"/\\"/g'
    echo '";';
done;
echo ']') > examples.ml
