#! /bin/sh
# Generate the examples.ml file
EXAMPLE_DIR="../../../examples"
(echo 'let list = [';
find $EXAMPLE_DIR -name "*.scr" -printf "%P\n" | while read file; do
    echo -n "\"$file\","
    echo -n '"'
    cat $EXAMPLE_DIR/$file | sed 's/"/\\"/g'
    echo '";';
done;
echo ']') > examples.ml
