#! /bin/bash
# Generate the examples.ml file
EXAMPLE_DIR=$1
EXAMPLE_DIR=${EXAMPLE_DIR:="../"}
(echo 'let list = [';
cat examplelist.txt | while read file; do
    echo -n "\"$file\","
    echo -n '"'
    cat $EXAMPLE_DIR/$file | sed 's/"/\\"/g'
    echo '";';
done;
echo ']') > examples.ml
