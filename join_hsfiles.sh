#!/bin/sh

set -e

input_dir="$1"

if [ -z "$input_dir" ]; then
    echo "Usage: join-hsfiles INPUT_DIR"
    exit 1;
fi

name=$(echo $input_dir | sed -e 's|/$||' -e 's|.*/\(.*\)|\1|')
template_file=$name.hsfiles

test -f $template_file && rm $template_file

find "$input_dir" -type f | sed -e '/\/stack.yaml/d' \
				-e '/\.cabal/d' \
				-e '/\/\.stack-work\//d' \
				-e '/\/yesod-devel\//d' \
				-e '/\/client_session_key.aes/d' | LC_ALL=C sort | while read input_file; do
    filename="${input_file#*$name/}" # with everything, up to and including first $name stripped out
    if file $input_file | grep -q \
			       -e 'empty$' \
			       -e 'ASCII text' \
			       -e 'Unicode text' \
			       -e 'Scalable Vector Graphics image$';
    then
	echo "{-# START_FILE $filename #-}" >> "$template_file"
	sed "s/$name/{{name}}/g" "$input_file" >> "$template_file"
    else
	echo "{-# START_FILE BASE64 $filename #-}" >> "$template_file"
	base64 "$input_file" >> "$template_file"
    fi
    echo >> "$template_file"
done
