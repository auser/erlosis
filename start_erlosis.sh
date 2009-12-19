#!/bin/sh

VERSION=$(cat VERSION | tr -d '\n')
PWD=$(dirname $0)
CONFIG=$1

make

if [[ ! -f ebin/erlosis*.boot ]]; then
	make boot
fi

erl -pa $PWD/ebin \
    -s reloader \
    -erlosis config_file "\"$CONFIG\"" \
    -boot erlosis-$VERSION