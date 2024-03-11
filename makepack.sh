#!/bin/bash

if [ $# -lt 1 ]; then
  echo "Usage: $0 version"
  exit 1
fi

PACK_VERSION="${@:1}"

PACK_DIR="./pack/traffic_rules"
mkdir -p $PACK_DIR
cp pack.pl $PACK_DIR

PACK_CODE_DIR="./pack/traffic_rules/prolog/"

mkdir -p $PACK_CODE_DIR

cp traffic_rules.pl traffic_rules-prolog.pl netlogo_glue.pl $PACK_CODE_DIR

cd ./pack

zip -r traffic_rules-$PACK_VERSION.zip traffic_rules

# to be used inside Prolog with ?- pack_install('logicalenglish-<...>.zip').s