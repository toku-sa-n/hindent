#!/bin/zsh

readonly EXPECTED=$(mktemp --suffix=.expected.hs)
readonly ACTUAL=$(mktemp --suffix=.actual.hs)

cat $1 > $EXPECTED
cat $1 > $ACTUAL

if ! hindent $EXPECTED
then
    echo "The original HIndent failed to format the code."
    exit 1
fi

if ! stack run --silent $ACTUAL
then
    echo "The development version of HIndent failed to format the code."
    exit 1
fi

cat $EXPECTED
cat $ACTUAL

diff $EXPECTED $ACTUAL
