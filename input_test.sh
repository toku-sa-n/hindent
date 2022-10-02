#!/bin/zsh

readonly INPUT=$(read -se)

readonly EXPECTED=$(mktemp --suffix=.expected.hs)
readonly ACTUAL=$(mktemp --suffix=.actual.hs)

echo $INPUT > $EXPECTED
echo $INPUT > $ACTUAL

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

diff $EXPECTED $ACTUAL
