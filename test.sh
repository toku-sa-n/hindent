#!/bin/zsh

for f in src/**/*.hs
do
    EXPECTED=$(mktemp --suffix=.expected.hs)
    ACTUAL=$(mktemp --suffix=.actual.hs)
    cp $f $EXPECTED
    cp $f $ACTUAL
    echo -n "$f ..."
    hindent -- $EXPECTED > /dev/null
    cabal run hindent -- $ACTUAL > /dev/null
    if ! diff $EXPECTED $ACTUAL > /dev/null
    then
        echo "$f is not formatted."
        vimdiff $EXPECTED $ACTUAL
        break
    fi
    echo " OK."
done
