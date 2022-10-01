#!/bin/zsh

for f in **/*.hs
do
    EXPECTED=$(mktemp --suffix=.expected.hs)
    ACTUAL=$(mktemp --suffix=.actual.hs)
    cp $f $EXPECTED
    cp $f $ACTUAL
    echo -n "$f ..."

    if ! hindent -- $EXPECTED > /dev/null
    then
        echo "The original HIndent failed to format $f."
        break
    fi

    if ! cabal run hindent -- $ACTUAL > /dev/null
    then
        echo "Failed to format $f."
        break
    fi

    if ! diff $EXPECTED $ACTUAL > /dev/null
    then
        echo "$f is not formatted."
        vimdiff $EXPECTED $ACTUAL
        break
    fi
    echo " OK."
done
