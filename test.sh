#!/bin/zsh

for f in src/**/*.hs
do
    TMPFILE=$(mktemp --suffix=.hs)
    cp $f $TMPFILE
    echo -n "$f ..."
    hindent -- $TMPFILE > /dev/null
    if ! (cabal build && cabal run hindent -- --validate $TMPFILE) > /dev/null
    then
        echo "$f is not formatted."
        cabal run hindent < $TMPFILE | diff --unified=0 $TMPFILE -
        break
    fi
    echo " OK."
done
