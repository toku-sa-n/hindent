#!/bin/zsh

for f in src/**/*.hs
do
    TMPFILE=$(mktemp --suffix=.hs)
    cp $f $TMPFILE
    echo -n "$f ..."
    hindent -- $TMPFILE > /dev/null
    if ! (cabal build > /dev/null && cabal run hindent -- --validate $TMPFILE)
    then
        cabal run hindent < $TMPFILE | diff --unified=0 $TMPFILE -
        break
    fi
    echo " OK."
done
