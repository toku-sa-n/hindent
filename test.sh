#!/bin/zsh

cabal build > /dev/null; for f in src/**/*.hs
do
    echo -n "$f ..."
    cabal run hindent -- $f > /dev/null
    if ! hindent --validate $f
    then
        hindent < $f | diff --unified=0 $f -
        break
    fi
    echo " OK."
done
