#!/bin/zsh

for f in src/**/*.hs
do
    echo -n "$f ..."
    hindent -- $f > /dev/null
    if ! cabal build > /dev/null && cabal run hindent -- --validate $f
    then
        cabal run hindent < $f | diff --unified=0 $f -
        break
    fi
    echo " OK."
done
