#!/bin/zsh

cabal build > /dev/null; for f in src/**/*.hs
do
    echo -n "$f ..."
    hindent -- $f > /dev/null
    if ! cabal run hindent -- --validate $f
    then
        cabal run hindent < $f | diff --unified=0 $f -
        break
    fi
    echo " OK."
done
