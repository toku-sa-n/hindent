#!/bin/zsh

for f in src/**/*.hs
do
    ACTUAL=$(mktemp --suffix=.actual.hs)
    cp $f $ACTUAL
    echo -n "$f ..."

    if ! cabal run hindent -- $ACTUAL > /dev/null
    then
        result=$(cabal run hindent -- $ACTUAL 2>&1 1>/dev/null|grep 'undefined')
        if [ ! -z "$result" ]
        then
            echo "Failed to format $f."
            exit 1
        fi
    fi

    echo " OK."
done
