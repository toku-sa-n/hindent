#!/bin/bash

readonly INPUT=$(cat -)

readonly EXPECTED=$(echo $INPUT|hindent)
readonly ACTUAL=$(echo $INPUT|stack run --silent)

echo "Executing ..."

diff <(echo "$EXPECTED") <(echo "$ACTUAL")
