#!/bin/bash

readonly INPUT=$(cat -)

readonly EXPECTED=$(echo $INPUT|hindent)
readonly ACTUAL=$(echo $INPUT|stack run --silent)

diff <(echo "$EXPECTED") <(echo "$ACTUAL")
