#!/bin/bash

VOLUME=(`pactl list sinks | awk '/Volume: front-left:/ {print substr($5, 1, index($5, "%") - 1)}'`)

echo "${VOLUME[1]}%"
