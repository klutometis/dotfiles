#!/usr/bin/env bash

cd ~/build/qtile-env/
compton -b --config ~/.config/compton.conf
poetry run qtile start
