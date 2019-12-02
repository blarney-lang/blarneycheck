#!/bin/bash

SCRIPTPATH="$( cd "$(dirname "$0")" ; pwd -P )"
File=~/.bashrc
if ! grep -q "[ -f \"$SCRIPTPATH/blarney_env\" ] && source \"$SCRIPTPATH/blarney_env\"" "$File"; then
  [ -f "$SCRIPTPATH/blarney_env" ] && echo "[ -f \"$SCRIPTPATH/blarney_env\" ] && source \"$SCRIPTPATH/blarney_env\"" >> ~/.bashrc
fi
