#!/bin/bash

# Required parameters:
# @raycast.schemaVersion 1
# @raycast.title Open terminal
# @raycast.mode silent
# @raycast.packageName Navigation

# Optional parameters:
# @raycast.icon images/terminal-icon.png

# Documentation:
# @raycast.description Opens new terminal window.

wezterm cli spawn --new-window
