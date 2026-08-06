#!/bin/sh
# Open a new Ghostty tab at the given path (its parent directory if it is a file).
#
# Ghostty has no CLI equivalent of `wezterm cli spawn` on macOS
# (`ghostty +new-window` reports "not supported on this platform"), so this
# drives the AppleScript dictionary that Ghostty 1.3+ ships instead.
set -e

target=${1:?usage: ghostty-new-tab.sh PATH}
[ -d "$target" ] || target=$(dirname -- "$target")
dir=$(cd -- "$target" && pwd -P)

osascript - "$dir" <<'APPLESCRIPT' >/dev/null
on run argv
  tell application "Ghostty"
    set cfg to new surface configuration
    set initial working directory of cfg to (item 1 of argv)
    if (count of windows) > 0 then
      new tab in front window with configuration cfg
    else
      new window with configuration cfg
    end if
  end tell
end run
APPLESCRIPT
