#!/usr/bin/env bash
cdir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )

for f in ${cdir}/.??*
do
    [[ "$( basename "$f")" == ".git" ]] && continue
    [[ "$( basename "$f")" == ".gitignore" ]] && continue
    [[ "$( basename "$f")" == ".DS_Store" ]] && continue

    if [ -d "$f" ]; then
        ln -sfn "$f" ~/"$( basename "$f")"
        echo Directory symbolic link created: ~/"$( basename "$f")"/
    fi
    
    if [ -f "$f" ]; then
        ln -sf "$f" ~/"$( basename "$f")"
        echo File symbolic link created: ~/"$( basename "$f")"
    fi
    
done

