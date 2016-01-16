#!/usr/bin/env bash
source_dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
dest_dir=~

echo "This script makes symbolic links to files in ${source_dir}."
echo "The links will be created in ${dest_dir}."
echo "When a link duplicates with a file in ${dest_dir},"
echo "the file will be renamed by adding a suffix \".bak\"."
echo 
echo -n "Are you sure to continue? [yes/no] "
read answer
if [ "$answer" != "yes" ]; then
    echo "Setup aborted"
    exit;
fi

# Windows needs administrator privilege to make symbolic link
if [ "$(uname -o)" = "Msys" -a -z "$(id -Gn | grep Administrators)" ]; then
    echo "Error: Permission denied"
    echo "Run this script as administrator"
    exit;
fi

echo "==== log output ===="
for f in ${source_dir}/.??*
do
    [[ "$( basename "$f")" == ".git" ]] && continue
    [[ "$( basename "$f")" == ".gitignore" ]] && continue
    [[ "$( basename "$f")" == ".DS_Store" ]] && continue

    dest="${dest_dir}/$( basename "$f")"

    if [ -d "$f" ]; then
        if [ -L ${dest} ]; then
            ln -snf -S ".bak" "$f" ${dest}
            echo Existing link ${dest} renamed to ${dest}.bak
        elif [ -d ${dest} ]; then
            mv ${dest} ${dest}.bak
            ln -s "$f" ${dest}
            echo Existing regular directory ${dest} renamed to ${dest}.bak
        else
            ln -s "$f" ${dest}
        fi
        echo Directory symbolic link created: ${dest}/
    fi
    
    if [ -f "$f" ]; then
        if [ -L ${dest} ]; then
            ln -snf -S ".bak" "$f" ${dest}
            echo Existing link ${dest} renamed to ${dest}.bak
        elif [ -f ${dest} ]; then
            mv ${dest} ${dest}.bak
            ln -s "$f" ${dest}
            echo Existing regular file ${dest} renamed to ${dest}.bak
        else
            ln -s "$f" ${dest}
        fi
        echo File symbolic link created: ${dest}
    fi
    
done

# Create symbolic link ~/.profile which points to .bash_profile on linux
if [ "$(uname)" = "Linux" -a -f "${source_dir}/.bash_profile" ]; then
    src="${source_dir}/.bash_profile"
    dest="${dest_dir}/.profile"
    if [ -L ${dest} ]; then
        ln -snf -S ".bak" ${src} ${dest}
        echo Existing link ${dest} renamed to ${dest}.bak
    elif [ -f ${dest} ]; then
        mv ${dest} ${dest}.bak
        ln -s ${src} ${dest}
        echo Existing regular file ${dest} renamed to ${dest}.bak
    else
        ln -s ${src} ${dest}
    fi
    echo File symbolic link created: ${dest}
fi
