#!/usr/bin/env bash
source_dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
dist_dir=~

echo "This script makes symbolic links to files in ${source_dir}."
echo "The links will be created in ${dist_dir}."
echo "When a link duplicates with a file in ${dist_dir},"
echo "the file will be renamed by adding a suffix \".bak\"."
echo 
echo -n "Are you sure to continue? [yes/no] "
read answer
if [ "$answer" != "yes" ]; then
    echo "Setup aborted"
    exit;
fi

echo "==== log output ===="
for f in ${source_dir}/.??*
do
    [[ "$( basename "$f")" == ".git" ]] && continue
    [[ "$( basename "$f")" == ".gitignore" ]] && continue
    [[ "$( basename "$f")" == ".DS_Store" ]] && continue

    dist="${dist_dir}/$( basename "$f")"

    if [ -d "$f" ]; then
        if [ -L ${dist} ]; then
            ln -snf -S ".bak" "$f" ${dist}
            echo Existing link ${dist} renamed to ${dist}.bak
        elif [ -d ${dist} ]; then
            mv ${dist} ${dist}.bak
            ln -s "$f" ${dist}
            echo Existing regular directory ${dist} renamed to ${dist}.bak
        else
            ln -s "$f" ${dist}
        fi
        echo Directory symbolic link created: ${dist}/
    fi
    
    if [ -f "$f" ]; then
        if [ -L ${dist} ]; then
            ln -snf -S ".bak" "$f" ${dist}
            echo Existing link ${dist} renamed to ${dist}.bak
        elif [ -f ${dist} ]; then
            mv ${dist} ${dist}.bak
            ln -s "$f" ${dist}
            echo Existing regular file ${dist} renamed to ${dist}.bak
        else
            ln -s "$f" ${dist}
        fi
        echo File symbolic link created: ${dist}
    fi
    
done

# Create symbolic link ~/.profile which points to .bash_profile on linux
if [ "$(uname)" = "Linux" -a -f "${source_dir}/.bash_profile" ]; then
    src="${source_dir}/.bash_profile"
    dist="${dist_dir}/.profile"
    if [ -L ${dist} ]; then
        ln -snf -S ".bak" ${src} ${dist}
        echo Existing link ${dist} renamed to ${dist}.bak
    elif [ -f ${dist} ]; then
        mv ${dist} ${dist}.bak
        ln -s ${src} ${dist}
        echo Existing regular file ${dist} renamed to ${dist}.bak
    else
        ln -s ${src} ${dist}
    fi
    echo File symbolic link created: ${dist}
fi
