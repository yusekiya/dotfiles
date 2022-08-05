#!/usr/bin/env bash
source_dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
dest_dir=~
ignore_list=(".." "." ".git" ".gitignore" ".DS_Store")

usage_exit() {
        echo "Usage: $( basename $0) [-h] [-f]" 1>&2
        echo
        echo "Options:"
        echo "  -h  show this help"
        echo "  -f  do not prompt before creating links in ${dest_dir}"
        exit 1
}

# Windows needs administrator privilege to make symbolic link
if [ "$(uname -o)" = "Msys" -a -z "$(id -Gn | grep Administrators)" ]; then
    echo "Error: Permission denied"
    echo "Run this script as administrator"
    exit;
fi

is_excluded() {
    for ignore_file in "${ignore_list[@]}"
    do
        if [ "$( basename "$1")" = $ignore_file ]; then
            return 0
        fi
    done
    return 1
}

make_link() {
    for f in "${@:2}"
    do
        if is_excluded "$f"; then
            continue
        fi
        
        dest="$1/$( basename "$f")"

        if [ -d "$f" ]; then
            if [ -L ${dest} ]; then
                if $flag_force; then
                    ln -snf "$f" ${dest}
                else
                    mv ${dest} ${dest}.bak
                    ln -s "$f" ${dest}
                    echo Existing link ${dest} renamed to ${dest}.bak
                fi
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
                if $flag_force; then
                    ln -snf "$f" ${dest}
                else
                    mv ${dest} ${dest}.bak
                    ln -s "$f" ${dest}
                    echo Existing link ${dest} renamed to ${dest}.bak
                fi
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
}


flag_force=false
while getopts :hf OPT
do
    case $OPT in
        h)  usage_exit
            ;;
        f)  flag_force=true
            ;;
        \?) usage_exit
            ;;
    esac
done

if ! $flag_force; then
    echo "This script makes symbolic links pointing to files in ${source_dir}."
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
fi

echo "==== log output ===="
make_link "${dest_dir}" ${source_dir}/.*
