#!/usr/bin/env bash
set -eu

source_dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
dest_dir=~
config_dir="${dest_dir}"/.config
ignore_list=(".." "." ".git" ".gitignore" ".DS_Store")

usage_exit() {
        echo "Usage: $( basename $0) [options]" 1>&2
        echo
        echo "Options:"
        echo "  -h            Show this help"
        echo "  -f            Do not prompt before creating links in ${dest_dir}"
        echo "  -e x1,x2,..   Exclude files/directories separated by comma"
        echo "  -q            Quiet mode"
        exit 1
}

# Windows needs administrator privilege to make symbolic link
if [ "$(uname -o)" = "Msys" -a -z "$(id -Gn | grep Administrators)" ]; then
    echo "Error: Permission denied"
    echo "Run this script as administrator"
    exit;
fi

is_excluded() {
    # Check if a target is ignored by git
    if [ -n "$(git check-ignore $1)" ]; then
        return 0
    fi
    # Check if base name of a target is included in the ignore list
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
                    if [ $verbose -ge 1 ]; then
                        echo Existing link ${dest} renamed to ${dest}.bak
                    fi
                fi
            elif [ -d ${dest} ]; then
                mv ${dest} ${dest}.bak
                ln -s "$f" ${dest}
                if [ $verbose -ge 1 ]; then
                    echo Existing regular directory ${dest} renamed to ${dest}.bak
                fi
            else
                ln -s "$f" ${dest}
            fi
            if [ $verbose -ge 1 ]; then
                echo Directory symbolic link created: ${dest}/
            fi
        fi

        if [ -f "$f" ]; then
            if [ -L ${dest} ]; then
                if $flag_force; then
                    ln -snf "$f" ${dest}
                else
                    mv ${dest} ${dest}.bak
                    ln -s "$f" ${dest}
                    if [ $verbose -ge 1 ]; then
                        echo Existing link ${dest} renamed to ${dest}.bak
                    fi
                fi
            elif [ -f ${dest} ]; then
                mv ${dest} ${dest}.bak
                ln -s "$f" ${dest}
                if [ $verbose -ge 1 ]; then
                    echo Existing regular file ${dest} renamed to ${dest}.bak
                fi
            else
                ln -s "$f" ${dest}
            fi
            if [ $verbose -ge 1 ]; then
                echo File symbolic link created: ${dest}
            fi
        fi
    done
}


flag_force=false
verbose=1
while getopts e:hfaq OPT
do
    case $OPT in
        h)  usage_exit
            ;;
        f)  flag_force=true
            ;;
        e)  excludes=(${OPTARG//,/ })
            ignore_list+=(${excludes[@]})
            ;;
        q)  verbose=0
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

if [ $verbose -ge 1 ]; then
    echo "==== log output ===="
fi
make_link "${dest_dir}" ${source_dir}/.*

if [ ! -d "${config_dir}" ]; then
    mkdir "${config_dir}"
fi

make_link "${config_dir}" ${source_dir}/config/*
