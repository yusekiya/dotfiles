#!/usr/bin/env bash
source_dir=$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )
dest_dir=~
ignore_list=(".git" ".gitignore" ".DS_Store")

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


make_link_safely() {
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

    echo "==== log output ===="
    for f in ${source_dir}/.??*
    do
        flag_ignore=false
        for ignore_file in "${ignore_list[@]}"
        do
            if [ "$( basename "$f")" = $ignore_file ]; then
                flag_ignore=true
                continue
            fi
        done
        if $flag_ignore; then
            continue
        fi
        
        dest="${dest_dir}/$( basename "$f")"

        if [ -d "$f" ]; then
            if [ -L ${dest} ]; then
                mv ${dest} ${dest}.bak
                ln -s "$f" ${dest}
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
                mv ${dest} ${dest}.bak
                ln -s "$f" ${dest}
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
            mv ${dest} ${dest}.bak
            ln -s ${src} ${dest}
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
}


make_link_forcibly() {
    echo "==== log output ===="
    for f in ${source_dir}/.??*
    do
        flag_ignore=false
        for ignore_file in "${ignore_list[@]}"
        do
            if [ "$( basename "$f")" = $ignore_file ]; then
                flag_ignore=true
                continue
            fi
        done
        if $flag_ignore; then
            continue
        fi

        dest="${dest_dir}/$( basename "$f")"

        if [ -d "$f" ]; then
            if [ -L ${dest} ]; then
                ln -snf "$f" ${dest}
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
                ln -snf "$f" ${dest}
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
            ln -snf ${src} ${dest}
        elif [ -f ${dest} ]; then
            mv ${dest} ${dest}.bak
            ln -s ${src} ${dest}
            echo Existing regular file ${dest} renamed to ${dest}.bak
        else
            ln -s ${src} ${dest}
        fi
        echo File symbolic link created: ${dest}
    fi
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

if $flag_force; then
    echo forced
else
    make_link_safely
fi
