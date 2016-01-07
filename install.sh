#!/usr/bin/env bash

main() {
    # Use colors, but only if connected to a terminal, and that terminal
    # supports them.
    TMP_DIR='/tmp/mydot'
    INIT_SCRIPT='.dotfiles.py'
    GIT_REPO="${MYDOT_GIT_REPO:-https://github.com/walkerning/mydot}"

    if which tput >/dev/null 2>&1; then
        ncolors=$(tput colors)
    fi
    if [ -t 1 ] && [ -n "$ncolors" ] && [ "$ncolors" -ge 8 ]; then
        RED="$(tput setaf 1)"
        GREEN="$(tput setaf 2)"
        BLUE="$(tput setaf 4)"
        BOLD="$(tput bold)"
        NORMAL="$(tput sgr0)"
    else
        RED=""
        GREEN=""
        BLUE=""
        BOLD=""
        NORMAL=""
    fi

    # Only enable exit-on-error after the non-critical colorization stuff,
    # which may fail on systems lacking tput or terminfo
    set -e

    printf "${BLUE}Cloning ...${NORMAL}\n"
    hash git >/dev/null 2>&1 || {
        echo "Error: git is not installed"
        exit 1
    }
    env git clone ${GIT_REPO} ${TMP_DIR} || {
        printf "${RED}Error: git clone repo failed\n${NORMAL}"
        exit 1
    }

    pushd ${TMP_DIR} > /dev/null 2>&1

    env python ${INIT_SCRIPT} || {
        printf "${RED}运行 ${INIT_SCRIPT} 遇到错误${NORMAL}"
        clean
        exit 1
    }

    printf "${GREEN}所有配置成功${NORMAL}"
    clean
}

clean() {
    popd
    rm -rf ${TMP_DIR}
}
main
