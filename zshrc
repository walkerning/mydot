# foxfi's env
export PS1="%{$fg[yellow]%}%n @ %m%{$reset_color%} $PS1"
export GLOBAL_VENV_DIR="/home/foxfi/venvs/"
export EDITOR="emacsclient -c"

export LC_CTYPE=en_US.UTF-8
export LC_ALL=en_US.UTF-8

# foxfi's alias
alias szsh="source /home/foxfi/.zshrc"
alias e="emacsclient -c"

# foxfi's functions
function fvenv() {
    ACTIVE_SUFFIX="bin/activate"
    if [[ "$#" ==  0 ]]; then
        if [[ -f "./venv/$ACTIVE_SUFFIX" ]]; then
            source ./venv/${ACTIVE_SUFFIX}
        elif [[ -f "./VENV/$ACTIVE_SUFFIX" ]]; then
            source ./VENV/${ACTIVE_SUFFIX}
        else
            echo "no venv directory found in the current directory"
        fi
    elif [[ "$#" == 1 ]]; then
        if [[ -f "./$1/${ACTIVE_SUFFIX}" ]]; then
            source "./$1/${ACTIVE_SUFFIX}"
        elif [[ -n "$GLOBAL_VENV_DIR" ]]; then
            if [[ -f "${GLOBAL_VENV_DIR}/${1}/${ACTIVE_SUFFIX}" ]]; then
		source "${GLOBAL_VENV_DIR}/${1}/${ACTIVE_SUFFIX}"
            else
		echo "no directory named < ${1} > found under ${GLOBAL_VENV_DIR}"
	    fi
        else
            echo 'no $GLOBAL_VENV_DIR variable set'
        fi
    else
        echo "only recv 0 or 1 parameter: the venv dirname(default venv/VENV)"
    fi

}

