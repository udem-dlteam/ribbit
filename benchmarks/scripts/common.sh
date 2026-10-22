# This file contains common procedures for the scripts

if [ "$__common_file_imported" != "1" ]; then

# parse_args NAME=VALUE... -- NAME=VALUE...
#   * NAME=VALUE (default)  → optional, with fallback
#   * NAME=?    (default)   → required: must be given after the --
#   * everything after -- must be a declared NAME=VALUE
parse_args () {
    _known=
    _required=
    _seen=
    while [ $# -gt 0 ] && [ "$1" != -- ]; do          # phase 1: schema + defaults
        case $1 in
            [A-Za-z_]*=*) ;;
            *) printf 'parse_args: bad default: %s\n' "$1" >&2; exit 2 ;;
        esac
        case ${1%%=*} in
            *[!A-Za-z0-9_]*) printf 'parse_args: bad name: %s\n' "$1" >&2; exit 2 ;;
        esac
        case ${1#*=} in
            '?') _required="$_required ${1%%=*} " ;;
            *)    eval "${1%%=*}=\${1#*=}" ;;
        esac
        _known="$_known ${1%%=*} "
        shift
    done
    [ $# -gt 0 ] || { printf 'parse_args: missing --\n' >&2; exit 2; }
    shift
    for _arg in "$@"; do                              # phase 2: user overrides
        case $_arg in
            [A-Za-z_]*=*) ;;
            *) printf 'parse_args: invalid argument: %s\n' "$_arg" >&2; exit 2 ;;
        esac
        case ${_arg#*=} in
            '?') printf 'parse_args: bad value: %s\n' "$_arg" >&2; exit 2 ;;
        esac
        case $_known in
            *" ${_arg%%=*} "*) eval "${_arg%%=*}=\${_arg#*=}"
                               _seen="$_seen ${_arg%%=*} " ;;
            *) printf 'parse_args: unknown option: %s\n' "$_arg" >&2; exit 2 ;;
        esac
    done
    for _name in $_required; do                       # phase 3: required given?
        case $_seen in
            *" $_name "*) ;;
            *) printf 'parse_args: missing required option: %s=<option>\n' "$_name" >&2; exit 2 ;;
        esac
    done
    unset _known _required _seen _arg _name
}


__common_file_imported=1

fi
