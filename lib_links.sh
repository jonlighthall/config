#!/bin/bash -u
# -----------------------------------------------------------------------------------------------
# LINKS LIBRARY
# -----------------------------------------------------------------------------------------------
#
# ~/config/lib_links.sh
#
# PURPOSE: functions for making links in bash shell.
#
# CONTAINS:
#   check_arg1()
#   check_arg2()
#   check_target()
#   do_link()
#   do_link_exe()
#   do_make_dir()
#   do_make_link()
#
# Mar 2024 JCL
#
# -----------------------------------------------------------------------------------------------

function check_arg1() {
    ddecho -n "$FUNCNAME: number of arguments = $# "
    # check number of arguments
    if [ $# -lt 1 ]; then
        ddecho -e "${BAD}FAIL${RESET}"
        echo "$FUNCNAME: One argument is required"
        echo "$FUNCNAME: Please provide a target name"
        echo "$FUNCNAME: $FUNCNAME TARGET"
        return 1
    else
        ddecho -e "${GOOD}OK${RESET}"
    fi
}

function check_arg2() {
    # check number of arguments
    if [ $# -lt 2 ]; then
        echo "number of arguments = $#"
        echo "Two arguments are required"
        echo "Please provide a target and a link name"
        echo "$FUNCNAME TARGET LINK_NAME"
        echo "where TARGET is the source file and LINK_NAME is the destination file"
        return 1
    fi
}

function check_target() {
    check_arg1 $@
    local funcDEBUG=${DEBUG:-0}
    # define target (source)
    local target="$@"
    local target_canon=$(readlink -f "${target}")
    if [ $funcDEBUG -gt 0 ]; then
        start_new_line
        fecho "input: ${target}"
        fecho " path: ${target_canon}"
    fi

    # get the cursor position
    echo -en "\E[6n"
    read -sdR CURPOS
    local CURPOS=${CURPOS#*[}
          #}# dummy bracket for Emacs indenting
    # get the x-position of the cursor
    local -i x_pos=${CURPOS#*;}
    if [ ${x_pos} -eq 1 ]; then
        echo -n "${TAB}"
    fi

    # determine type
    [ -L "${target}" ] && type="link ${VALID}"
    [ -f "${target}" ] && type="file ${FILE}"
    [ -d "${target}" ] && type="directory ${DIR}"

    # check if target exists
    echo -en "target ${type}${target}${RESET}... "
    export elin=0
    if [ -e "${target_canon}" ]; then
        echo -e "${GOOD}exists${RESET}"
        return 0
    else
        echo -e "${BAD}does not exist${RESET}"
        return 1
    fi
}

# This function is used to check destination link directories that must exist like ${HOME} or
# /etc. If the directory does not exist, return with error.
function check_link_dir() {
    check_arg1 $@

    # define link (destination)
    local link="$@"
    local link_canon=$(readlink -f "${link}")

    # get the cursor position
    echo -en "\E[6n"
    read -sdR CURPOS
    local CURPOS=${CURPOS#*[}
          #}# dummy bracket for Emacs indenting
    # get the x-position of the cursor
    local -i x_pos=${CURPOS#*;}
    #echo "${TAB}x_pos=${x_pos}"
    if [ ${x_pos} -eq 1 ]; then
        echo -n "${TAB}"
    fi

    # determine
    [ -L "${link}" ] && type="link ${VALID}"
    [ -f "${link}" ] && type="file ${FILE}"
    [ -d "${link}" ] && type="directory ${DIR}"

    # check if link exists
    echo -en "link ${type}${link}${RESET}... "
    if [ -e "${link_canon}" ]; then
        echo -e "${GOOD}exists${RESET}"
        return 0
    else
        echo -e "${BAD}does not exist"
        return 1
    fi
}

function print_OK() {
    #    return
    echo -en "\E[${elin}F\E[0J"
    echo -e "${TAB}target ${type}${target}${RESET}... ${GOOD}OK${RESET}"
}

function do_link() {
    # PURPOSE - create a link to a target
    #
    # SYNTAX
    #   do_link target link_name
    #   the syntax is the same as the 'ln' intrinsic
    #   two arguments are required
    #
    # DEPENDENCIES
    #   check_target
    #
    # METHOD -
    #   FIND check if the target (source) exists
    #   SSH handling - applies when files are in .ssh/
    #     check permission of the parent directory
    #     check the public/private permission of individual files
    #     exclude linking of certain files
    #   LINK
    #     check if link_name exists
    #       + check if link_name points to target
    #           + already done, LS link_name and RETURN
    #           - check if link_name is writable
    #               + check if link_name and target have the same contents
    #                   + DELETE
    #                   - check if exists
    #                       + get date from date
    #                       - get date from stat
    #                     RENAME the existing file occupying the link_name
    #               - issue error and RETURN
    #       - proceed with linking
    #     LINK
    #       check if target is authorized_keys
    #         + create physical link
    #         - create symbolic link
    #
    # CALLED BY
    #   do_link_exe

    # check arguments
    check_arg2 $@

    # define target (source)
    local target="$1"

    # define link name (destination)
    local link_name="$2"

    # check if target exists
    if [ ${#FUNCNAME[@]} -gt 1 ]; then
        if [[ ! ${FUNCNAME[1]} =~ "do_link" ]]; then
            check_target "$target" || return 1
        else
            decho "${TAB}${target##*/} already checked"
        fi
    fi

    # check if target is an SSH configuration file
    local target_dir=$(dirname "$target")
    if [[ "${target_dir}" == *".ssh" ]]; then
        itab
        echo "${TAB}Applying options for SSH configuration files..."

        # before linking, check parent directory permissions
        echo -n "${TAB}${target_dir} requires specific permissions: "
        ((++elin))
        local -i permOK=700
        echo "${permOK}"
        itab
        echo -n "${TAB}checking permissions... "
        ((++elin))
        local -i perm=$(stat -c "%a" "${target_dir}")
        echo -n "${perm} "
        if [[ ${perm} -gt ${permOK} ]]; then
            echo -e "${BAD}FAIL${RESET}"
            echo -en "${TAB}${GRH}changing permissions${RESET} to ${permOK}... "
            chmod ${permOK} ${target_dir}
            local RETVAL=$?
            if [ $RETVAL -eq 0 ]; then
                echo -e "${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
            else
                echo -e "${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
            fi
        else
            echo -e "${GOOD}OK${RESET}"
        fi
        dtab

        # ...and file permissions
        if [[ ${target##*/} = id* || ${target##*/} = config || ${target##*/} = authorized_keys* ]]; then
            # determine permission requirements
            echo -n "${TAB}${target##*/} requires specific permissions: "
            ((++elin))
            if [[ "${target}" == *"pub"* ]]; then
                local -i permOK=644
                echo -n "PUB "
            else
                local -i permOK=600
                echo -n "PRIV "
            fi
            echo "${permOK}"

            # check existing permissions
            itab
            echo -n "${TAB}checking permissions... "
            ((++elin))
            # if the target file is itself a link, the permissions are irrelevant; check the
            # permissions of the canonicalized target
            local target_canon=$(readlink -f "${target}")
            local -i perm=$(stat -c "%a" "${target_canon}")
            echo -n "${perm} "
            ((++elin))
            # if necessary, the canonicalized target file will have its existing permissions
            # replaced with the required permissions
            if [[ ${perm} -gt ${permOK} ]]; then
                echo -e "${BAD}FAIL${RESET}"
                echo -en "${TAB}${GRH}changing permissions${RESET} to ${permOK}... "
                chmod ${permOK} ${target_canon}
                local RETVAL=$?
                if [ $RETVAL -eq 0 ]; then
                    echo -e "${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
                else
                    echo -e "${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
                fi
            else
                echo -e "${GOOD}OK${RESET}"
                dtab
            fi
            dtab
        fi

        # check exclusions
        for fname in keys2 pub. \~ id_dsa _[0-9]{4}-[0-9]{2}-[0-9]{2}; do
            # check both pattern matching and regex
            if [[ ${target##*/} == *"$fname"* ]] || [[ ${target##*/} =~ .*$fname.* ]]  ; then
                itab
                echo -e "${TAB}${GRH}exclude${RESET} $fname: ${target##*/}"
                dtab 2
                return 0
            fi
        done
        dtab
    fi # done with SSH settings

    # begin linking...
    itab
    echo -n "${TAB}link name ${link_name}... "
    ((++elin))

    # first, check for existing copy
    if [ -L "${link_name}" ] || [ -f "${link_name}" ] || [ -d "${link_name}" ]; then
        itab
        echo -n "exists and "
        # check if link and target are the same
        if [[ "${target}" == "${link_name}" ]]; then
            echo -e "${IT}is${NORMAL} ${target}"
            echo -n "${TAB}skipping..."
            ((++elin))
            dtab 2 # reset status and link tab
            print_OK
            return 0
        fi

        # check if link already points to the target
        # in the case of an authorized_keys file, the target must be hard-linked
        local inode_target=$(stat -c "%i" "${target}")
        local inode_link=$(stat -c "%i" "${link_name}")
        if [[ "${target}" -ef "${link_name}" && "${target}" != *"_keys"* ]] || [ ${inode_target} -eq ${inode_link} ] ; then
            echo "already points to $(basename ${link_name})"
            echo -n "${TAB}"
            if [ "$(stat -c "%i" "${target}")" == "$(stat -c "%i" "${link_name}")" ]; then
                echo -n "hardlink: "
            else
                echo -n "symlink: "
            fi
            #TODO print only link and target
            ls -lhG --color=always "${link_name}" | tr -s ' ' | cut -d' ' -f 8-
            ((++elin))
            echo -n "${TAB}skipping..."
            ((++elin))
            dtab 2 # reset status and link tab
            print_OK
            return 0
        else
            # next, check write permissions
            local link_dir=$(dirname "${link_name}")
            if  [ -w "${link_dir}" ] && ([ -w "${link_name}" ] || [ ! -e "${link_name}" ]); then
                # then, delete or backup existing copy

                # check file contents
                if [ $(diff -ebwB "${target}" "${link_name}" 2>&1 | wc -c) -eq 0 ]; then
                    echo "has the same contents"
                    echo -n "${TAB}deleting... "
                    rm -v "${link_name}"
                else
                    # get file/link modification date
                    if [ -e "${link_name}" ]; then
                        echo "will be backed up..."
                        local mdate=$(date -r "${link_name}" +'%Y-%m-%d-t%H%M')
                    else
                        echo "is a broken link..."
                        local mdate=$(stat -c '%y' "${link_name}" | sed 's/\(^[0-9-]*\) \([0-9:]*\)\..*$/\1-t\2/' | sed 's/://g')
                    fi
                    # backup/rename existing file
                    echo -en "${TAB}"
                    local link_copy="${link_name}_${mdate}"
                    mv -v "${link_name}" "${link_copy}"
                fi
            else
                # issue warning
                echo -en "${BAD}cannot be written to"
                if [ "$EUID" -ne 0 ]; then
                    echo -e "\n${TAB}${BAD}This command must be run as root!${RESET}"
                fi
                dtab 2
                return 1
            fi
            #dtab 2
        fi
        dtab
    else
        echo "does not exist"
    fi
    # finally, link target to link_name
    itab
    echo -en "${GRH}"
    hline 72
    echo -e "${TAB}${GRH}making link... "
    # check if target file is authorized_keys
    if [[ "${target}" == *"_keys"* ]]; then
        # make a physical link
        echo -n "${TAB}PHYS: "
        ln -Pv "${target}" ${link_name}
        RETVAL=$?
    else
        # make a symbolic link
        echo -n "${TAB}SYM: "
        ln -sv "${target}" ${link_name}
        RETVAL=$?
    fi
    hline 72
    dtab 2
    return $RETVAL
}

function do_link_exe() {
    # PURPOSE - create a link to an executable file
    #
    # SYNTAX
    #   do_link_exe target link_name
    #   the syntax is the same as the 'ln' intrinsic
    #   two arguments are required
    #
    # DEPENDENCIES
    #   check_target
    #   do_link
    #
    # METHOD -
    #   FIND check if the target (source) exists
    #   PERM check the target is executable
    #   LINK pass arguments to do_link

    check_arg2 $@

    # define target (source)
    local target="$1"

    # define link (destination)
    local link_name="$2"

    check_target "$target" || return 1

    # next, check file permissions
    itab
    echo -n "${TAB}${target##*/} requires specific permissions: "
    ((++elin))
    local permOK=500
    echo "${permOK}"
    itab
    echo -n "${TAB}checking permissions... "
    ((++elin))
    local perm=$(stat -c "%a" "${target}")
    echo ${perm}
    # the target files will have the required permissions added to the existing permissions
    if [[ ${perm} -le ${permOK} ]] || [[ ! (-f "${target}" && -x "${target}") ]]; then
        echo -en "${TAB}${GRH}adding permissions${RESET} to ${permOK}... "
        chmod +${permOK} "${target}" || chmod u+rx "${target}"
        local RETVAL=$?
        if [ $RETVAL -eq 0 ]; then
            echo -e "${GOOD}OK${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        else
            echo -e "${BAD}FAIL${RESET} ${GRAY}RETVAL=$RETVAL${RESET}"
        fi
    else
        echo -e "${TAB}permissions ${GOOD}OK${RESET}"
    fi
    ((++elin))
    dtab 2

    # then link
    do_link "$target" "$link_name" || return 1
    return 0
}

function check_link() {
    # PURPOSE - checks a link to a target
    #
    # SYNTAX
    #   check_link target link_name
    #   the syntax is the same as the 'ln' intrinsic
    #   two arguments are required
    #
    # DEPENDENCIES
    #   check_target
    #
    # METHOD -
    #   FIND check if the target (source) exists
    #   LINK
    #     check if link_name exists
    #       + check if link_name points to target
    #           + already done, LS link_name and RETURN
    #           - check if link_name is writable
    #               + check if link_name and target have the same contents
    #                   + DELETE
    #                   - check if exists
    #                       + get date from date
    #                       - get date from stat
    #                     RENAME the existing file occupying the link_name
    #               - issue error and RETURN
    #       - proceed with linking

    # check arguments
    check_arg2 $@

    # define target (source)
    local target="$1"

    # define link name (destination)
    local link_name="$2"

    check_target "$target" || return 1
    check_target "$link_name" || return 1

    echo -n "${TAB}${target} and ${link_name}... "

    # check if link and target are the same
    if [[ "${target}" == "${link_name}" ]]; then
        echo -e "$have the same logical name"
    fi

    # check if link already points to the target
    # in the case of an authorized_keys file, the target must be hard-linked
    local -i inode_target=$(stat -c "%i" "${target}")
    local -i inode_link=$(stat -c "%i" "${link_name}")
    decho
    decho "target: ${inode_target}"
    decho "  link: ${inode_link}"
    if [ ${inode_target} -eq ${inode_link} ] ; then
        decho "same inode"
    else
        decho "different inode"
    fi
    if [ ${inode_target} == ${inode_link} ] ; then
        echo "are hard-linked"
        return 0
    else
        echo "are not the same"
        (
            echo "${target}: $inode_target"
            echo "${link_name}: $inode_link"
        ) | column -t -s: -N file,inode | sed "s/^/${TAB}/"
    fi

    if [[ "${target}" -ef "${link_name}" ]]; then
        echo "are symlinked"
        echo -n "${TAB}"
        ls -lhG --color=always "${link_name}" | tr -s ' ' | cut -d' ' -f 8-
        return 0
    else
        echo "are not linked"
        # check file contents
        if [ $(diff -ebwB "${target}" "${link_name}" 2>&1 | wc -c) -eq 0 ]; then
            echo "have the same contents"
            return 0
        else
            # get file/link modification date
            if [ -e "${link_name}" ]; then
                echo "has different contents"
                return 1
            else
                echo "is a broken link..."
                return 0
            fi
        fi
    fi
    dtab
}

function do_make_dir() {
    # METHOD
    #  check if target directory exists
    #   + return
    #   - make the directory
    #
    # DEPENDENCIES
    #   check_target

    local DEBUG=0
    check_arg1 $@

    # define target (source)
    local target="$@"

    # Since the specified directory may not exist, a return code of 1 from check_target is
    # valid. Disable exit-on-error and turn off traps.
    if [[ "$-" == *e* ]]; then
        old_opts=$(echo "$-")
        set +e
    fi
    unset_traps

    # check if target exists
    check_target "$target"
    local -i RETVAL=$?

    # if target does not exist, make the directory
    if [ $RETVAL = 1 ]; then
        itab
        echo -en "${TAB}${GRH}"
        hline 72
        echo "${TAB}${GRH}making directory... "
        echo -n "${TAB}"
        mkdir -pv "${target}"
        RETVAL=$?
        echo -ne "${TAB}"
        hline 72
        dtab
    fi

    # reset shell options and traps
    reset_shell ${old_opts-''}
    reset_traps

    # return value of last relevant command
    return $RETVAL
}

function do_make_link() {
    # DESCRIPTION - check if target directory exists and link to it. If the target directory does
    # not exist, create it.
    #
    # DEPENDENCIES
    #   check_target
    #   do_link
    #   do_make_dir

    check_arg2 $@

    # define target (source)
    local target="$1"

    # define link (destination)
    local link_name="$2"

    do_make_dir "$target" || return 1

    do_link "$target" "$link_name" || return 1
    return 0
}

function find_broken() {
    # find links
    find -L ./ -type l | xargs ~/utils/bash/rm_broken_dupes.sh
}
