#!/bin/bash -u
# -----------------------------------------------------------------------------------------------
# LINKS LIBRARY
# -----------------------------------------------------------------------------------------------
#
# ~/config/lib_links.sh
#
# PURPOSE: functions for making links in bash shell.
#
# CONTIANS:
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
    # check number of arguments
    if [ $# -lt 1 ]; then
        echo "number of arguments = $#"
        echo "One argument is required"
        echo "Please provide a target name"
        echo "$FUNCNAME TARGET"
        exit 1
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
        exit 1
    fi
}

function check_target() {
    check_arg1 $@

    # define target (source)
    local target="$1"
    local target_canon=$(readlink -f "${target}")

    # get the cursor position
    echo -en "\E[6n"
    read -sdR CURPOS
    local CURPOS=${CURPOS#*[}
          #}# dummy bracket for emacs indenting
    # get the x-position of the cursor
    local -i x_pos=${CURPOS#*;}
    #echo "${TAB}x_pos=${x_pos}"
    if [ ${x_pos} -eq 1 ]; then
        echo -n "${TAB}"
    fi

    # determine 
    [ -L "${target}" ] && type="link "
    [ -f "${target}" ] && type="file "
    [ -d "${target}" ] && type="directory "

    if [[ "${target_canon}" == ${HOME} ]]; then
        echo -e "target ${YELLOW}$1${RESET} is ${RED}HOME${RESET}"
    fi    
    
    # check if target exists
    echo -en "target ${type}${YELLOW}${target}${RESET}... "
    if [ -e "${target_canon}" ]; then
        echo -e "${GOOD}exists${RESET}"
        itab
        return 0
    else
        echo -e "${BAD}does not exist${RESET}"
        dtab
        return 1
    fi
}

# This function is used to check destination link directories that must exist like ${HOME} or
# /etc. If the directory does not exist, exit with error.
function check_link_dir() {
    check_arg1 $@

    # define link (destination)
    local link="$1"
    local link_canon=$(readlink -f "${link}")

    # get the cursor position
    echo -en "\E[6n"
    read -sdR CURPOS
    local CURPOS=${CURPOS#*[}
          #}# dummy bracket for emacs indenting
    # get the x-position of the cursor
    local -i x_pos=${CURPOS#*;}
    #echo "${TAB}x_pos=${x_pos}"
    if [ ${x_pos} -eq 1 ]; then
        echo -n "${TAB}"
    fi

    # determine 
    [ -L "${link}" ] && type="link "
    [ -f "${link}" ] && type="file "
    [ -d "${link}" ] && type="directory "
    
    # check if link exists
    echo -en "link ${type}${YELLOW}${link}${RESET}... "
    if [ -e "${link_canon}" ]; then
        echo -e "${GOOD}exists${RESET}"
        dtab
        return 0
    else
        echo -e "${BAD}does not exist"
        echo -e "this should never be true${RESET}"
        echo -e "link ${YELLOW}$1${RESET} is ${RED}HOME${NORMAL}"
        dtab        
        exit 1
    fi
}

function do_link() {
    # PURPOSE - create a link to a target
    #
    # SYNTAX - two arguments are required
    #   the syntaxt is the same as the 'ln' intrinsic
    #
    # DEPENDENCIES
    #   check_target
    #
    # METHOD - 
    #   FIND check if the target (source) exists
    #   SSH handling - applies when files are in .ssh/
    #     check premission of the parent directory
    #     check the public/private premission of individual files
    #     exclude linking of certain files
    #   LINK
    #     check if link_name exits
    #       + check if link_name points to target
    #           + already done, LS link_name and RETURN
    #           - check if link_name is writable
    #               + check if link_name and target have the same conents
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
            decho "${TAB}${target##*/} alread checked"
        fi
    fi

    # check if target is an SSH configuration file
    local target_dir=$(dirname "$target")
    if [[ "${target_dir}" == *".ssh" ]]; then
        echo "${TAB}Applying options for SSH configuration files..."

        # before linking, check parent directory permissions
        echo -n "${TAB}${target_dir} requires specific permissions: "
        local -i permOK=700
        echo "${permOK}"
        itab
        echo -n "${TAB}checking permissions... "
        local -i perm=$(stat -c "%a" ${target_dir})
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
            # if the target file is itself a link, the permissions are irrelevant; check the
            # permissions of the canonicalized target
            local target_canon=$(readlink -f "${target}")
            local -i perm=$(stat -c "%a" "${target_canon}")
            echo -n "${perm} "

            # if necessary, the canonicalized target file will have its existing permissions
            # replaced with the required permissions
            if [[ ${perm} -gt ${permOK} ]]; then
                echo -e "${BAD}FAIL${RESET}"
                echo -en "${TAB}${GRH}changing permissions${RESET} to ${permOK}... "
                chmod ${permOK} ${target_cannon}
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
        fi

        # check exclusions
        for fname in keys2 pub. \~ id_dsa _[0-9]{4}-[0-9]{2}-[0-9]{2}; do
            # check both pattern matching and regex
            if [[ ${target##*/} == *"$fname"* ]] || [[ ${target##*/} =~ .*$fname.* ]]  ; then
                echo -e "${TAB}${GRH}exclude${RESET} $fname: ${target##*/}"
                dtab
                return 0
            fi
        done

    fi # done with SSH settings

    # begin linking...
    echo -n "${TAB}link name ${link_name}... "

    # first, check for existing copy
    if [ -L "${link_name}" ] || [ -f "${link_name}" ] || [ -d "${link_name}" ]; then
        itab
        echo -n "exists and "
        # check if link already points to the target
        # in the case of an authorized_keys file, the target must be hardlinked
        if [[ "${target}" -ef "${link_name}" && "${target}" != *"_keys"* ]] || [[ "$(stat -c "%i" ${target})" == "$(stat -c "%i" ${link_name})" ]] ; then
            echo "already points to $(basename ${link_name})"
            echo -n "${TAB}"
            if [ "$(stat -c "%i" "${target}")" == "$(stat -c "%i" "${link_name}")" ]; then
                echo -n "hardlink: "
            else
                echo -n "symlink: "
            fi
            #TODO print only link and target
            ls -lhG --color=always "${link_name}" | tr -s ' ' | cut -d' ' -f 8-
            echo "${TAB}skipping..."
            dtab 2 # reset status and link tab
            return 0
        else
            # next, check write permissions
            local link_dir=$(dirname "${link_name}")
            if  [ -w ${link_dir} ] && ([ -w ${link_name} ] || [ ! -e ${link_name} ]); then
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
                        local mdate=$(stat -c '%y' ${link_name} | sed 's/\(^[0-9-]*\) \([0-9:]*\)\..*$/\1-t\2/' | sed 's/://g')
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
    echo -en "${TAB}${GRH}"
    hline 72
    echo "${TAB}making link... "
    # check if target file is authorized_keys
    if [[ "${target}" == *"_keys"* ]]; then
        # make a physical link
        ln -Pv "${target}" ${link_name} | sed "s/^/${TAB}PHYS: /"
    else
        # make a symbolic link
        ln -sv "${target}" ${link_name} | sed "s/^/${TAB}SYM: /"
    fi
    echo -ne "${TAB}"
    hline 72
    echo -en "${RESET}"
    dtab 2
    return 0
}

function do_link_exe() {
    # PURPOSE - create a link to an executable file
    #
    # SYNTAX - two arguments are required
    #   the syntaxt is the same to the 'ln' intrinsic
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
    if true; then
        echo -n "${TAB}${target##*/} requires specific permissions: "
        local permOK=500
        echo "${permOK}"
        itab
        echo -n "${TAB}checking permissions... "
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
        dtab
    fi

    # then link
    do_link "$target" "$link_name" || return 1
    return 0
}


function do_make_dir() {
    # METHOD
    #  check if target directory exists
    #   + return
    #   - make the directory
    #
    # DEPENDENCIES
    #   check_target
    
    check_arg1 $@
    
    # define target (source)
    local target="$1"

    check_target "$target" || {
        itab 2
        mkdir -pv ${target} &> >(sed "s/^/${TAB}/")
        dtab 2
    }
    return $?
}

function do_make_link() {
    # DESCRIPTION - check if target directory exists and link to it. If the target directory does
    # not exist, link to it
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
