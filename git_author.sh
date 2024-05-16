#!/bin/bash -u

function git_get_user() {
    export get_name=$(git config --get user.name)
    export get_email=$(git config --get user.email)
}

function git_print_user() {
    git_get_user
    local TAB="     "
    echo "current author name:"
    echo "${TAB}${get_name}"
    echo
    echo "current author email:"
    echo "${TAB}${get_email}"    
}

function git_set_user() {
    local set_name="Jon Lighthall"
    local set_email="jon.lighthall@gmail.com"

    git_print_user

    local do_update=false

    if [[ "${get_name}" == "${set_name}" ]]; then
        if [ ${DEBUG:-0} -gt 0 ]; then
            echo "names match"
        fi
    else
        echo "names do not match"
        echo "setting git user name..." 
        git config user.name "${set_name}"
        do_update=true
    fi

    if [[ "${get_email}" == "${set_email}" ]]; then
        if [ ${DEBUG:-0} -gt 0 ]; then
            echo "emails match"
        fi
    else
        echo "emails do not match"
        echo "setting git user email..."
        git config user.email "${set_email}"
        do_update=true
    fi

    if ${do_update}; then
        git_print_user
    fi
}

git_set_user
