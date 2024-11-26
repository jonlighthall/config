#!/bin/bash -u

# Function to get the current Git user name and email from the Git configuration
function git_get_user() {
    export get_name=$(git config --get user.name)
    export get_email=$(git config --get user.email)
}

# Function to print the current Git user name and email
function git_print_user() {
    git_get_user
    export TAB="     "
    echo "current author name:"
    echo "${TAB}${get_name}"
    echo
    echo "current author email:"
    echo "${TAB}${get_email}"    
}

# Function to set the Git user name and email if they do not match the desired values
function git_set_user() {
    local set_name="Jon Lighthall"
    local set_email="jon.lighthall@gmail.com"

    git_print_user

    local do_update=false

    # Check if the current Git user name matches the desired name
    if [[ "${get_name}" == "${set_name}" ]]; then
        if [ ${DEBUG:-0} -gt 0 ]; then
            echo "names match"
        fi
    else
        echo -e "${TAB}\x1B[1;31mnames do not match\x1B[0m"
        echo "${TAB}setting git user name..." 
        git config user.name "${set_name}"
        do_update=true
    fi

    # Check if the current Git user email matches the desired email
    if [[ "${get_email}" == "${set_email}" ]]; then
        if [ ${DEBUG:-0} -gt 0 ]; then
            echo "emails match"
        fi
    else
        echo -e "${TAB}\x1B[1;31memails do not match\x1B[0m"
        echo "${TAB}setting git user email..."
        git config user.email "${set_email}"
        do_update=true
    fi

    # If any updates were made, print the updated Git user name and email
    if ${do_update}; then
        echo
        git_print_user
    fi
}

# Call the function to set the Git user name and email
git_set_user
