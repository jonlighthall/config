# ------------------------------------------------------------------------------
# KORN SHELL SETTINGS
# ------------------------------------------------------------------------------
#
# ~/config/linux/.kshrc
#
# Purpose: Define prompt and path settings for interactive shells on linux
#   kernel systems
#
# Sep 2024 JCL
#
# ------------------------------------------------------------------------------

# prompt settings
export PS1="$(id -un)@$(hostname) $PWD
$ "
