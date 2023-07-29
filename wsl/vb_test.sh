clear -x
VAR=VB

echo "VAR: ${VAR} = '${!VAR}'"

TRUE='\x1B[1;32mtrue\x1B[0m'
FALSE='\x1B[1;31mfalse\x1B[0m'
UNSET='\x1B[1;33munset\x1B[0m'

# true false
echo -e "----------------------------------------------------"
echo -e -n "             bare : "
if ${!VAR}; then
    echo -e " ${TRUE}"
else
    echo -e "${FALSE}"
fi
echo -e -n "         brackets : "
if ${!VAR}; then
    echo -e " ${TRUE}"
else
    echo -e "${FALSE}"
fi

echo -e -n "        not unset\x1B[0m : "
if [ ! -z ${!VAR:+dummy} ]; then
    echo -e " ${TRUE}"
else
    echo -e "${FALSE}"
fi

echo -e -n "not unset\x1B[0m and true\x1B[0m: "
if [ ! -z ${!VAR:+dummy} ] && ${!VAR}; then
    echo -e " ${TRUE}"
else
    echo -e "${FALSE}"
fi

echo -e -n "      set and true: "
if [ -n ${!VAR:+dummy} ] && ${!VAR}; then
    echo -e " ${TRUE}"
else
    echo -e "${FALSE}"
fi

echo -e "----------------------------------------------------"
# null, no quotes
echo -e "----------------------------------------------------"
echo -e -n "    NULL (-z)         : "
if [ -z ${!VAR} ]; then
    echo -e " ${TRUE}: ${UNSET} or null (empty)"
else
    echo -e "${FALSE}: set and not null"
fi

echo -e -n "    NULL (-z -)       : "
if [ -z ${!VAR-dummy} ]; then
    echo -e " ${TRUE}: set and null (empty)"
else
    echo -e "${FALSE}: ${UNSET} or not null"
fi

echo -e -n "    NULL (-z +)       : "
if [ -z ${!VAR+dummy} ]; then
    echo -e " ${TRUE}: ${UNSET}"
else
    echo -e "${FALSE}: set (maybe null)"
fi

echo -e -n "    NULL (-z :+)      : "
if [ -z ${!VAR:+dummy} ]; then
    echo -e " ${TRUE}: ${UNSET} or null"
else
    echo -e "${FALSE}: set and not null"
fi

# not null, no quotes
echo -e "----------------------------------------------------"
echo -e -n "NOT NULL (! -z)       : "
if [ ! -z ${!VAR} ]; then
    echo -e " ${TRUE}: not null"
else
    echo -e "${FALSE}: ${UNSET} or null"
fi

echo -e -n "NOT NULL (! -z -)     : "
if [ ! -z ${!VAR-dummy} ]; then
    echo -e " ${TRUE}: ${UNSET} or not null"
else
    echo -e "${FALSE}: set and null"
fi

echo -e -n "NOT NULL (! -z +)     : "
if [ ! -z ${!VAR+dummy} ]; then
    echo -e " ${TRUE}: set (maybe null)"
else
    echo -e "${FALSE}: ${UNSET}"
fi

echo -e -n "NOT NULL (! -z :+)    : "
if [ ! -z ${!VAR:+dummy} ]; then
    echo -e " ${TRUE}: set and not null"
else
    echo -e "${FALSE}: ${UNSET} or null"
fi

# not null, quotes
echo -e "----------------------------------------------------"
echo -e -n "NOT NULL (-n \"\")      : "
if [ -n "${!VAR}" ]; then
    echo -e " ${TRUE}: set and not null (empty)"
else
    echo -e "${FALSE}: ${UNSET} or null"
fi

echo -e -n "NOT NULL (-n - \"\")    : "
if [ -n "${!VAR-dummy}" ]; then
    echo -e " ${TRUE}: ${UNSET} or not null (empty)"
else
    echo -e "${FALSE}: set and null"
fi

echo -e -n "NOT NULL (-n + \"\")    : "
if [ -n "${!VAR+dummy}" ]; then
    echo -e " ${TRUE}: set (maybe null)"
else
    echo -e "${FALSE}: ${UNSET}"
fi

echo -e -n "NOT NULL (-n :+ \"\")   : "
if [ -n "${!VAR:+dummy}" ]; then
    echo -e " ${TRUE}: set and not null"
else
    echo -e "${FALSE}: ${UNSET} or null"
fi

# not not null, quotes
echo -e "----------------------------------------------------"
echo -e -n "    NULL (! -n \"\")    : "
if [ ! -n "${!VAR}" ]; then
    echo -e " ${TRUE}: ${UNSET} or null"
else
    echo -e "${FALSE}: set and not null"
fi

echo -e -n "    NULL (! -n - \"\")  : "
if [ ! -n "${!VAR-dummy}" ]; then
    echo -e " ${TRUE}: set and null"
else
    echo -e "${FALSE}: ${UNSET} or not null"
fi

echo -e -n "    NULL (! -n + \"\")  : "
if [ ! -n "${!VAR+dummy}" ]; then
    echo -e " ${TRUE}: ${UNSET}"
else
    echo -e "${FALSE}: set (maybe null)"
fi

echo -e -n "    NULL (! -n :+ \"\") : "
if [ ! -n "${!VAR:+dummy}" ]; then
    echo -e " ${TRUE}: ${UNSET} or null"
else
    echo -e "${FALSE}: set and not null"
fi
echo -e "----------------------------------------------------"
# not null ands, quotes
echo -e "----------------------------------------------------"
echo -e -n "NOT NULL (! -z && -n \"\")    : "
if [ ! -z ${!VAR} ] && [ -n "${!VAR}"  ]; then
    echo -e " ${TRUE}: set and not null"
else
    echo -e "${FALSE}: ${UNSET} or null"
fi

echo -e -n "NOT NULL (! -z && -n + \"\")  : "
if [ ! -z ${!VAR+dummy} ] && [ -n "${!VAR+dummy}"  ]; then
    echo -e " ${TRUE}: set (maybe null)"
else
    echo -e "${FALSE}: ${UNSET}"
fi

echo -e -n "NOT NULL (! -z && -n :+ \"\") : "
if [ ! -z ${!VAR:+dummy} ] && [ -n "${!VAR:+dummy}" ]; then
    echo -e " ${TRUE}: set and not null"
else
    echo -e "${FALSE}: ${UNSET} or null"
fi

# null ands, quotes
echo -e "----------------------------------------------------"
echo -e -n "    NULL (-z && ! -n \"\")    : "
if [ -z ${!VAR} ] && [ ! -n "${!VAR}"  ]; then
    echo -e " ${TRUE}: ${UNSET} or null"
else
    echo -e "${FALSE}: set and not null"
fi

echo -e -n "    NULL (-z && ! -n + \"\")  : "
if [ -z ${!VAR+dummy} ] && [ ! -n "${!VAR+dummy}" ]; then
    echo -e " ${TRUE}: ${UNSET}"
else
    echo -e "${FALSE}: set (maybe null)"
fi

echo -e -n "    NULL (-z && ! -n :+ \"\") : "
if [ -z ${!VAR:+dummy} ] && [ ! -n "${!VAR:+dummy}" ]; then
    echo -e " ${TRUE}: ${UNSET} or null"
else
    echo -e "${FALSE}: set and not null"
fi

echo -e "----------------------------------------------------"
# impossible and
echo -e "----------------------------------------------------"
echo -e -n "    NULL and NOT NULL (-z && -n \"\")     : "
if [ -z ${!VAR} ] && [ -n "${!VAR}"  ]; then
    echo -e " ${TRUE}: impossible!"
else
    echo -e "${FALSE}: OK"
fi

echo -e -n "NOT NULL and     NULL (! -z && ! -n \"\") : "
if [ ! -z ${!VAR} ] && [ ! -n "${!VAR}"  ]; then
    echo -e " ${TRUE}: impossible!"
else
    echo -e "${FALSE}: OK"
fi
echo -e "----------------------------------------------------"

if (return 0 2>/dev/null); then
    echo "sourced=1"
    return
else
    echo "sourced=0"
fi

echo -e -n "NULL (-z :+): "
if [ -z ${!VAR:+dummy} ]; then
    echo -e "${VAR} ${UNSET}"
else
    echo -e "${VAR} set"
    if [ -z ${!VAR} ]; then
	echo -e " ${TRUE}: ${VAR} -z yes"
    else
	echo -e "${FALSE}: ${VAR} -z no"
    fi

    if [ -n ${!VAR} ]; then
	echo -e " ${TRUE}: ${VAR} -n yes"
    else
	echo -e "${FALSE}: ${VAR} -n no"
    fi
    if ${!VAR}; then
	echo -e " ${TRUE}: ${VAR} yes"
    else
	echo -e "${FALSE}: ${VAR} no"
    fi

fi
