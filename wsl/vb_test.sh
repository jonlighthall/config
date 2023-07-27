echo "VB = '${VB}'"

# true false
echo -e "----------------------------------------------------"
echo -e -n "             bare : "
if $VB; then
    echo -e " \x1B[1;32mtrue\x1B[0m"
else
    echo -e "\x1B[1;31mfalse\x1B[0m"
fi
echo -e -n "         brackets : "
if ${VB}; then
    echo -e " \x1B[1;32mtrue\x1B[0m"
else
    echo -e "\x1B[1;31mfalse\x1B[0m"
fi

echo -e -n "        not unset\x1B[0m : "
if [ ! -z ${VB:+dummy} ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m"
else
    echo -e "\x1B[1;31mfalse\x1B[0m"
fi

echo -e -n "not unset\x1B[0m and true\x1B[0m: "
if [ ! -z ${VB:+dummy} ] && ${VB}; then
    echo -e " \x1B[1;32mtrue\x1B[0m"
else
    echo -e "\x1B[1;31mfalse\x1B[0m"
fi


echo -e -n "      set and true\x1B[0m: "
if [ -n ${VB:+dummy} ] && ${VB}; then
    echo -e " \x1B[1;32mtrue\x1B[0m"
else
    echo -e "\x1B[1;31mfalse\x1B[0m"
fi



echo -e "----------------------------------------------------"
# null, no quotes
echo -e "----------------------------------------------------"
echo -e -n "    NULL (-z)         : "
if [ -z ${VB} ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: \x1B[1;33munset\x1B[0m or null (empty)"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: set and not null"
fi

echo -e -n "    NULL (-z -)       : "
if [ -z ${VB-dummy} ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: set and null (empty)"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: \x1B[1;33munset\x1B[0m or not null"
fi

echo -e -n "    NULL (-z +)       : "
if [ -z ${VB+dummy} ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: \x1B[1;33munset\x1B[0m"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: set (maybe null)"
fi

echo -e -n "    NULL (-z :+)      : "
if [ -z ${VB:+dummy} ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: \x1B[1;33munset\x1B[0m or null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: set and not null"
fi

# not null, no quotes
echo -e "----------------------------------------------------"
echo -e -n "NOT NULL (! -z)       : "
if [ ! -z ${VB} ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: not null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: \x1B[1;33munset\x1B[0m or null"
fi

echo -e -n "NOT NULL (! -z -)     : "
if [ ! -z ${VB-dummy} ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: \x1B[1;33munset\x1B[0m or not null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: set and null"
fi

echo -e -n "NOT NULL (! -z +)     : "
if [ ! -z ${VB+dummy} ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: set (maybe null)"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: \x1B[1;33munset\x1B[0m"
fi

echo -e -n "NOT NULL (! -z :+)    : "
if [ ! -z ${VB:+dummy} ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: set and not null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: \x1B[1;33munset\x1B[0m or null"
fi

# not null, quotes
echo -e "----------------------------------------------------"
echo -e -n "NOT NULL (-n \"\")      : "
if [ -n "${VB}" ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: set and not null (empty)"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: \x1B[1;33munset\x1B[0m or null"
fi

echo -e -n "NOT NULL (-n - \"\")    : "
if [ -n "${VB-dummy}" ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: \x1B[1;33munset\x1B[0m or not null (empty)"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: set and null"
fi

echo -e -n "NOT NULL (-n + \"\")    : "
if [ -n "${VB+dummy}" ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: set (maybe null)"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: \x1B[1;33munset\x1B[0m"
fi

echo -e -n "NOT NULL (-n :+ \"\")   : "
if [ -n "${VB:+dummy}" ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: set and not null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: \x1B[1;33munset\x1B[0m or null"
fi

# not not null, quotes
echo -e "----------------------------------------------------"
echo -e -n "    NULL (! -n \"\")    : "
if [ ! -n "${VB}" ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: \x1B[1;33munset\x1B[0m or null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: set and not null"
fi

echo -e -n "    NULL (! -n - \"\")  : "
if [ ! -n "${VB-dummy}" ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: set and null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: \x1B[1;33munset\x1B[0m or not null"
fi

echo -e -n "    NULL (! -n + \"\")  : "
if [ ! -n "${VB+dummy}" ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: \x1B[1;33munset\x1B[0m"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: set (maybe null)"
fi

echo -e -n "    NULL (! -n :+ \"\") : "
if [ ! -n "${VB:+dummy}" ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: \x1B[1;33munset\x1B[0m or null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: set and not null"
fi
echo -e "----------------------------------------------------"
# not null ands, quotes
echo -e "----------------------------------------------------"
echo -e -n "NOT NULL (! -z && -n \"\")    : "
if [ ! -z ${VB} ] && [ -n "${VB}"  ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: set and not null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: \x1B[1;33munset\x1B[0m or null"
fi

echo -e -n "NOT NULL (! -z && -n + \"\")  : "
if [ ! -z ${VB+dummy} ] && [ -n "${VB+dummy}"  ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: set (maybe null)"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: \x1B[1;33munset\x1B[0m"
fi

echo -e -n "NOT NULL (! -z && -n :+ \"\") : "
if [ ! -z ${VB:+dummy} ] && [ -n "${VB:+dummy}" ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: set and not null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: \x1B[1;33munset\x1B[0m or null"
fi

# null ands, quotes
echo -e "----------------------------------------------------"
echo -e -n "    NULL (-z && ! -n \"\")    : "
if [ -z ${VB} ] && [ ! -n "${VB}"  ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: \x1B[1;33munset\x1B[0m or null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: set and not null"
fi
   
echo -e -n "    NULL (-z && ! -n + \"\")  : "
if [ -z ${VB+dummy} ] && [ ! -n "${VB+dummy}" ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: \x1B[1;33munset\x1B[0m"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: set (maybe null)"
fi

echo -e -n "    NULL (-z && ! -n :+ \"\") : "
if [ -z ${VB:+dummy} ] && [ ! -n "${VB:+dummy}" ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: \x1B[1;33munset\x1B[0m or null"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: set and not null"
fi

echo -e "----------------------------------------------------"
# impossible and
echo -e "----------------------------------------------------"
echo -e -n "    NULL and NOT NULL (-z && -n \"\")     : "
if [ -z ${VB} ] && [ -n "${VB}"  ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: impossible!"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: OK"
fi

echo -e -n "NOT NULL and     NULL (! -z && ! -n \"\") : "
if [ ! -z ${VB} ] && [ ! -n "${VB}"  ]; then
    echo -e " \x1B[1;32mtrue\x1B[0m: impossible!"
else
    echo -e "\x1B[1;31mfalse\x1B[0m: OK"
fi
echo -e "----------------------------------------------------"

return
echo -e -n "NULL (-z :+): "
if [ -z ${VB:+dummy} ]; then
    echo -e "\x1B[1;33munset\x1B[0m"
else
    echo -e "set"
    if [ -z ${VB} ]; then
	echo -e " \x1B[1;32mtrue\x1B[0m: VB -z yes"
    else
	echo -e "\x1B[1;31mfalse\x1B[0m: VB -z no"
    fi

    if [ -n ${VB} ]; then
	echo -e " \x1B[1;32mtrue\x1B[0m: VB -n yes"
    else
	echo -e "\x1B[1;31mfalse\x1B[0m: VB -n no"
    fi
    if ${VB}; then
	echo -e " \x1B[1;32mtrue\x1B[0m: VB yes"
    else
	echo -e "\x1B[1;31mfalse\x1B[0m: VB no"
    fi

fi
