echo "VB = '${VB}'"

# null, no quotes
echo "----------------------------------------------------"
echo -n "    NULL (-z)         : "
if [ -z ${VB} ]; then
    echo " true: unset or null (empty)"
else
    echo "false: set and not null"
fi

echo -n "    NULL (-z -)       : "
if [ -z ${VB-dummy} ]; then
    echo " true: set and null (empty)"
else
    echo "false: unset or not null"
fi

echo -n "    NULL (-z +)       : "
if [ -z ${VB+dummy} ]; then
    echo " true: unset"
else
    echo "false: set (maybe null)"
fi

echo -n "    NULL (-z :+)      : "
if [ -z ${VB:+dummy} ]; then
    echo " true: unset or null"
else
    echo "false: set and not null"
fi

# null, quotes
echo "----------------------------------------------------"
echo -n "    NULL (-z \"\")      : "
if [ -z "${VB}" ]; then
    echo " true: unset or null (empty)"
else
    echo "false: set and not null"
fi

echo -n "    NULL (-z - \"\")    : "
if [ -z "${VB-dummy}" ]; then
    echo " true: set and null (empty)"
else
    echo "false: unset or not null"
fi

echo -n "    NULL (-z + \"\")    : "
if [ -z "${VB+dummy}" ]; then
    echo " true: unset"
else
    echo "false: set (maybe null)"
fi

echo -n "    NULL (-z :+ \"\")   : "
if [ -z "${VB:+dummy}" ]; then
    echo " true: unset or null"
else
    echo "false: set and not null"
fi

echo "----------------------------------------------------"
# not null, no quotes
echo -n "NOT NULL (! -z)       : "
if [ ! -z ${VB} ]; then
    echo " true: not null"
else
    echo "false: unset or null"
fi

echo -n "NOT NULL (! -z -)     : "
if [ ! -z ${VB-dummy} ]; then
    echo " true: unset or not null"
else
    echo "false: set and null"
fi

echo -n "NOT NULL (! -z +)     : "
if [ ! -z ${VB+dummy} ]; then
    echo " true: set (maybe null)"
else
    echo "false: unset"
fi

echo -n "NOT NULL (! -z :+)    : "
if [ ! -z ${VB:+dummy} ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

# not null, quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (! -z \"\")    : "
if [ ! -z "${VB}" ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

echo -n "NOT NULL (! -z - \"\")  : "
if [ ! -z "${VB-dummy}" ]; then
    echo " true: unset or not null"
else
    echo "false: set and null"
fi

echo -n "NOT NULL (! -z + \"\")  : "
if [ ! -z "${VB+dummy}" ]; then
    echo " true: set (maybe null)"
else
    echo "false: unset"
fi

echo -n "NOT NULL (! -z :+ \"\") : "
if [ ! -z "${VB:+dummy}" ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi
echo "----------------------------------------------------"
# not null, no quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (-n)      BAD: "
if [ -n ${VB} ]; then # BAD when unset or null
    echo " true: set and not null (empty)"
else
    echo "false: unset or null"
fi

echo -n "NOT NULL (-n -)    BAD: "
if [ -n ${VB-dummy} ]; then # BAD when null
    echo " true: unset or not null (empty)"
else
    echo "false: set and null"
fi

echo -n "NOT NULL (-n +)    BAD: "
if [ -n ${VB+dummy} ]; then # BAD when unset
    echo " true: set (maybe null)"
else
    echo "false: unset"
fi

echo -n "NOT NULL (-n :+)   BAD: "
if [ -n ${VB:+dummy} ]; then # BAD when unset or null
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

# not null, quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (-n \"\")      : "
if [ -n "${VB}" ]; then
    echo " true: set and not null (empty)"
else
    echo "false: unset or null"
fi

echo -n "NOT NULL (-n - \"\")    : "
if [ -n "${VB-dummy}" ]; then
    echo " true: unset or not null (empty)"
else
    echo "false: set and null"
fi

echo -n "NOT NULL (-n + \"\")    : "
if [ -n "${VB+dummy}" ]; then
    echo " true: set (maybe null)"
else
    echo "false: unset"
fi

echo -n "NOT NULL (-n :+ \"\")   : "
if [ -n "${VB:+dummy}" ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

# not not null, no quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (! -n)    BAD: "
if [ ! -n ${VB} ]; then # BAD when unset or null
    echo " true: unset or null"
else
    echo "false: set and not null"
fi

echo -n "NOT NULL (! -n -)  BAD: "
if [ ! -n ${VB-dummy} ]; then # BAD when null
    echo " true: set and null"
else
    echo "false: unset or not null"
fi

echo -n "NOT NULL (! -n +)  BAD: "
if [ ! -n ${VB+dummy} ]; then # BAD when unset
    echo " true: unset"
else
    echo "false: set (maybe null)"
fi

echo -n "NOT NULL (! -n :+) BAD: "
if [ ! -n ${VB:+dummy} ]; then # BAD when unset or null
    echo " true: unset or null"
else
    echo "false: set and not null"
fi

# not not null, quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (! -n \"\")    : "
if [ ! -n "${VB}" ]; then
    echo " true: unset or null"
else
    echo "false: set and not null"
fi

echo -n "NOT NULL (! -n - \"\")  : "
if [ ! -n "${VB-dummy}" ]; then
    echo " true: set and null"
else
    echo "false: unset or not null"
fi

echo -n "NOT NULL (! -n + \"\")  : "
if [ ! -n "${VB+dummy}" ]; then
    echo " true: unset"
else
    echo "false: set (maybe null)"
fi

echo -n "NOT NULL (! -n :+ \"\") : "
if [ ! -n "${VB:+dummy}" ]; then
    echo " true: unset or null"
else
    echo "false: set and not null"
fi
echo "----------------------------------------------------"
# not null ands, not quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (! -z && -n)       : "
if [ ! -z ${VB} ] && [ -n ${VB}  ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

echo -n "NOT NULL (! -z && -n +)     : "
if [ ! -z ${VB+dummy} ] && [ -n ${VB+dummy}  ]; then
    echo " true: set (maybe null)"
else
    echo "false: unset"
fi

echo -n "NOT NULL (! -z && -n :+)    : "
if [ ! -z ${VB:+dummy} ] && [ -n ${VB:+dummy}  ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

# not null ands, quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (! -z && -n \"\")    : "
if [ ! -z ${VB} ] && [ -n "${VB}"  ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

echo -n "NOT NULL (! -z && -n + \"\")  : "
if [ ! -z ${VB+dummy} ] && [ -n "${VB+dummy}"  ]; then
    echo " true: set (maybe null)"
else
    echo "false: unset"
fi

echo -n "NOT NULL (! -z && -n :+ \"\") : "
if [ ! -z ${VB:+dummy} ] && [ -n "${VB:+dummy}" ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

echo "----------------------------------------------------"
# null ands, no quotes
echo "----------------------------------------------------"
echo -n "    NULL (-z && ! -n)    BAD: "
if [ -z ${VB} ] && [ ! -n ${VB}  ]; then # BAD when unset or null
    echo " true: unset or null"
else
    echo "false: set and not null"
fi

echo -n "    NULL (-z && ! -n +)  BAD: " # BAD when unset
if [ -z ${VB+dummy} ] && [ ! -n ${VB+dummy} ]; then
    echo " true: unset"
else
    echo "false: set (maybe null)"
fi

echo -n "    NULL (-z && ! -n :+) BAD: "
if [ -z ${VB:+dummy} ] && [ ! -n ${VB:+dummy} ]; then # BAD when unset or null
    echo " true: unset or null"
else
    echo "false: set and not null"
fi

# null ands, quotes
echo "----------------------------------------------------"
echo -n "    NULL (-z && ! -n \"\")    : "
if [ -z ${VB} ] && [ ! -n "${VB}"  ]; then
    echo " true: unset or null"
else
    echo "false: set and not null"
fi
   
echo -n "    NULL (-z && ! -n + \"\")  : "
if [ -z ${VB+dummy} ] && [ ! -n "${VB+dummy}" ]; then
    echo " true: unset"
else
    echo "false: set (maybe null)"
fi

echo -n "    NULL (-z && ! -n :+ \"\") : "
if [ -z ${VB:+dummy} ] && [ ! -n "${VB:+dummy}" ]; then
    echo " true: unset or null"
else
    echo "false: set and not null"
fi

echo "----------------------------------------------------"
# impossible and
echo -n "    NULL and     NOT NULL (-z && -n)        : "
if [ -z ${VB} ] && [ -n ${VB}  ]; then
    echo " true: impossible! unset or null?"
else
    echo "false: OK"
fi

echo -n "    NULL and     NOT NULL (-z && -n \"\")     : "
if [ -z ${VB} ] && [ -n "${VB}"  ]; then
    echo " true: impossible!"
else
    echo "false: OK"
fi

echo -n "NOT NULL and NOT NOT NULL (! -z && ! -n)    : "
if [ ! -z ${VB} ] && [ ! -n ${VB}  ]; then
    echo " true: impossible!"
else
    echo "false: OK"
fi

echo -n "NOT NULL and NOT NOT NULL (! -z && ! -n \"\") : "
if [ ! -z ${VB} ] && [ ! -n "${VB}"  ]; then
    echo " true: impossible!"
else
    echo "false: OK"
fi

echo "----------------------------------------------------"

return
echo -n "NULL (-z :+): "
if [ -z ${VB:+dummy} ]; then
    echo "unset"
else
    echo "set"
    if [ -z ${VB} ]; then
	echo " true: VB -z yes"
    else
	echo "false: VB -z no"
    fi

    if [ -n ${VB} ]; then
	echo " true: VB -n yes"
    else
	echo "false: VB -n no"
    fi
    if ${VB}; then
	echo " true: VB yes"
    else
	echo "false: VB no"
    fi

fi
