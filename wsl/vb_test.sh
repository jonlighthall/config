echo "VB = '${VB}'"

# null, no quotes
echo "----------------------------------------------------"
echo -n "    NULL (-z)         : "
if [ -z ${VB} ]; then
    echo " true: unset or null (empty)"
else
    echo "false: not null"
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
    echo "false: set or null"
fi

echo -n "    NULL (-z :+)      : "
if [ -z ${VB:+dummy} ]; then
    echo " true: unset or null"
else
    echo "false: set"
fi

# null, quotes
echo "----------------------------------------------------"
echo -n "    NULL (-z \"\")      : "
if [ -z "${VB}" ]; then
    echo " true: unset or null (empty)"
else
    echo "false: not null"
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
    echo "false: set or null"
fi

echo -n "    NULL (-z :+ \"\")   : "
if [ -z "${VB:+dummy}" ]; then
    echo " true: unset or null"
else
    echo "false: set"
fi

echo "----------------------------------------------------"
# not null, no quotes
echo -n "NOT NULL (! -z)       : "
if [ ! -z ${VB} ]; then
    echo " true: unset or null (empty)"
else
    echo "false: not null"
fi

echo -n "NOT NULL (! -z -)     : "
if [ ! -z ${VB-dummy} ]; then
    echo " true: set and null (empty)"
else
    echo "false: unset or not null"
fi

echo -n "NOT NULL (! -z +)     : "
if [ ! -z ${VB+dummy} ]; then
    echo " true: unset"
else
    echo "false: set or null"
fi

echo -n "NOT NULL (! -z :+)    : "
if [ ! -z ${VB:+dummy} ]; then
    echo " true: unset or null"
else
    echo "false: set"
fi

# not null, quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (! -z \"\")    : "
if [ ! -z "${VB}" ]; then
    echo " true: unset or null (empty)"
else
    echo "false: not null"
fi

echo -n "NOT NULL (! -z - \"\")  : "
if [ ! -z "${VB-dummy}" ]; then
    echo " true: set and null (empty)"
else
    echo "false: unset or not null"
fi

echo -n "NOT NULL (! -z + \"\")  : "
if [ ! -z "${VB+dummy}" ]; then
    echo " true: unset"
else
    echo "false: set or null"
fi

echo -n "NOT NULL (! -z :+ \"\") : "
if [ ! -z "${VB:+dummy}" ]; then
    echo " true: unset or null"
else
    echo "false: set"
fi
echo "----------------------------------------------------"
# not null, no quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (-n)         : "
if [ -n ${VB} ]; then
    echo " true: unset or null (empty)"
else
    echo "false: not null"
fi

echo -n "NOT NULL (-n -)       : "
if [ -n ${VB-dummy} ]; then
    echo " true: set and null (empty)"
else
    echo "false: unset or not null"
fi

echo -n "NOT NULL (-n +)       : "
if [ -n ${VB+dummy} ]; then
    echo " true: unset"
else
    echo "false: set or null"
fi

echo -n "NOT NULL (-n :+)      : "
if [ -n ${VB:+dummy} ]; then
    echo " true: unset or null"
else
    echo "false: set"
fi

# not null, quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (-n \"\")      : "
if [ -n "${VB}" ]; then
    echo " true: unset or null (empty)"
else
    echo "false: not null"
fi

echo -n "NOT NULL (-n - \"\")    : "
if [ -n "${VB-dummy}" ]; then
    echo " true: set and null (empty)"
else
    echo "false: unset or not null"
fi

echo -n "NOT NULL (-n + \"\")    : "
if [ -n "${VB+dummy}" ]; then
    echo " true: unset"
else
    echo "false: set or null"
fi

echo -n "NOT NULL (-n :+ \"\")   : "
if [ -n "${VB:+dummy}" ]; then
    echo " true: unset or null"
else
    echo "false: set"
fi

# not not null, no quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (! -n)       : "
if [ ! -n ${VB} ]; then
    echo " true: unset or null (empty)"
else
    echo "false: not null"
fi

echo -n "NOT NULL (! -n -)     : "
if [ ! -n ${VB-dummy} ]; then
    echo " true: set and null (empty)"
else
    echo "false: unset or not null"
fi

echo -n "NOT NULL (! -n +)     : "
if [ ! -n ${VB+dummy} ]; then
    echo " true: unset"
else
    echo "false: set or null"
fi

echo -n "NOT NULL (! -n :+)    : "
if [ ! -n ${VB:+dummy} ]; then
    echo " true: unset or null"
else
    echo "false: set"
fi

# not not null, quotes
echo "----------------------------------------------------"
echo -n "NOT NULL (! -n \"\")    : "
if [ ! -n "${VB}" ]; then
    echo " true: unset or null (empty)"
else
    echo "false: not null"
fi

echo -n "NOT NULL (! -n - \"\")  : "
if [ ! -n "${VB-dummy}" ]; then
    echo " true: set and null (empty)"
else
    echo "false: unset or not null"
fi

echo -n "NOT NULL (! -n + \"\")  : "
if [ ! -n "${VB+dummy}" ]; then
    echo " true: unset"
else
    echo "false: set or null"
fi

echo -n "NOT NULL (! -n :+ \"\") : "
if [ ! -n "${VB:+dummy}" ]; then
    echo " true: unset or null"
else
    echo "false: set"
fi
echo "----------------------------------------------------"
# not null ands
echo "----------------------------------------------------"
echo -n "NOT NULL (! -z && -n)       : "
if [ ! -z ${VB} ] && [ -n ${VB}  ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

echo -n "NOT NULL (! -z && -n +)     : "
if [ ! -z ${VB+dummy} ] && [ -n ${VB+dummy}  ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

echo -n "NOT NULL (! -z && -n :+)    : "
if [ ! -z ${VB:+dummy} ] && [ -n ${VB:+dummy}  ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

echo "----------------------------------------------------"
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
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

echo -n "NOT NULL (! -z && -n :+ \"\") : "
if [ ! -z ${VB:+dummy} ] && [ -n "${VB:+dummy}" ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi



return
echo "----------------------------------------------------"
# null ands
echo "----------------------------------------------------"

echo -n "    NULL (-z && ! -n \"\"): "
if [ -z ${VB} ] && [ ! -n "${VB}"  ]; then
    echo " true: must be null"
else
    echo "false: must be not null"
fi

echo -n "    NULL (-z && ! -n + \"): "
if [ -z ${VB+dummy} ] && [ ! -n "${VB+dummy}" ]; then
    echo " true: must be unset"
else
    echo "false: must be set or null"
fi

echo -n "NULL and NOT NULL (-z && -n): "
if [ -z ${VB} ] && [ -n ${VB}  ]; then
    echo " true: impossible!, VB must be unset"
else
    echo "false: VB either no, VB must be set"
fi

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

echo

echo -n "NOT NULL (-n) BAD       : "
if [ -n ${VB} ]; then
    echo " true: not null"
else
    echo "false: null (length zero)"
fi

echo -n "NOT NULL (-n +) BAD     : "
if [ -n ${VB+dummy} ]; then
    echo " true: set or null"
else
    echo "false: unset or not null"
fi

echo -n "NOT NULL (-n :+) BAD    : "
if [ -n ${VB:+dummy} ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

echo -n "    NULL (-z && ! -n)BAD: "
if [ -z ${VB} ] && [ ! -n ${VB}  ]; then
    echo " true: must be null"
else
    echo "false: must be not null"
fi

echo -n "NOT NULL (! -z && -n +) : BAD"
if [ ! -z ${VB+dummy} ] && [ -n ${VB+dummy}  ]; then
    echo " true: set and not null"
else
    echo "false: unset or null"
fi

echo -n "BAD NULL (-z && ! -n +) : "
if [ -z ${VB+dummy} ] && [ ! -n ${VB+dummy}  ]; then
    echo " true: must be unset"
else
    echo "false: must be set or null"
fi
