#!/bin/bash

if pgrep -x xcompmgr &>/dev/null; then
    echo "Turning xcompmgr ON"
    pkill xcompmgr &
else
    echo "Turning xcompmgr OFF"
    xcompmgr -c -C -t-5 -l-5 -r4.2 -o.55 &
fi

exit 0
