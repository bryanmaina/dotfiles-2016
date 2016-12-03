#!/bin/bash

# Terminate already running bar instances
killall -q polybar 

# Wait until the pollybar processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done 

# get info from xrandr
activeOutput=$(xrandr | grep -E " connected (primary )?[1-9]+" | sed -e "s/\([A-Z0-9]\+\) connected.*/\1/")

# initialize variables
intern=LVDS1
extern=VGA1
i=0

for display in $activeOutput
do
  i=$(( $i + 1 ))
done


if [ $i -eq 2 ]; then
  xrandr --output "$extern" --off --output "$intern" --auto &
  sleep 1
  polybar mainbar &
  echo "Dual monitors mode OFF"
else
  xrandr --output "$intern" --primary --auto --output "$extern" --right-of "$intern" --auto &
  polybar mainbar &
  sleep 1
  polybar exbar &
  echo "Dual monitors mode ON"
fi

exit 0
