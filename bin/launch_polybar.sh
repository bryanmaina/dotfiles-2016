#!/usr/bin/env sh

# Terminate already running bar instances
killall -q polybar

intern=LVDS1
extern=VGA1

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

# Launch bar1 and bar2
if xrandr | grep "$extern disconnected"; then
  polybar mainbar &
else
  polybar mainbar &
  sleep 1
  polybar exbar &
fi

echo "Bars launched..."

