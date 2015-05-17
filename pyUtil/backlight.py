#!/usr/bin/env python

from subprocess import call, check_output

status = check_output("xbacklight | cut -d '.' -f 1", shell=True)
try:
    status = int(status)
except ValueError:
    status = 5

if status >= 90:
    call("xbacklight -set 75", shell=True)
elif status >= 60:
    call("xbacklight -set 50", shell=True)
elif status >= 40:
    call("xbacklight -set 25", shell=True)
elif status >= 15:
    call("xbacklight -set 5", shell=True)
else:
    call("xbacklight -set 100", shell=True)

