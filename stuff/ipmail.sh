#!/bin/sh

datum=$(date "+%Y-%m-%d")
getip=$(curl http://myip.dnsomatic.com --silent)
address="sulamiification@gmail.com"

echo $getip | mutt -s "IP $datum" $address > /dev/null

