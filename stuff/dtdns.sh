#!/bin/sh

getip=$(curl http://myip.dtdns.net --silent)
path="/tmp/dtdns"
domain="a123qwertz567.dtdns.net"
pass="bla"

if [[ -f $path ]]; then
    old=$(cat $path)
else
    old="(None)"
fi

if [ "$getip" == "$old" ]; then
    echo "IP unchanged: $old <> $getip"
else
    echo -e "GET /api/autodns.cfm?id=$domain&pw=$pass HTTP/1.1\n\
Host: www.dtdns.com\n\
User-Agent: bash\n\n" |
    nc www.dtdns.com 80 > /dev/null
    echo "$getip" > $path
    echo "IP updated: $old -> $getip"
fi

