#!/bin/sh
# start usb networking to the n900
/sbin/ip address add 192.168.2.14/24 dev usb0
/sbin/ip link set dev usb0 up
/sbin/route add -host 192.168.2.15 dev usb0
