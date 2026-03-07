#!/usr/bin/bash

# There is an x11 window incompatibility with krita. This script together
# with the fix_krita program fixes this.
#
# Run this before sharing krita e.g. with teams

for id in $(xwininfo -tree -root | grep -i "krita" | awk '{print $1}'); do
  fix_krita $id
done

echo 'Krita may now be shared in teams, etc'
