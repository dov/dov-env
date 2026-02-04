#!/usr/bin/env bash

# Usage: ./who_listens.sh <port>
# Example: ./who_listens.sh 8080

if [ $# -ne 1 ]; then
  echo "Usage: $0 <port>"
  exit 1
fi

PORT="$1"

# Validate that PORT is a number
if ! [[ "$PORT" =~ ^[0-9]+$ ]]; then
  echo "Error: Port must be a numeric value."
  exit 1
fi

# Collect PIDs of processes listening on the port
PIDS=$(lsof -t -i :"$PORT" -sTCP:LISTEN -P -n 2>/dev/null)

if [ -z "$PIDS" ]; then
  echo "No listening process found on port $PORT."
  exit 1
fi

echo "Processes listening on port $PORT:"
echo

# Use ps -ef to show details, restricting to the found PIDs
# This keeps the exact ps -ef style output.
ps -ef | awk -v pids="$PIDS" '
BEGIN {
  n = split(pids, a, " ")
  for (i = 1; i <= n; i++) {
    want[a[i]] = 1
  }
}
NR == 1 {
  # Print header
  print
  next
}
{
  if ($2 in want) {
    print
  }
}
'
