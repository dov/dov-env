#!/usr/bin/env python

import argparse
import sys
import urllib.request

PRINTER_IP = "192.168.1.47"
SNAPSHOT_URL = f"http://{PRINTER_IP}:8080/?action=snapshot"

parser = argparse.ArgumentParser(description="Grab a JPEG snapshot from the Creality printer camera.")
parser.add_argument("output", help="Output file path (e.g. snapshot.jpg)")
args = parser.parse_args()

try:
  with urllib.request.urlopen(SNAPSHOT_URL, timeout=5) as resp:
    data = resp.read()
  with open(args.output, "wb") as f:
    f.write(data)
  print(f"Saved {len(data)//1024}KB snapshot to {args.output}")
except Exception as e:
  print(f"Error: {e}", file=sys.stderr)
  sys.exit(1)
