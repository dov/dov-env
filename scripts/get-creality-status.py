#!/usr/bin/env python

import sys
import json
import websocket

PRINTER_IP = "192.168.1.47"

STATE_NAMES = {0: "idle", 1: "printing", 2: "paused", 3: "complete", 4: "error"}

def fmt_time(seconds):
  seconds = int(seconds)
  h, m = divmod(seconds, 3600)
  m, s = divmod(m, 60)
  if h:
    return f"{h}h{m:02d}m"
  return f"{m}m{s:02d}s"

def on_message(ws, msg):
  d = json.loads(msg)

  nozzle   = float(d.get("nozzleTemp", 0))
  t_nozzle = float(d.get("targetNozzleTemp", 0))
  bed      = float(d.get("bedTemp0", 0))
  t_bed    = float(d.get("targetBedTemp0", 0))
  progress = int(d.get("printProgress", 0))
  layer    = d.get("layer", 0)
  total    = d.get("TotalLayer", 0)
  left     = d.get("printLeftTime", 0)
  state    = STATE_NAMES.get(d.get("state", 0), "unknown")
  filename = d.get("printFileName", "")
  if filename:
    filename = filename.split("/")[-1]

  print(f"State:    {state}")
  print(f"Nozzle:   {nozzle:.1f}°C → {t_nozzle:.0f}°C")
  print(f"Bed:      {bed:.1f}°C → {t_bed:.0f}°C")
  if state == "printing":
    print(f"Progress: {progress}%  (layer {layer}/{total})")
    if left:
      print(f"Time left: {fmt_time(left)}")
    if filename:
      print(f"File:     {filename}")

  ws.close()

def on_error(ws, err):
  print(f"Error: {err}", file=sys.stderr)

ws = websocket.WebSocketApp(
  f"ws://{PRINTER_IP}:9999",
  on_message=on_message,
  on_error=on_error,
)
try:
  ws.run_forever(ping_timeout=5)
except KeyboardInterrupt:
  pass
