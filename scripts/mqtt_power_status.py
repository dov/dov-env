#!/usr/bin/env python

######################################################################
#  A script for reading and controlling the state of a tasmota
#  based mqtt device from the command line.
#
#  2025-09-08 Mon
#  Dov Grobgeld <dov.grobgeld@gmail.com>
######################################################################
import argparse
import sys
import time
import threading

import paho.mqtt.client as mqtt

def main():
  parser = argparse.ArgumentParser(description="Query status/power from a remote MQTT device.")
  parser.add_argument('--host', help="MQTT broker hostname or IP", default="localhost")
  parser.add_argument('--mqtt-id', required=True, help="MQTT device/module id")
  parser.add_argument('--timeout', type=float, default=1.0, help="Timeout in seconds for response")
  parser.add_argument('--port', type=int, default=1883, help="MQTT broker port (default: 1883)")
  parser.add_argument('--user', help="MQTT username (optional)")
  parser.add_argument('--password', help="MQTT password (optional)")
  parser.add_argument('--on', action='store_true', help="Turn on the device")
  parser.add_argument('--off', action='store_true', help="Turn off the device")
  args = parser.parse_args()

  status_topic = f"stat/{args.mqtt_id}/POWER"
  cmnd_topic = f"cmnd/{args.mqtt_id}/POWER"

  result_payload = {}
  if args.on:
    payload = 'ON'
  elif args.off:
    payload = 'OFF'
  else:
    payload=''

  got_result = threading.Event()

  def on_connect(client, userdata, flags, rc):
    if rc != 0:
      print(f"Failed to connect to MQTT broker with code {rc}", file=sys.stderr)
      sys.exit(1)
    client.subscribe(status_topic)

  def on_message(client, userdata, msg):
    if msg.topic == status_topic:
      result_payload['payload'] = msg.payload.decode('utf-8')
      got_result.set()

  client = mqtt.Client()
  if args.user:
    client.username_pw_set(args.user, args.password)
  client.on_connect = on_connect
  client.on_message = on_message

  try:
    client.connect(args.host, args.port, 60)
  except Exception as e:
    print(f"Could not connect to MQTT broker: {e}", file=sys.stderr)
    sys.exit(2)

  client.loop_start()
  # Wait for connection and subscription
  time.sleep(0.1)

  # Send command to query power status
  client.publish(cmnd_topic, payload=payload)

  if not got_result.wait(timeout=args.timeout):
    print("Error: Timeout waiting for power status.", file=sys.stderr)
    client.loop_stop()
    sys.exit(3)

  print(result_payload['payload'])
  client.loop_stop()

if __name__ == "__main__":
  main()
  
