#!/bin/bash

# Usage: ./launch_view_stl_with_reload.sh /path/to/model.stl

if [ "$#" -ne 1 ]; then
  echo "Usage: $0 /path/to/model.stl"
  exit 1
fi

STLFILE="$1"

TMP_SCRIPT=$(mktemp /tmp/view_stl_with_reload_XXXX.py)

cat > "$TMP_SCRIPT" <<EOF
import bpy
import os

# -------- User Settings --------
STL_FILEPATH = r"""$STLFILE"""
CHECK_INTERVAL = 1.0  # seconds
# -------------------------------

def remove_all_mesh_objects():
  bpy.ops.object.select_all(action='SELECT')
  bpy.ops.object.delete(use_global=False)

def import_stl(filepath):
  bpy.ops.wm.stl_import(filepath=filepath)

def reload_stl_if_changed(state):
  stl_path = STL_FILEPATH
  if not os.path.isfile(stl_path):
    print(f"STL file does not exist: {stl_path}")
    return CHECK_INTERVAL

  mtime = os.path.getmtime(stl_path)
  if state['mtime'] != mtime:
    print(f"Reloading STL: {stl_path}")
    remove_all_mesh_objects()
    import_stl(stl_path)
    state['mtime'] = mtime
  return CHECK_INTERVAL

def start_watchdog():
  state = { 'mtime': None }
  # Initial load
  if os.path.isfile(STL_FILEPATH):
    state['mtime'] = os.path.getmtime(STL_FILEPATH)
    remove_all_mesh_objects()
    import_stl(STL_FILEPATH)
  else:
    print(f"Waiting for STL: {STL_FILEPATH}")

  def _watch():
    next_time = reload_stl_if_changed(state)
    bpy.app.timers.register(_watch, first_interval=next_time)
  bpy.app.timers.register(_watch, first_interval=CHECK_INTERVAL)

if __name__ == "__main__":
  start_watchdog()
EOF

blender --python "$TMP_SCRIPT"
rm -f "$TMP_SCRIPT"
