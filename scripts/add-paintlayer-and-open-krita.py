#!/usr/bin/python

# Convert an image into an image with overlay
from pyora import Project, TYPE_LAYER
from PIL import Image
import sys, os
import argparse
from pathlib import Path
import subprocess

def xec(cmd, decode=True, chomp=True, verbose=False):
  '''Run a command a returns its stdout output.

  decode -- run (utf8) decode of the resulting string
  chomp -- get rid of the last newline (like in perl)
  '''
  with subprocess.Popen(cmd,shell=True,stdout=subprocess.PIPE) as ph:
    if verbose:
      sys.stderr.write(cmd + '\n')
    res = b''
    maxsize = 1024
    while True:
      buf = ph.stdout.read()
      if len(buf)==0:
        break
      res += buf
    ph.wait()
  if decode:
    res=res.decode()
  if chomp:
    res = res[:-1]
  return res

parser = argparse.ArgumentParser(description='Process a file')
parser.add_argument('filename', nargs=1, help='File input')
args = parser.parse_args()

# make a new ORA project, numbers are canvas dimensions

# adding a layer
filename = Path(args.filename[0])
print(f'{filename=}')
img = Image.open(filename)
width,height = img.size
print(f'{width=} {height=}')
project = Project.new(width, height)
project.add_layer(img, 'Background')

# create an empty image
paint_layer = Image.new('RGBA', (width, height), (0, 0, 0, 0))

# adding a layer at any arbitrary path will automatically create the appropriate layer groups
project.add_layer(paint_layer, 'Paint')

# save current project as ORA file
ora_filename = Path('/tmp') / filename.with_suffix('.ora').name.replace('.ora','-paint.ora')
project.save(ora_filename)
xec(f'krita {ora_filename}')
xec('FvwmCommand \'All ("krita") Iconify false\'')
xec('FvwmCommand \'Next ("krita") SelectWindow\'')

print('ok')
