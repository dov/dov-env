#!/usr/bin/python

# Convert an image into an image with overlay
from pyora import Project, TYPE_LAYER
from PIL import Image
import sys, os
import argparse
from pathlib import Path

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
project.add_layer(img, 'Jackie')

# create an empty image
paint_layer = Image.new('RGBA', (width, height), (0, 0, 0, 0))

# adding a layer at any arbitrary path will automatically create the appropriate layer groups
project.add_layer(paint_layer, 'Paint')

# save current project as ORA file
ora_filename = Path('/tmp') / filename.with_suffix('.ora').name.replace('.ora','-paint.ora')
project.save(ora_filename)
os.system(f'krita {ora_filename}')
print('ok')
