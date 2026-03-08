#!/usr/bin/env python

import argparse
from PIL import Image
import os

def SplitAndSaveQuadrants(FileName):
  BaseName, Ext = os.path.splitext(FileName)
  ImageFile = Image.open(FileName)
  Width, Height = ImageFile.size

  MidX = Width // 2
  MidY = Height // 2

  Quadrants = {
    '-00': (0, 0, MidX, MidY),         # Top-left
    '-01': (MidX, 0, Width, MidY),     # Top-right
    '-10': (0, MidY, MidX, Height),    # Bottom-left
    '-11': (MidX, MidY, Width, Height) # Bottom-right
  }

  for Suffix, Box in Quadrants.items():
    QuadrantImage = ImageFile.crop(Box)
    OutputFileName = f"{BaseName}{Suffix}.png"
    QuadrantImage.save(OutputFileName)
    print(f"Saved: {OutputFileName}")

if __name__ == "__main__":
  Parser = argparse.ArgumentParser(description="Split a PNG image into four quadrants.")
  Parser.add_argument('FileName', help="Input PNG filename")
  Args = Parser.parse_args()
  SplitAndSaveQuadrants(Args.FileName)
  
