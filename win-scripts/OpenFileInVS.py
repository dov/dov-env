"""
Open a file in visual studio, optionally on the requested linenumber.

This should be replaced with the more complete solution:
  - http://badbyteblues.blogspot.com/2007/08/automating-developer-studio.html

Dov Grobgeld <dov.grobgeld@gmail.com>
Wednesday 2012-02-22 17:10 
"""

import win32com.client
import sys
import os

if len(sys.argv)<2:
  print "Need filename!"
  exit(-1)

Filename = sys.argv[1]
if not os.path.exists(Filename):
  print "File does not exist!!"
  exit(-1)
  
LineNum = None
ColNum = 1
if len(sys.argv)>2:
  LineNum = int(sys.argv[2])
if len(sys.argv)>3:
  ColNum = int(sys.argv[3])+1
  
b = win32com.client.Dispatch('VisualStudio.DTE')
b.ItemOperations.OpenFile(os.path.abspath(Filename))
if not LineNum is None:
  s = b.ActiveDocument.Selection
  s.MoveToLineAndOffset(LineNum,ColNum)
