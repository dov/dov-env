"""
Open a file in visual studio, optionally on the requested linenumber.

This should be replaced with the more complete solution:
  - http://badbyteblues.blogspot.com/2007/08/automating-developer-studio.html

May access any Visual Studio command that is listed in Tools/Customize, e.g.
b.ExecuteCommand("IncrediBuild.BuildSolution")

Dov Grobgeld <dov.grobgeld@gmail.com>
Wednesday 2012-02-22 17:10 
"""

import win32com.client
import win32gui
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

  # set breakpoint add current line
#  b.ExecuteCommand("Debug.ToggleBreakpoint")
  b.Debugger.BreakPoints.Add("",os.path.abspath(Filename),LineNum)


# Raise Visual Studio to top
w = b.MainWindow
h = w.HWnd

win32gui.SetForegroundWindow(h)
win32gui.ShowWindow(h,5)
