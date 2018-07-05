# A command for running "finish" from C++ and get back into python
# when in a XJet python wrapper context.
#
# Dov Grobgeld
# 2016-09-25 Sun

# Print variables by python

from __future__ import print_function
import gdb
import re

class PyXJetFinish (gdb.Command):
  "A command for getting from C++ back to python"

  def __init__ (self):
    super(PyXJetFinish, self).__init__ ("xjpyf",
                                        gdb.COMMAND_SUPPORT,
                                        gdb.COMPLETE_NONE, True)

  def invoke(self, arg, from_tty):
    args = arg.split(' ')
    argp = 0
    while len(args) and len(args[argp]) and args[argp][0]=='-':
      S_ = args[argp]
      argp+=1
      if S_=='-help':
        print ("xjpyf - From C++ to python\n"
               )
        return
      print("Unknown option '%s'!"%S_)
      return

    where_string = gdb.execute('where',False,True)
    lines = where_string.split("\n")

    n = len(lines)-2  # Get rid of last empty line
    
    print('lines[n]=',lines[n])
    assert re.match('0x0+$',lines[n].split()[1]),'Last line should be 0x0000'

    # Now find first place that does not have () args.
    k = n-1
    while k>=0 and lines[k].split()[3]=='()':
      k-=1

    assert(k>=0 and lines[k].split()[3]!='()')

#    # Count frames through python. This is not used yet!
#    num_frames = 1
#    fr = gdb.newest_frame()
#    while fr.older():
#      num_frames += 1
#      fr = fr.older()
#    print('num_frames = %d'%num_frames)

    # Select this last frame and finish from it
    for Cmd in ['select-frame %d'%k,
                'finish',
                'stepi 3']:
      gdb.execute(Cmd,from_tty=False,to_string=True)

    # Present the python stack to the user!
    print('<<--------- Python stack ----------->>')
    print(gdb.execute('py-bt',False,True),end='')

PyXJetFinish()

