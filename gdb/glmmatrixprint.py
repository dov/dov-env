# Print variables by python

import gdb, re
from pyparsing import nestedExpr
import pdb

class GlmMatrixPrint(gdb.Command):
  "A command for printing glm variables via python"

  def __init__ (self):
    super(GlmMatrixPrint, self).__init__ ("pglm",
                                          gdb.COMMAND_SUPPORT,
                                          gdb.COMPLETE_NONE, True)

  def invoke(self, arg, from_tty):
    args = arg.split(' ')
    argp = 0
    do_compact = False
    while args[argp][0]=='-':
      S_ = args[argp]
      argp+=1
      if S_=='--help':
        print("pglm - Print eigen matrices\n"
              "\n"
              "Syntax:\n"
              "  pglm m1\n"
              "")
        return
      if S_=='-c':
        do_compact = True
        continue
      print("Unknown option '%s'!"%S_)
      return

    for v in args[argp:]:
      try:
        # Get eigen pretty print output. This will collide with any other
        # mangling of the eigen output...
        gdb.execute("disable pretty-printer",False,True)
        val = gdb.execute("print "+v,False,True)
        val = val[val.find('=')+2:]
        gdb.execute("enable pretty-printer",False,True)

        # "Dereference" references
        if '@' in val:
          val = val[val.find('@'):]
          val = val[val.find(':')+2:]

        ne = nestedExpr('{','}').parseString(val).asList()

        # Heuristic recognize matrices
        table = []
        if len(ne)==1 and len(ne[0])==3 and ne[0][0]=='value':
          m = ne[0][2]
          row = []
          n = (len(m)+1)//2
          for idx in range(n*n):
            yidx = idx//n
            xidx = idx%n
            if m[0][0][0][3]=='y':
              v = float(m[xidx*2][0][0][yidx*3+2].replace(',',''))
            else:
              v = float(m[xidx*2][yidx*2][2][:-1])

            # Add to the table
            row += ['{:>12}'.format(f'{v:.5f}')]
            if (idx+1)%n==0:
              table += [row]
              row=[]
        elif len(ne)==1 and ne[0][0]!='value':
          m = ne[0]
          row = []
          if m[0][0][3]=='y':
            n = len(m[0][0])//3
            for i in range(n):
              v = float(m[0][0][3*i+2].replace(',',''))
              row += ['{:>8}'.format(f'{v:.5f}')]
          else:
            n = (len(m)+1)//2
            for i in range(n):
              v = float(m[i*2][2][:-1])
              row += ['{:>8}'.format(f'{v:.5f}')]
          table += [row]

        for row in table:
          print('['+' '.join(row)+']')

      except ValueError:
        print("%s not found!"%v)
        raise
    if do_compact:
      print("")

GlmMatrixPrint()
