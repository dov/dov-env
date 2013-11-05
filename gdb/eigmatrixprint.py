# Print variables by python

import gdb,re
class EigMatrixPrint (gdb.Command):
  "A command for printing variables via python"

  def __init__ (self):
    super(EigMatrixPrint, self).__init__ ("peig",
                                          gdb.COMMAND_SUPPORT,
                                          gdb.COMPLETE_NONE, True)

  def invoke(self, arg, from_tty):
    args = arg.split(' ')
    argp = 0
    do_compact = False
    while args[argp][0]=='-':
      S_ = args[argp]
      argp+=1
      if S_=='-help':
        print "peig - Print eigen matrices"
        print ""
        print "Syntax:"
        print "  peig m1"
        print ""
        return
      if S_=='-c':
        do_compact = True
        continue
      print "Unknown option '%s'!"%S_
      return

    for v in args[argp:]:
      try:
        # Get eigen pretty print output
        val = "%s=%s"%(v,gdb.parse_and_eval(v))
        # Get width and height
        m = re.search(r'Eigen::Matrix<double,(\d+),(\d+),ColMajor>', val)
        if m:
          nrows,ncols = [int(m.group(1)), int(m.group(2))]
          mat = [[' ' for i in range(ncols)] for j in range(nrows)]
          for row in range(nrows):
            for col in range(ncols):
              m = re.search(r'\[%d,%d\] = ([\d.e\+-]+)'%(row,col), val)
              mat[row][col] = '%8.5g'%float(m.group(1))
          for m in mat:
            print '    [',','.join(m),']'
        else:
          print val
        
      except ValueError:
        print "%s not found!"%v
        raise
    if do_compact:
      print ""

EigMatrixPrint()
