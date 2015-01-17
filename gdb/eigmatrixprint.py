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
        # Get eigen pretty print output. This will collide with any other
        # mangling of the eigen output...
        val = gdb.execute("print "+v,False,True)
        # Get width and height
        m = re.search(r'Eigen::Matrix<(double|float), (\d+), (\d+), 0, \d+, \d+>.*?array\s*=\s*\{(.*?)\}', val,flags=re.DOTALL)
        # TBD check for column major!
        if m:
          nrows,ncols = [int(m.group(2)), int(m.group(3))]
          array = [float(s) for s in re.split(r',\s*',m.group(4))]
          mat = [[' ' for i in range(ncols)] for j in range(nrows)]
          for row in range(nrows):
            for col in range(ncols):
              mat[row][col] = '%12.5g'%float(array[col*nrows+row])
          for m in mat:
            print '    |'+ ''.join(m)+'|'
        else:
          print 'No match found!'
          print val
        
      except ValueError:
        print "%s not found!"%v
        raise
    if do_compact:
      print ""

EigMatrixPrint()
