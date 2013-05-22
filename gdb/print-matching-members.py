class PyPrintMatchingMembers (gdb.Command):
  "A command for printing matching members of a struct/class"

  def __init__ (self):
    super(PyPrintMatchingMembers, self).__init__ ("ppm",
                                                  gdb.COMMAND_SUPPORT,
                                                  gdb.COMPLETE_NONE, True)

  def invoke(self, arg, from_tty):
    args = arg.split(' ')
    argp = 0
    do_keys = False
    do_compact = False
    do_list = False
    while args[argp][0]=='-':
      S_ = args[argp]
      argp+=1
      if S_=='-help':
        print "ppm - Print matching members of a class"
        print ""
        print "Syntax:"
        print "  ppm var pattern"
        print ""
        print "Options:"
        print "  -keys  Only output keys"
        print "  -c     Compact output"
        return
      if S_=='-keys':
        do_keys = True
        continue
      if S_=='-list':
        do_list = True
        continue
      if S_=='-c':
        do_compact = True
        continue
      print "Unknown option '%s'!"%S_
      return

    try:
      var = args[argp]
      argp+=1
      if argp < len(args):
        pattern = args[argp]
        argp+=1
      else:
        pattern = '.*'
      val = gdb.execute("ptype %s"%var,True,True)
      for g in re.finditer(r'(\w+);',val):
        v = g.group(1)
        if v=='const':
          continue
        if not re.search(pattern,v):
          continue
        if do_list:
          print v
          continue
        local_var = g.group(1)
        # This can be speeded up by not calling a python function.
        val = gdb.execute("pyp -c %s->%s"%(var,local_var),True,True),
        if do_compact:
          print val,
        else:
          print "%s=%s"%(local_var,val[0]),
    except ValueError:
      print "%s not found!"%v

PyPrintMatchingMembers()
