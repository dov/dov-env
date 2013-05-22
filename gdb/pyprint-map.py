class PyMapPrintCommand (gdb.Command):
  "A command for printing stl maps"

  def __init__ (self):
    super(PyMapPrintCommand, self).__init__ ("pmap",
                                             gdb.COMMAND_SUPPORT,
                                             gdb.COMPLETE_NONE, True)

  def invoke(self, arg, from_tty):
    args = arg.split(' ')
    argp = 0
    do_keys = False
    do_compact = False
    while args[argp][0]=='-':
      S_ = args[argp]
      argp+=1
      if S_=='-help':
        print "pyp-map - Print gdb stl variables"
        print ""
        print "Syntax:"
        print "  pmap [-keys] map"
        print ""
        print "Options:"
        print "  -keys  Only output keys"
        print "  -c     Compact output"
        return
      if S_=='-keys':
        do_keys = True
        continue
      if S_=='-c':
        do_compact = True
        continue
      print "Unknown option '%s'!"%S_
      return

    try:
      v = args[argp]
      val = gdb.execute("p %s"%v,True,True)
      val = re.search(r"\{(.*)\}",val).group(1)
      keyvals = val.split(',')
      for kv in keyvals:
        key,val = kv.split('=')
        try:
          key = re.search(r'\[\"?(.*?)\"?\]', key).group(1)
        except:
          pass

        if do_keys:
          print key,
        else:
          print key,':',val,
        if not do_compact:
          print ""
      if do_compact:
        print ""
    except ValueError:
      print "%s not found!"%v

PyMapPrintCommand()
