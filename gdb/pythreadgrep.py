# Match callstack for all systemthreads.

import gdb
import re

class colors:
  HEADER = '\033[01;30m'
  BLUE = '\033[01;34m'
  ENDC = '\033[0m'

class PyThreadGrep (gdb.Command):
  "A command for grepping threads for frame info in python"

  def __init__ (self):
    super(PyThreadGrep, self).__init__ ("threadgrep",
                                        gdb.COMMAND_SUPPORT,
                                        gdb.COMPLETE_NONE, True)

  def invoke(self, arg, from_tty):
    args = arg.split(' ')
    argp = 0
    do_print_stack = False
    do_print_full_stack = False
    while len(args[argp])>0 and args[argp][0]=='-':
      S_ = args[argp]
      argp+=1
      if S_=='-help':
        print "threadgrep - Print gdb variables via python"
        print ""
        print "Syntax:"
        print "  threadgrep [-stack] [-fullstack] pattern"
        print ""
        print "Options:"
        print "  -stack      Output stack lines matching the search pattern"
        print "  -fullstack  Output full stack if the search pattern is matched for a thread"
        return

      if S_=='-stack':
        do_print_stack = True
        continue

      if S_=='-fullstack':
        do_print_full_stack = True
        continue

      print "Unknown option '%s'!"%S_
      return

    pattern = args[argp]

    # Get list of threads
    threads_string = gdb.execute('info threads',False,True)
    thread_ids = []
    for thread_string in threads_string.split('\n'):
      m = re.match(r'^\*?\s+(\d+)',thread_string)
      if m:
        thread_ids += [int(m.group(1))]
    thread_ids.sort()

    # Loop over all threads
    for thread_id in thread_ids:
      gdb.execute("thread %d"%thread_id, False, True)
      where_string = gdb.execute('where',False,True)
      if re.search(pattern, where_string):
        print colors.HEADER + "Match in thread #%d"%thread_id + colors.ENDC
        if do_print_stack:
          for where_line in where_string.split("\n"):
            if re.search(pattern,where_line):
              where_line = re.sub('('+pattern+')', colors.BLUE + r"\1" + colors.ENDC,where_line)
              print where_line
          print ""
        if do_print_full_stack:
          where_string = re.sub('('+pattern+')', colors.BLUE + r"\1" + colors.ENDC,where_string)
          print where_string

PyThreadGrep()
