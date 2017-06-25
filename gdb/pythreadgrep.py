# Match callstack for all systemthreads.
#
# TBD: Change from using gdb.execute() to using the inferior variables.

from __future__ import print_function
import gdb
import re

class colors:
  HEADER = '\x1b[37;1m'
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
        print("threadgrep - Print gdb variables via python\n"
              "\n"
              "Syntax:\n"
              "  threadgrep [-stack] [-fullstack] pattern\n"
              "\n"
              "Options:\n"
              "  -stack      Output stack lines matching the search pattern\n"
              "  -fullstack  Output full stack if the search pattern is matched for a thread")
        return

      if S_=='-stack':
        do_print_stack = True
        continue

      if S_=='-fullstack':
        do_print_full_stack = True
        continue

      print("Unknown option '%s'!"%S_)
      return

    pattern = args[argp]

    threads = sorted(gdb.inferiors()[0].threads(),
                     key = lambda v : v.global_num)

    for th in threads:
      th.switch()
      thread_id = th.global_num
      thread_name = th.num
                   
      where_string = gdb.execute('where',False,True)
      if re.search(pattern, where_string):
        print(colors.HEADER + "Match in thread #%d (%s)"%(thread_id, thread_name) + colors.ENDC)
        if do_print_stack:
          for where_line in where_string.split("\n"):
            if re.search(pattern,where_line):
              where_line = re.sub('('+pattern+')', colors.BLUE + r"\1" + colors.ENDC,where_line)
              print(where_line)
          print("")
        if do_print_full_stack:
          where_string = re.sub('('+pattern+')', colors.BLUE + r"\1" + colors.ENDC,where_string)
          print(where_string)

PyThreadGrep()
