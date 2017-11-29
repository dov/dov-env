# Given a vector and a field print the field value for
# all the instances of the vector.

# TBD - Do this dynamically and not according to a specific
# gcc version.
from libstdcxx.v6 import printers 
import re,sys,imp

class PyPrintVectorFields (gdb.Command):
  "A command for for printing the field of a struct of vectors in python"

  def __init__ (self):
    super(PyPrintVectorFields, self).__init__ (
      "pvectorfields",
      gdb.COMMAND_SUPPORT,
      gdb.COMPLETE_NONE, True)

  def invoke(self, arg, from_tty):
    args = arg.split(' ')
    var,fields = args[0],args[1:]

    typename = 'std::vector'
    val = gdb.lookup_symbol(var)[0].value(gdb.selected_frame ())
    p = printers.StdVectorPrinter(typename, val)
    for ch in p.children():
      print(ch[0]+':'+ ' '
            + ', '.join(field+'='+str(ch[1][field]) for field in fields))

class PyVectorLoop (gdb.Command):
  "A general command for for looping over the field of a struct of vectors in python"

  def __init__ (self):
    super(PyVectorLoop, self).__init__ (
      "pvectorloop",
      gdb.COMMAND_SUPPORT,
      gdb.COMPLETE_NONE, True)

  def invoke(self, arg, from_tty):
    var = arg.split(' ')[0]
    m = re.match(r'^\s*\S+\b\s*(.*)',arg)
    code = m.group(1)

    # Special case recognizing _.\w+ and turning it into
    # _[\w+]
    code = re.sub(r'_\.(\w+)','(_[\'\\1\'])',code)

    # Turn the code into a function definition
    code = 'def pvf(_,_idx=0) :\n  return ' + code

    # Compile it into a moule
    module = imp.new_module('myfunctions')
    exec(code, module.__dict__)
    
    typename = 'std::vector'
    val = gdb.lookup_symbol(var)[0].value(gdb.selected_frame ())
    p = printers.StdVectorPrinter(typename, val)
    for ch in p.children():
      ret = module.pvf(ch[1],ch[0])
      if isinstance(ret,tuple):
        print(' '.join([str(v) for v in ret]))
      elif ret is not None:
        print(ret)

PyPrintVectorFields()
PyVectorLoop()

