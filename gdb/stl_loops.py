# Given a vector and a field print the field value for
# all the instances of the vector.

# TBD - Do this dynamically and not according to a specific
# gcc version.

# Other TBD:
#   - [ ] Add -table option to pvectorfields to print use prettytable
#         for outputfrom optparse import OptionParser
#   - [ ] Add a pmap command to print an entry of a map.
#   - [ ] Generalize and separate creation of list of values from their
#         printing.
#   - [ ] Support printing of map elements in a similar way.

from libstdcxx.v6 import printers 
import re,sys,imp
import shlex
from optparse import OptionParser

def var_to_value(var):
  '''Looks up a symbol in either the current frame or in this'''
  typename = 'Foo'  # Is this used??
  sym,in_this = gdb.lookup_symbol(var)
  if sym is not None:
    val = sym.value(gdb.selected_frame ())
  elif in_this:
    val = gdb.lookup_symbol('this')[0].value(gdb.selected_frame())[var]
  else:
    raise Exception('Variable not found!')
  return val

def print_key_vals(keyvals):
  for key,val in keyvals:
    print(key+':'+ val)
  
def add_key_val(keyvals, key, val, filter=None):
  '''Generic add the key and the value to the keyval list and return it'''
  if val.type.tag is None:
    keyvals += [(str(key),str(val))]
  else:
    if filter is not None and len(filter):
      fields = filter
    else:
      fields = [v.name for v in val.type.fields()]
    keyvals += [(str(key),', '.join(field+' = '+str(val[field]) for field in fields))]
    
  return keyvals
  
def vec_to_keyvals(val, fields_filter=None, selector=None):
  '''Turn a vector into a list of keyvals'''
  p = printers.StdVectorPrinter('dummy', val)
  keyvals = []
  for key,val in p.children():
    if selector is not None:
      k = str(key).replace('[','').replace(']','')
      if not k in selector:
        continue
    keyvals = add_key_val(keyvals, key, val, fields_filter)
  return keyvals

def map_to_keyvals(val, fields_filter=None, selector=None):
  '''Turn a vector into a list of keyvals'''
  p = printers.StdMapPrinter('dummy', val)
  children = p.children()
  keyvals = []
  while 1:
    try:
      key = children.__next__()[1]
      val = children.__next__()[1]
      if selector is not None and str(key) not in selector:
        continue
      keyvals = add_key_val(keyvals,key,val,filter=fields_filter)
    except StopIteration:
      break

  return keyvals

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
    code = 'def pvf__(_,_idx=0) :\n  return ' + code

    # Compile it into the current module
#    module = imp.new_module('myfunctions')
    module = sys.modules[__name__]
    exec(code, module.__dict__)

    val = var_to_value(var)

    p = printers.StdVectorPrinter('dummy', val)
    for ch in p.children():
      ret = module.pvf__(ch[1],ch[0])
      if isinstance(ret,tuple):
        print(' '.join([str(v) for v in ret]))
      elif ret is not None:
        print(ret)

class PyPrintVectorFields (gdb.Command):
  "A command for for printing the field of a struct of vectors in python"

  def __init__ (self):
    super(PyPrintVectorFields, self).__init__ (
      "pvec",
      gdb.COMMAND_SUPPORT,
      gdb.COMPLETE_NONE, True)

  def invoke(self, arg, from_tty):
    args = shlex.split(arg)
    var,fields_filter = args[0],args[1:]
    m = re.match(r'(\w+)\[(.*?)\]',var)
    selector = None
    if m:
      var = m.group(1)
      selector = m.group(2)

    val = var_to_value(var)

    p = printers.StdVectorPrinter('dummy', val)
    keyvals = []
    for key,val in p.children():
      if selector is not None:
        k = str(key).replace('[','').replace(']','')
        if not k in selector:
          continue
      keyvals = add_key_val(keyvals, key, val, fields_filter)
    print_key_vals(keyvals)

class PyMapPrint (gdb.Command):
  "A general command for for looping over the field of a struct of vectors in python"

  def __init__ (self):
    super(PyMapPrint, self).__init__ (
      "pmap",
      gdb.COMMAND_SUPPORT,
      gdb.COMPLETE_NONE, True)

  def invoke(self, arg, from_tty):
    args = shlex.split(arg)
    var,fields_filter = args[0],args[1:]
    m = re.match(r'(\w+)\[(.*?)\]',var)
    selector = None
    if m:
      var = m.group(1)
      selector = m.group(2)

    val = var_to_value(var)

    p = printers.StdMapPrinter('dummy', val)
    children = p.children()
    keyvals = []
    while 1:
      try:
        key = children.__next__()[1]
        val = children.__next__()[1]
        if selector is not None and str(key) not in selector:
          continue
        keyvals = add_key_val(keyvals,key,val,filter=fields_filter)
      except StopIteration:
        break
    print_key_vals(keyvals)

class PyPrint (gdb.Command):
  "A general command for for looping over the field of a struct of vectors in python"

  def __init__ (self):
    super(PyPrint, self).__init__ (
      "pp",
      gdb.COMMAND_SUPPORT,
      gdb.COMPLETE_NONE, True)

  def invoke(self, arg, from_tty):
    # Get the gdb.Value() to check its type
    args = shlex.split(arg)
    var,fields_filter = args[0],args[1:]
    m = re.match(r'(\w+)\[(.*?)\]',var)
    selector = None
    if m:
      var = m.group(1)
      selector = m.group(2)

    var = args[0]

    # Subscripts are used for map values
    m = re.match(r'(\w+)\[(.*?)\]',var)
    if m:
      var = m.group(1)

    val = var_to_value(var)
    type_string = str(val.type)
    type_prefix= type_string
    if '<' in type_string:
      type_prefix = type_prefix.split('<')[0]

    # Dispatch to the rest of the printers
    if type_string.startswith('std::map'):
      keyvals = map_to_keyvals(val, fields_filter=fields_filter, selector=selector)
#      gdb.execute('pmap ' + arg)
    elif type_string.startswith('std::vector'):
      keyvals = vec_to_keyvals(val,fields_filter=fields_filter, selector=selector)
      #gdb.execute('pvec ' + arg)
    else:
      raise Exception('Sorry. No support for the type '+ type_prefix)
    print_key_vals(keyvals)

PyPrintVectorFields()
PyVectorLoop()
PyMapPrint()
PyPrint()
