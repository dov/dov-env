# Print variables by python

import gdb, re
import numpy as np
from pyparsing import nestedExpr
import pdb

class VsgMatrixPrint(gdb.Command):
  "A command for printing vsg variables via python"

  def __init__ (self):
    super(VsgMatrixPrint, self).__init__ ("pvsg",
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
        print("pvsg - Print vsg matrices\n"
              "\n"
              "Syntax:\n"
              "  pvsg m1\n"
              "")
        return
      if S_=='-c':
        do_compact = True
        continue
      print("Unknown option '%s'!"%S_)
      return

    pattern3 = re.compile(r'\{x = ([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?), y = ([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?), z = ([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?)')
    pattern4 = re.compile(r'\{x = ([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?), y = ([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?), z = ([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?), w = ([-+]?\d*\.?\d+(?:[eE][-+]?\d+)?)\}')

    for v in args[argp:]:
      try:
        typename = gdb.parse_and_eval(v).type.name or gdb.parse_and_eval(v).type.target().name
        if typename is None:
          raise RuntimeError('Unknown expression: ' + v)
        gdb.execute("disable pretty-printer",False,True)
        val = gdb.execute("print "+v,False,True)
        val = val[val.find('=')+2:]
        gdb.execute("enable pretty-printer",False,True)

        if typename in ['vsg::dvec3','vsg::vec3', 'vsg::t_vec3<double>','vsg::t_vec3<float>','vsg::t_box<double>::vec_type','vsg::t_box<float>::vec_type']:
            matches = pattern3.findall(val)
            array = [float(w) for w in matches[0]]
        elif typename in ['vsg::mat4', 'vsg::dmat4','vsg::t_mat4<double>','vsg::t_mat4<float>']:
            #Find all matches
            matches = pattern4.findall(val)

            array = [[float(w) for w in m] for m in matches]
        elif typename in ['vsg::dbox', 'vsg::box','vsg::t_box<double>','vsg::t_box<float>']:
          matches = list(pattern3.findall(val))
          array = [[float(w) for w in m] for m in matches]
        else:
            pdb.set_trace()
            raise RuntimeError('Unrecognized expression!')
        
        print(np.array2string(np.array(array), formatter={'float_kind': lambda x: f"{x:.3f}"}))
        
      except ValueError:
        print("%s not found!"%v)
        raise
    if do_compact:
      print("")

VsgMatrixPrint()
