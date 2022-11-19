class PyPrintFields(gdb.Command):
    'Look up the given python variable name, and print it'
    def __init__(self):
        gdb.Command.__init__ (self,
                              "py-fields",
                              gdb.COMMAND_DATA,
                              gdb.COMPLETE_NONE)


    def invoke(self, args, from_tty):
        name = str(args)

        frame = Frame.get_selected_python_frame()
        if not frame:
            print('Unable to locate python frame')
            return

        pyop_frame = frame.get_pyop()
        if not pyop_frame:
            print('Unable to read information on python frame')
            return

        def get_var_dict(v):
            if isinstance(v, PyInstanceObjectPtr):
                return v.pyop_field('in_dict')
            elif isinstance(v, HeapTypeObjectPtr):
                return v.get_attr_dict()
            else:
                return {}
        parent = None
        fields = name.split('.')
        for name in fields:
            if parent is None:
                pyop_var, scope = pyop_frame.get_var_by_name(name)
            else:
                for k, pyop_var in get_var_dict(parent).iteritems():
                    if str(k) == name:
                        break
                else:
                    print("Can't find field %s of %s" % (name, parent))
                    return
            parent = pyop_var
        if pyop_var:
            print ('%s %r = %s'
                   % (scope,
                      name,
                      pyop_var.get_truncated_repr(MAX_OUTPUT_LEN)))
        else:
            print('%r not found' % name)

PyPrintFields()
