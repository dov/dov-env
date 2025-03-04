######################################################################
#  A command for formatted variable output in gdb
#
#  Written by github copilot.
#
#  2025-03-04 Tue
#  Dov Grobgeld <dov.grobgeld@gmail.com>
######################################################################

import gdb

# Global variable to store the precision
precision = 6

class SetPrecision(gdb.Command):
    """Set the precision for floating-point numbers."""

    def __init__(self):
        super(SetPrecision, self).__init__("set_precision", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        global precision
        try:
            precision = int(arg)
            print(f"Precision set to {precision} significant digits.")
        except ValueError:
            print("Invalid precision value. Please provide an integer.")

class PrintFormatted(gdb.Command):
    """Custom GDB command to print variables with their names and values."""

    def __init__(self):
        super(PrintFormatted, self).__init__("pf", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        global precision
        args = gdb.string_to_argv(arg)
        output = []
        for var in args:
            try:
                value = gdb.parse_and_eval(var)
                if value.type.code == gdb.TYPE_CODE_FLT:
                    formatted_value = f"{float(value):.{precision}g}"
                else:
                    formatted_value = str(value)
                output.append(f"{var} = {formatted_value}")
            except gdb.error as e:
                output.append(f"{var} = <error: {e}>")
        print(" ".join(output))

SetPrecision()
PrintFormatted()
