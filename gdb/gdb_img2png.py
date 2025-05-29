# A gdb command that saves a shared_ptr<CImg> to the fixed file /tmp/gdb-img.png
# This allows using giv --auto-reload to view images processed in he debugger.
#
# 2025-05-08 Thu
# Dov Grobgeld 

import gdb
import pdb

class Img2PngCommand(gdb.Command):
    """Save a shared_ptr<CImg> to a PNG file using the SavePng method."""

    def __init__(self):
        super(Img2PngCommand, self).__init__("img2png", gdb.COMMAND_USER)

    def invoke(self, arg, from_tty):
        # Parse the argument (the shared_ptr<CImg>)
        if not arg:
            print("Usage: img2png <shared_ptr<CImg>>")
            return

        # Evaluate the argument to get the shared_ptr<CImg>
        try:
            shared_ptr = gdb.parse_and_eval(arg)
        except gdb.error as e:
            print(f"Error evaluating argument: {e}")
            return

        # Dereference the shared_ptr to get the CImg object
        try:
            cimg_ptr = shared_ptr["_M_ptr"]
        except gdb.error as e:
            print(f"Error accessing _M_ptr of shared_ptr: {e}")
            return

        # Call the SavePng method on the CImg object
        try:
            gdb.execute(f'call ImgSavePng(' + arg + ', "/tmp/gdb-img.png")')
            print("Image saved to /tmp/gdb-img.png")
        except gdb.error as e:
            print(f"Error calling SavePng: {e}")

# Register the command with GDB
Img2PngCommand()
