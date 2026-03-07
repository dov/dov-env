// g++ fix_krita.cpp -o fix_krita -lX11
// cp fix_krita ../bin

#include <X11/Xlib.h>
#include <X11/Xatom.h>
#include <X11/Xutil.h>
#include <iostream>

int main(int argc, char* argv[])
{
  if (argc < 2) return 1;

  Display* DisplayPtr = XOpenDisplay(NULL);
  if (!DisplayPtr) return 1;

  Window TargetWindow = strtol(argv[1], NULL, 0);

  // 1. Force the EWMH Type (The modern way)
  Atom TypeAtom = XInternAtom(DisplayPtr, "_NET_WM_WINDOW_TYPE", False);
  Atom NormalAtom = XInternAtom(DisplayPtr, "_NET_WM_WINDOW_TYPE_NORMAL", False);
  XChangeProperty(DisplayPtr, TargetWindow, TypeAtom, XA_ATOM, 32, PropModeReplace, (unsigned char *)&NormalAtom, 1);

  // 2. Force the Legacy WM_CLASS (The old way)
  XClassHint* ClassHint = XAllocClassHint();
  ClassHint->res_name = (char*)"krita";
  ClassHint->res_class = (char*)"Krita";
  XSetClassHint(DisplayPtr, TargetWindow, ClassHint);
  XFree(ClassHint);

  // 3. Force the Window Name
  XStoreName(DisplayPtr, TargetWindow, "Krita");

  XFlush(DisplayPtr);
  XCloseDisplay(DisplayPtr);
  
  return 0;
}
