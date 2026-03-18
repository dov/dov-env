#!/usr/bin/env python

import subprocess
import pathlib
import tempfile
import os
import argparse
import sys

def RunInBlender(Script: pathlib.Path) -> None:
  AbsoluteScriptPath: pathlib.Path = Script.resolve()
  
  if not AbsoluteScriptPath.exists():
    print(f"Error: {AbsoluteScriptPath} does not exist.")
    sys.exit(1)

  ScriptDirectory: str = str(AbsoluteScriptPath.parent)
  CurrentWorkingDirectory: str = os.getcwd()

  # Create a temporary file to hold the code
  with tempfile.NamedTemporaryFile(suffix=".py", delete=False) as TempFile:
    TempFilePath: str = TempFile.name

  # The updated bootstrap adds paths to sys.path and sets the execution context
  BootstrapScript: str = f"""
import traceback, sys, os
PathToRun = r'{AbsoluteScriptPath}'
ScriptDir = r'{ScriptDirectory}'
CurrentDir = r'{CurrentWorkingDirectory}'

# Ensure the script's directory and CWD are in sys.path for imports
if ScriptDir not in sys.path:
  sys.path.insert(0, ScriptDir)
if CurrentDir not in sys.path:
  sys.path.insert(0, CurrentDir)

try:
  with open(PathToRun, 'r') as f:
    CodeContent = f.read()
    CodeObj = compile(CodeContent, PathToRun, 'exec')
    
    # Create a custom global namespace
    GlobalDict = {{
      "__file__": PathToRun,
      "__name__": "__main__",
      "__package__": None,
    }}
    
    import builtins
    GlobalDict["__builtins__"] = builtins
    
    # Execute with the custom globals
    exec(CodeObj, GlobalDict)
except Exception:
  ExcType, ExcValue, ExcTraceback = sys.exc_info()
  traceback.print_exception(ExcType, ExcValue, ExcTraceback.tb_next, file=sys.stdout)
"""

  try:
    print(f"Executing {AbsoluteScriptPath.name} in Blender...")
    
    subprocess.run(
      ["blender-remote-cli", "execute", "-c", BootstrapScript],
      cwd=ScriptDirectory,
      check=True
    )

  except subprocess.CalledProcessError as Error:
    print(f"Blender execution failed: {Error}")
  except FileNotFoundError:
    print("Error: 'blender-remote-cli' not found.")
  finally:
    if os.path.exists(TempFilePath):
      os.remove(TempFilePath)

if __name__ == "__main__":
  Parser = argparse.ArgumentParser(description="Run a Python script in Blender.")
  Parser.add_argument("ScriptPath", type=str)
  Args = Parser.parse_args()
  
  InputPath: pathlib.Path = pathlib.Path(Args.ScriptPath)
  RunInBlender(InputPath)
