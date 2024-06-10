#!/usr/bin/python

# This script contains perl code for doing various code conversions
# and instantiations related to the XJet RComponent library.

import re,sys

def psplit(text):
    """Splits a string on commas, ignoring commas within parentheses."""
    return re.split(r",\s*(?![^()]*\))", text)


def print_c_function(comment, return_type, class_name, function_name, arguments):
  """Prints a skeleton C function with comments and placeholders."""

  # Join arguments with comma separation (handle empty list)
  args_string = ", ".join(arguments) if arguments else "void"

  # Build the function string
  template = ""
  if comment:
    template += f"{comment}\n"
  template += f"{return_type} {class_name}::{function_name}({args_string})\n"
  template += "{\n"
  template += "  R_TRACE;\n"
  template += "\n"
  template += "  // TBD\n"
  template += "}"

  return template

def print_h_function(comment, return_type, class_name, function_name, arguments):
  """Prints a h function with comments and placeholders."""

  # Join arguments with comma separation (handle empty list)
  args_string = ", ".join(arguments) if arguments else "void"

  # Build the function string
  template = ""
  if comment:
    template += f"    {comment}\n"
  template += f"    {return_type} {function_name}({args_string});"

  return template

def def2init(text, class_name):
  """Converts DEF statements in a string to INIT_METHOD_DOC statements.

  Args:
      text: The input string containing DEF statements.
      class_name: The class name for which the methods are defined.

  Returns:
      A string containing the converted INIT_METHOD_DOC statements.
  """

  # States
  # 0 - Before comment
  # 1 - In comment
  # 2 - In function definition

  state = 0
  comment = ""
  output_text = ""

  for line in text.splitlines():
    if match:=re.search(r'^\s*//\s*(.*)', line):  # Check for comment line
      if state == 0:
        state = 1
        comment = match.group(1).strip()  # Remove leading "//" and strip whitespace
      else:
        comment += " " + match.group(1).strip()  # Append comment line (remove "//" and strip)

    elif match := re.search(r"(DEFINE.*?)\((.*?)\)", line):
      state = 0
      define_statement, arguments = match.group(1), match.group(2)

      # Process arguments (remove matching parens, extract class and docstring)
      args_list = psplit(arguments)

      class_arg = args_list.pop(0)
      docstring = ""
      if not 'VOID' in define_statement.upper():
        if args_list:
          args_list.pop(0)  # Remove return type if not VOID
      function_name = args_list.pop(0)

      # Extract docstring from comments within remaining arguments
      docstring_vars = []
      for arg in args_list:
        if match:= re.search(r"/\*\s*(.*?)\s*\*/", arg):
          docstring_vars.append(match.group(1))

      var_string = ", ".join(docstring_vars)

      # Build INIT_METHOD_DOC statement
      comment = comment.replace('"','\\"')
      output_text += f"  INIT_METHOD_DOC({class_name},{function_name},\"{function_name}({var_string}): {comment}\");\n"

  return output_text


def h2c(text, class_name):
  """Converts function declarations from a header file to C statements.

  Args:
      text: The input string containing header file declarations.
      class_name: The class name for which the functions are defined.

  Returns:
      A string containing the converted C statements.
  """

  output_text = ""
  # Regular expression pattern for function declarations
  pattern = r"\s*(\S+)\s+(\w+)\s*\((.*?)\)\s*(?:override)?;"

  for match in re.finditer(pattern, text, flags=re.DOTALL):
    # Extract return type, function name, and arguments
    return_type, function_name, arguments = match.groups()

    # Extract comment after the declaration (remove leading whitespace)
    comment = match.group(0)[match.end():].strip()

    # Process arguments (assuming split_ignoring_parens exists)
    args_list = psplit(arguments)

    # Call PrintCFunction (assuming it's defined)
    output_text += print_c_function(comment, return_type, class_name, function_name, args_list)

  return output_text

def h2def(text, class_name):
  """Converts function declarations from a header file to Def statements.

  Args:
      text: The input string containing header file declarations.
      class_name: The class name for which the functions are defined.

  Returns:
      A string containing the converted Def statements.
  """

  output_text = ""
  # Regular expression pattern for function declarations
  pattern = r"(\S+)\s+(\w+)\s*\((.*?)\)\s*;"

  for match in re.finditer(pattern, text, flags=re.DOTALL):
    # Extract comment, return type, function name, and arguments
    comment = match.group(0)[:match.start(1)].strip()  # Extract comment before declaration
    if comment:
      comment += "\n"  # Add newline for readability
    return_type, function_name, arguments = match.groups()

    # Process arguments (assuming split_ignoring_parens exists)
    args_list = psplit(arguments)

    # Mark last argument as comment (assuming single line comment syntax)
    for i, arg in enumerate(args_list):
      args_list[i] = re.sub(r"(\S+)\s+(\w+)$", r"\1 /*\2*/", arg)

    # Join arguments with comma separation
    arg_string = ", ".join(args_list)
    num_args = len(args_list)
    void_args = (num_args == 0 or args_list[0] == "void")

    # Choose appropriate DEFINE statement based on return type and arguments
    if return_type.lower() == "void" and void_args:
      output_text += f"    DEFINE_VOID_METHOD({class_name}, {function_name});\n"
    elif return_type.lower() == "void":
      output_text += f"    DEFINE_VOID_METHOD_{num_args}({class_name}, {function_name}, {arg_string});\n"
    elif void_args:
      output_text += f"    DEFINE_METHOD({class_name}, {return_type}, {function_name});\n"
    else:
      output_text += f"    DEFINE_METHOD_{num_args}({class_name}, {return_type}, {function_name}, {arg_string});\n"

  return output_text

def def2c(text, class_name):
  """Converts DEF statements in a string to C template code using PrintCFunction.

  Args:
      text: The input string containing DEF statements.
      class_name: The class name for which the methods are defined.

  Returns:
      A string containing the converted C template code.
  """

  output_text = ""
  for line in text.splitlines():

    # Unindent and print comments
    if m:= re.match(r'^\s*(//.*)', line):
      output_text += m.group(1) + "\n"
      continue

    # Match DEF statements
    if match:= re.search(r"(DEFINE.*?)\((.*)\)", line):
      function_name_template, arguments = match.groups()

      # Process arguments (remove matching parens, extract class and return type)
      args_list = psplit(arguments)  

      class_name_arg = args_list.pop(0) if args_list else None
      return_type = "void" if 'VOID' in function_name_template.upper() else args_list.pop(0)
      function_name = args_list.pop(0)

      # Remove comments within remaining arguments (assuming single line comment syntax)
      for i, arg in enumerate(args_list):
        args_list[i] = re.sub(r"\s*/\*\s*", " ", arg)
        args_list[i] = re.sub(r"\*/", "", args_list[i]).strip()

      # Call PrintCFunction (assuming it's defined) to generate the C template
      output_text += print_c_function("", return_type, class_name, function_name, args_list)

  return output_text

def def2h(text, class_name):
  """Converts DEF statements in a string back to an h- statement

  Returns:
      A string containing the converted C template code.
  """

  output_text = ""
  for line in text.splitlines():

    # Unindent and print comments
    if m:= re.match(r'^\s*(//.*)', line):
      output_text += line + '\n'
      continue

    # Match DEF statements
    if match:= re.search(r"(DEFINE.*?)\((.*)\)", line):
      function_name_template, arguments = match.groups()

      # Process arguments (remove matching parens, extract class and return type)
      args_list = psplit(arguments)  

      class_name_arg = args_list.pop(0) if args_list else None
      return_type = "void" if 'VOID' in function_name_template.upper() else args_list.pop(0)
      function_name = args_list.pop(0)

      # Remove comments within remaining arguments (assuming single line comment syntax)
      for i, arg in enumerate(args_list):
        args_list[i] = re.sub(r"\s*/\*\s*", " ", arg)
        args_list[i] = re.sub(r"\*/", "", args_list[i]).strip()

      output_text += print_h_function("", return_type, class_name, function_name, args_list)

  return output_text

def create_set_function(text, class_name):
  """Creates a C template for a Set function based on a variable list.

  Args:
      text: The input string containing the variable list (space-separated).
      class_member_prefix: The prefix used for member variables (optional).

  Returns:
      A string containing the C template code for the Set function.
  """

  # Remove leading whitespace from the variable list
  variable_list = text.strip()

  # Split the variable list into individual variables
  variables = variable_list.split()

  # Build the function template (f-strings for cleaner formatting)
  template = f"""
          void Set{''.join(variables)}(double {', double '.join(variables)})
          {{
  """
  for var in variables:
    template += f"            m_{var} = {var};\n"
  template += "          }\n"

  return template

# Initialize variables with default values
class_name = 'Unknown'
do_def2init = False
do_def2c = False
do_def2h = False
do_h2c = False
do_set = False
do_h2def = False
help = False

while len(sys.argv) > 1 and sys.argv[1].startswith("-"):
  arg = sys.argv.pop(1)

  if arg == "--classname":
    if len(sys.argv) > 1:
      class_name = sys.argv.pop(1)
    else:
      print("Error: Missing class name after --classname")
      sys.exit(1)
  elif arg == "--def2init":
    do_def2init = True
  elif arg == "--def2c":
    do_def2c = True
  elif arg == "--h2c":
    do_h2c = True
  elif arg == "--set":
    do_set = True
  elif arg == "--h2def":
    do_h2def = True
  elif arg == "--def2h":
    do_def2h = True
  elif arg == "--help":
    help = True
    print("""
rcomponent.py -- Convert code for the rcomponent library

Syntax:
  rcomponent.py --classname cn [--h2def] [--def2h] [--def2init]
""")
    sys.exit(0)
  else:
    print(f"Error: Unknown option: {arg}")
    sys.exit(1)

text = ''.join(sys.stdin)

if do_def2init:
  print(def2init(text, class_name))
elif do_h2def:
  print(h2def(text, class_name))
elif do_h2c:
  print(h2c(text, class_name))
elif do_def2c:
  print(def2c(text, class_name))
elif do_def2h:
  print(def2h(text, class_name))
elif do_set:
  print(CreateSetFunction(text, class_name))
