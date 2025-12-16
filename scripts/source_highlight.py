#!/usr/bin/env python
######################################################################
#  Highlight a source file as HTML with inline CSS using Pygments,
#  optionally put it inside a framed box with the filename as a title,
#  optionally add line numbers, and open it in the default web browser.
#  
#  Requirements:
#    pip install pygments
#
#  2025-12-16 Tue
#  Dov Grobgeld <dov.grobgeld@gmail.com>
######################################################################

import argparse
import os
import sys
import webbrowser

from pygments import highlight
from pygments.formatters import HtmlFormatter
from pygments.lexers import (
  get_lexer_by_name,
  get_lexer_for_filename,
  guess_lexer_for_filename,
)
from pygments.styles import get_all_styles


def parse_args():
  parser = argparse.ArgumentParser(
    description="Highlight a source file as HTML with inline CSS (style baked in)."
  )

  parser.add_argument(
    "--lang",
    dest="lang",
    metavar="LANG",
    help=(
      "Source language (e.g. 'python', 'cpp'). "
      "If omitted, language is guessed from filename/contents."
    ),
  )

  parser.add_argument(
    "--style",
    dest="style",
    default="default",
    metavar="STYLE",
    help=(
      "Pygments style to use (default: %(default)s). "
      "Use --list-styles to see all available styles."
    ),
  )

  parser.add_argument(
    "--list-styles",
    dest="list_styles",
    action="store_true",
    help="List all available Pygments styles and exit.",
  )

  parser.add_argument(
    "-o",
    dest="output",
    metavar="OUTFILE",
    help='Output HTML filename. Default: INPUTFILE + ".html"',
  )

  parser.add_argument(
    "--no-open",
    dest="no_open",
    action="store_true",
    help="Do not automatically open the generated HTML in a web browser.",
  )

  parser.add_argument(
    "--frame",
    dest="frame",
    action="store_true",
    help="Wrap the code in a framed box with the filename as a title.",
  )

  parser.add_argument(
    "--lineno",
    dest="lineno",
    action="store_true",
    help="Add line numbers to the highlighted code.",
  )

  parser.add_argument(
    "filename",
    metavar="FILE",
    nargs="?",
    help="Source file to highlight.",
  )

  return parser.parse_args()


def list_styles_and_exit():
  styles = sorted(get_all_styles())
  print("Available styles:")
  for style in styles:
    print(f"  {style}")
  sys.exit(0)


def read_input_file(path):
  try:
    with open(path, "r", encoding="utf-8") as file_handle:
      return file_handle.read()
  except OSError as exc:
    print(f"Error: cannot read '{path}': {exc}", file=sys.stderr)
    sys.exit(1)


def determine_lexer(filename, text, lang):
  if lang:
    try:
      return get_lexer_by_name(lang)
    except Exception as exc:
      print(f"Error: unknown language '{lang}': {exc}", file=sys.stderr)
      sys.exit(1)

  try:
    return get_lexer_for_filename(filename, text)
  except Exception:
    try:
      return guess_lexer_for_filename(filename, text)
    except Exception as exc:
      print(f"Error: could not determine lexer for '{filename}': {exc}", file=sys.stderr)
      sys.exit(1)


def determine_output_filename(input_filename, output_option):
  if output_option:
    return output_option
  return input_filename + ".html"


def parse_hex_color_to_rgb(hex_color):
  if not hex_color:
    return 255, 255, 255

  hex_color = hex_color.strip()
  if not hex_color.startswith("#"):
    return 255, 255, 255

  value = hex_color[1:]
  length = len(value)

  try:
    if length == 3:
      r = int(value[0] * 2, 16)
      g = int(value[1] * 2, 16)
      b = int(value[2] * 2, 16)
      return r, g, b
    if length == 4:
      r = int(value[0] * 2, 16)
      g = int(value[1] * 2, 16)
      b = int(value[2] * 2, 16)
      return r, g, b
    if length == 6:
      r = int(value[0:2], 16)
      g = int(value[2:4], 16)
      b = int(value[4:6], 16)
      return r, g, b
    if length == 8:
      r = int(value[0:2], 16)
      g = int(value[2:4], 16)
      b = int(value[4:6], 16)
      return r, g, b
  except ValueError:
    pass

  return 255, 255, 255


def pick_contrast_text_color(background_hex):
  r, g, b = parse_hex_color_to_rgb(background_hex)
  luminance = 0.299 * r + 0.587 * g + 0.114 * b
  return "#000000" if luminance > 128 else "#ffffff"


def make_highlight_fragment(text, lexer, style_name, lineno):
  """
  Return a self-contained HTML fragment with inline styles only,
  including a background color on the <pre> block. Optionally add line numbers.
  """
  probe = HtmlFormatter(style=style_name)
  background_color = probe.style.background_color or "#ffffff"

  formatter_kwargs = {
    "full": False,
    "noclasses": True,
    "style": style_name,
  }
  if lineno:
    # "table" layout tends to be more robust for line numbers
    formatter_kwargs["linenos"] = "table"

  formatter = HtmlFormatter(**formatter_kwargs)

  raw_html = highlight(text, lexer, formatter)

  # Ensure the <pre> itself has background + padding and fills width.
  pre_style = (
    f'background-color: {background_color}; '
    f'padding: 0.5em; '
    f'overflow-x: auto; '
    f'margin: 0; '
    f'line-height: 125%; '
    f'display: block; '
    f'box-sizing: border-box; '
    f'width: 100%;'
  )

  if "<pre" in raw_html:
    raw_html = raw_html.replace(
      "<pre",
      f'<pre style="{pre_style}"',
      1,
    )
  else:
    raw_html = f'<pre style="{pre_style}">{raw_html}</pre>'

  return raw_html, background_color


def wrap_in_frame(fragment, background_color, title_text):
  """
  Wrap the code fragment in a very simple, email-friendly frame.

  One table, two rows:
    - Row 1: filename label with an <hr> under it.
    - Row 2: highlighted code.
  """
  safe_title = title_text or ""
  title_color = pick_contrast_text_color(background_color)

  frame_html = f"""
<table cellpadding="0" cellspacing="0" border="0"
       style="border-collapse: collapse; margin: 0.5em 0; width: 100%;">
  <tr>
    <td style="
      border: 1px solid #888;
      border-bottom: none;
      padding: 4px 8px;
      font-family: sans-serif;
      font-size: 0.9em;
      font-weight: bold;
      color: {title_color};
      background-color: {background_color};
      width: 100%;
    ">
      {safe_title}
      <hr style="border: 0; border-top: 1px solid #888; margin: 4px 0 0 0; padding: 0;">
    </td>
  </tr>
  <tr>
    <td style="
      border: 1px solid #888;
      border-top: none;
      padding: 4px 8px;
      background-color: {background_color};
      width: 100%;">
      {fragment}
    </td>
  </tr>
</table>
"""
  return frame_html


def make_full_html_document(fragment, background_color):
  html = f"""<!DOCTYPE html>
<html>
  <head>
    <meta charset="utf-8">
    <title>Highlighted source</title>
  </head>
  <body style="background-color: {background_color}; margin: 0; padding: 1em; font-family: sans-serif;">
    <!--
      When copy/pasting into Gmail/Outlook:
        - With --frame: copy the outer <table>...</table> block.
        - Without --frame: copy ONLY the <pre>...</pre> block.
      Email clients may still alter borders/background/width; this is a
      limitation of their sanitization and layout, not your HTML.
    -->
{fragment}
  </body>
</html>
"""
  return html


def open_in_browser(path):
  try:
    absolute_path = os.path.abspath(path)
    url = "file://" + absolute_path
    webbrowser.open(url)
  except Exception as exc:
    print(f"Warning: could not open browser for '{path}': {exc}", file=sys.stderr)


def main():
  args = parse_args()

  if args.list_styles:
    list_styles_and_exit()

  if not args.filename:
    print("Error: missing FILE argument.", file=sys.stderr)
    print("Use --help for usage.", file=sys.stderr)
    sys.exit(1)

  filename = args.filename

  if not os.path.isfile(filename):
    print(f"Error: '{filename}' is not a file or does not exist.", file=sys.stderr)
    sys.exit(1)

  source_text = read_input_file(filename)
  lexer = determine_lexer(filename, source_text, args.lang)

  fragment, bg_color = make_highlight_fragment(
    source_text,
    lexer,
    args.style,
    args.lineno,
  )

  if args.frame:
    base_name = os.path.basename(filename)
    fragment = wrap_in_frame(fragment, bg_color, base_name)

  full_html = make_full_html_document(fragment, bg_color)

  out_filename = determine_output_filename(filename, args.output)

  try:
    with open(out_filename, "w", encoding="utf-8") as file_handle:
      file_handle.write(full_html)
  except OSError as exc:
    print(f"Error: cannot write '{out_filename}': {exc}", file=sys.stderr)
    sys.exit(1)

  print(f"Wrote highlighted HTML to '{out_filename}'")
  if args.frame:
    print("Tip: for email, copy the outer <table>...</table> block.")
  else:
    print("Tip: for email, copy ONLY the <pre>...</pre> block from the file.")

  if not args.no_open:
    open_in_browser(out_filename)


if __name__ == "__main__":
  main()
