#!/usr/bin/python3
######################################################################
#  A script for pretty printing and possibly join dataframes in
#  either pkl or csv formats.
#
#  This script is used by the emacs pkl-mode to turn dataframe
#  pickles into emacs visible text.
#
#  Dov Grobgeld
#  2019-12-19 Thu
######################################################################

from __future__ import print_function
import sys,pickle,os
import pandas as pd
from tabulate import tabulate
import matplotlib.pyplot as plt
import pdb

def die(msg):
  print(msg)
  sys.exit(-1)

def print_table(df, fmt=None, do_reset_index = True, do_show_index = False):
  if do_reset_index:
    df = df.reset_index()
  if fmt=='csv':
    print(df.to_csv(na_rep=''))
  elif fmt is not None:
    print(tabulate(df, tablefmt=fmt,headers='keys',showindex=False))
  else:
    print(df.to_string(index=False,justify='right'))
  

argp=1
do_join = False
fmt = None
filter_columns = None
do_plot = False
join_numeric_index = False

while argp < len(sys.argv) and sys.argv[argp][0]=='-':
  S_= sys.argv[argp]
  argp+=1
  if S_=='--help':
    print('pkl-cat - Cat and join csv and pkl files\n'
          '\n'
          'Syntax:\n'
          '  pkl-cat [--join] file...\n'
          '\n'
          'Options:\n'
          '  --join      Join together several files\n'
          '  --join-num  When joining use numerical index\n'
          '  --org       Output table in org mode. Default is ascii\n'
          '  --csv       Output table in csv mode. Useful when joining.\n'
          '  --fmt fmt   Output table in any format supported by python tabulate.\n'
          '  --cols cc   A comma separated list of columns to show.\n'
          '  --plot      When joining plot the filtered columns\n'
          '  --title     Title for plot\n'
          '  --xlabel    x-label for plot\n'
          '  --ylabel    y-label for plot\n'
          )
    
    sys.exit(0)
  if S_=='--join':
    do_join = True
    continue
  if S_=='--join-num':
    do_join = True
    join_numeric_index = True
    continue
  if S_=='--org':
    fmt = 'orgtbl'
    continue
  if S_=='--fmt':
    fmt = sys.argv[argp]
    argp+=1
    continue
  if S_=='--csv':
    fmt = 'csv'
    continue
  if S_=='--cols':
    filter_columns = sys.argv[argp].split(',')
    argp+= 1
    continue
  if S_=='--plot':
    do_plot = True
    continue
  if S_=='--title':
    title = sys.argv[argp]
    argp+= 1
    continue
  if S_=='--xlabel':
    xlabel = sys.argv[argp]
    argp+= 1
    continue
  if S_=='--ylabel':
    ylabel = sys.argv[argp]
    argp+= 1
    continue
  die('Unknown option ' + S_)
  
if do_plot and not do_join:
  die('Plotting is only supported when joining!')

# Should these be optional?
pd.set_option('display.max_columns', 9999)
pd.set_option('display.max_rows', 999999)
pd.set_option('display.width', 999999)

res = pd.DataFrame()
for fn in sys.argv[argp:]:
  df = None
  if fn.endswith('.csv'):
    df = pd.read_csv(fn)
  else:
    df = pd.read_pickle(fn)
  if filter_columns is not None:
    df = df[filter_columns]
  if do_join:
    df['Filename'] = os.path.basename(os.path.dirname(fn))
    res = res.append(df,sort=False)
  else:
    print_table(df, fmt=fmt)

if do_join:
  ColumnsWithoutFilename = [c for c in res.columns if c!='Filename']

  if do_plot:
    res = res.reset_index(drop=True)
    # Get rid of non-numeric data
    for c in ColumnsWithoutFilename:
      if res[c].dtype==object:
        res[c] = res[c].str.extract(r'(\d+\.*\d*)', expand=False).astype(float)

    res[ColumnsWithoutFilename].plot()
    if title is not None:
      plt.title(title)
    if xlabel is not None:
      plt.xlabel(xlabel)
    if ylabel is not None:
      plt.ylabel(ylabel)
    plt.show()
  else:
    if join_numeric_index:
      res['Index'] = range(len(res))
      res = res[['Index']+ColumnsWithoutFilename]
    else:
      res = res[['Filename']+ColumnsWithoutFilename] # Reorder columns
    print_table(res, fmt=fmt)
  
