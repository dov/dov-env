#!/usr/bin/python3

from __future__ import print_function
import sys,pickle
import pandas as pd

# Should these be optional
pd.set_option('display.max_columns', 9999)
pd.set_option('display.max_rows', 999999)
pd.set_option('display.width', 999999)
print(pd.read_pickle(sys.argv[1]))
