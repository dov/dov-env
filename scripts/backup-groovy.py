#!/usr/bin/python

######################################################################
#  A script for backup up my main computer to gropi's ssd disk
#
#  2023-10-08 Sun
#  Dov Grobgeld <dov.grobgeld@gmail.com>
######################################################################

import datetime
import selectors # Note! Does not work with pipes on Windows!
import logging
from pathlib import Path
import os
import math
import argparse

verbose = False
dry_run = False

home = Path.home()
log_dir = home / 'log'
backup_dir = home / 'backup'
time_stamp_file = backup_dir/'last-backup.txt'
min_seconds_between_backups = 12 * 3600  # 12h

def info(msg):
  logging.info(msg)
  if verbose:
    print(msg)

def error(msg):
  logging.error(msg)
  if verbose:
    print(msg)

import subprocess

def xec(cmd, decode=True, chomp=True,
        return_stdout=False):
  '''Run a command a returns its stdout output.

  decode -- run (utf8) decode of the resulting string
  chomp -- get rid of the last newline (like in perl)
  '''
  def chomp(s):
    s=s.decode()
    while len(s) and s[-1]=='\n':
      s = s[:-1]
    return s

  if dry_run:
    cmd = f'echo "{cmd}"'
  info(f'xec: {cmd}')

  ret = ''
  with subprocess.Popen(cmd,shell=True,
                        stdout=subprocess.PIPE,
                        stderr=subprocess.PIPE) as ph:
    sel = selectors.DefaultSelector()
    sel.register(ph.stdout, selectors.EVENT_READ)
    sel.register(ph.stderr, selectors.EVENT_READ)

    line_stdout = line_stderr = ''
    done = False
    while not done:
      for key, _ in sel.select():
        data = key.fileobj.read1().decode(errors='ignore')
        if not data:
           done=True
           break
        if key.fileobj is ph.stdout:
          line_stdout += data
          while '\n' in line_stdout:
             line, line_stdout = line_stdout.split('\n', maxsplit=1)
             info(f'Got stdout: {line}')
             if return_stdout:
               ret += line
        else:
          line_stderr += data
          while '\n' in line_stderr:
             line, line_stderr = line_stderr.split('\n', maxsplit=1)
             info(f'Got stderr: {line}')

    if len(line_stdout):
      info(f'Got stdout: {line_stdout}')
      if return_stdout:
        ret += line_stdout
    if len(line_stderr):
      info(f'Got stderr: {line_stderr}')
  return ret if return_stdout else None

def chdir(directory):
  cwd = os.getcwd()
  os.chdir(directory)
  logging.info(f'>>> cd {directory}')
  return cwd

def write_time_stamp_file():
  with open(time_stamp_file, 'w') as fh:
    fh.write(datetime.datetime.now().isoformat())

def read_time_stamp_file():
  with open(time_stamp_file) as fh:
    return datetime.datetime.fromisoformat(fh.read())

def format_dt_hhmmss(dt):
  total_seconds = dt.total_seconds()
  hours =  int(total_seconds // 3600)
  minutes = int((total_seconds % 3600) // 60)
  seconds = int((total_seconds % 60))
  return f'{hours:02d}:{minutes:02d}:{seconds:02d}'

def main():
  global verbose

  log_dir.mkdir(parents=True, exist_ok=True)
  logging.basicConfig(filename=log_dir / 'backup-groovy.log',
                      level=logging.DEBUG,
                      format='%(levelname)s: %(asctime)s.%(msecs)03d: %(message)s',
                      datefmt='%H:%M:%S')
  
  logging.info('---=-===-==<O>==-===-=---')
  logging.info(f'Running on {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")}')
  
  parser = argparse.ArgumentParser(description='Backup groovy')
  parser.add_argument('-v', '--verbose',
                      dest='verbose',
                      action='store_true',
                      help='Verbose editing')
  parser.add_argument('-f', '--force',
                      dest='force',
                      action='store_true',
                      help='force backup even if done "recently"')
  parser.add_argument('filename', nargs='?', help='Input. Default is stdin')
  args = parser.parse_args()
  
  verbose = args.verbose

  last_time =read_time_stamp_file()
  dt = datetime.datetime.now() - last_time
  if not args.force and dt.total_seconds() < min_seconds_between_backups:
    info(f'Backup was already done {format_dt_hhmmss(dt)} ago.')
    exit()
  
  if '0 received' in xec('ping -W 0.1 -c 1 192.168.1.10',return_stdout=True):
    logging.info('Groovy not alive! exiting')
    exit()
  info('Confirmed Groovy is alive! Continuing.')
  
  xec(f'cd {home/"org"} && rsync -av groovy:org/ .')
  xec(f'cd {backup_dir/"scans"} && rsync -av groovy:/terra/space/scans/ .')
  
  chdir(home/'backup/repos')
  for d in Path('.').iterdir():
    if (d / 'refs').exists():
      xec(f'cd {d} && git remote update')
    else:
      info(f'Skipping {d}')
  
  write_time_stamp_file()
  
  info('Backup finished successfully!')

if __name__=='__main__':
  main()
print('ok')

