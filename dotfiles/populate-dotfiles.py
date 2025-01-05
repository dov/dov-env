#!/usr/bin/python

######################################################################
#  Distribute the dot files in this directory to their destination.
#
#  2025-01-05 Sun
#  Dov Grobgeld <dov.grobgeld@gmail.com>
######################################################################

import argparse
import sys
from subprocess import Popen, PIPE
import logging
from threading import Timer
from pathlib import Path
import os

def die(msg):
  logging.error(msg.strip())
  logging.error('Exiting...')
  sys.stderr.write(msg.strip()+'\n')
  sys.exit(-1)

# Run an external command and throw an exception on timout and on
# rc != 0.
def xec(cmd, decode=True, chomp=True, verbose=False, timeout=None):
  '''Run a command a returns its stdout output.

  decode -- run (utf8) decode of the resulting string
  chomp -- get rid of the last newline (like in perl)
  '''
  def chomp_(s):
    '''Remove trailing '\n' and '\r'''
    while(len(s) and s[-1] in ['\n','\r']):
      s = s[:-1]
    return s
  
  logging.info(f'Running command: {cmd}')
  rc = 0
  stdout = stderr = b''
  timer = None
  with Popen(cmd,shell=True,stdout=PIPE,stderr=PIPE) as ph:
    if timeout is not None:
      timer = Timer(timeout, ph.kill)

    try:
      if timer is not None:
        timer.start()
      stdout, stderr = ph.communicate()
      if len(stdout):
        logging.info(f'stdout: {chomp_(stdout.decode())}')
      if len(stderr):
        logging.info(f'stderr: {chomp_(stderr.decode())}')
    finally:
      if timer:
        if timer.is_alive():
          timer.cancel() # No timeout
        else:
          # Raise an exception on timeout
          logging.warning(f'Got timeout after {timeout}s!')
          raise RuntimeError('Timeout!')

    rc = ph.returncode

  if decode:
    stdout=stdout.decode()
    stderr=stderr.decode()
  if chomp:
    stdout = chomp_(stdout)

  # Raise an exception on error
  if rc != 0:
    logging.info(f'Process terminated with error rc={rc}')
    if not decode:
      stderr = stderr.decode()  # Decode before writing to stderr
    sys.stderr.write(stderr)
    raise RuntimeError(f'Process terminated with return code={rc}!')

  return stdout

if __name__ == '__main__':
  import datetime
  import logging
  
  logging.basicConfig(filename='populate-dotfiles.log',
                      level=logging.DEBUG,
                      format='%(levelname)s: %(name)s: %(message)s')
  logging.info('------------------')
  logging.info(f'Running on {datetime.datetime.now().strftime("%Y-%m-%d %H:%M:%S")}')

  parser = argparse.ArgumentParser(description='Process a file')
  parser.add_argument('-d', '--destination',
                      dest='destination',
                      action='store',
                      type=str,
                      default=os.environ['HOME'],
                      help='Destination')
  parser.add_argument('-f', '--force',
                      dest='force',
                      action='store_true',
                      help='Override without question')
  args = parser.parse_args()
  destination = args.destination
  for file in ('.xkbmap',
               '.fvwm2rc',
               '.gitconfig',
               '.tmux.conf',
               '.vimrc',
               '.zshrc',
               '.config',
               '.cmake'):
    dest = Path(destination) / file
    print(f'Copying {file}')
    if dest.exists() and not args.force:
      die(f'{dest} already exist. Use --force to override')

    xec(f'rsync -a {file} {destination}', verbose=True)
  print('ok')
    
    
