from os import system
from shutil import which
import os.path
from glob import glob
import platform
from sys import argv

# Check for Cabal
if not which('cabal'):
  print('Please install cabal to build Reality.')
  exit(1)

# Build the compiler project
system('cabal build exe:reality')

ext = '.exe' if platform.system() == 'Windows' else ''

executable_name = f"reality{ext}"

found_executables = glob(f"dist-newstyle/**/{executable_name}", recursive=True)
executable_files = [file for file in found_executables if os.path.isfile(file)]

if len(executable_files) == 0:
  print('No executable found')
  exit(1)

executable = executable_files[0]
executable_out = f"rl{ext}"

if not os.path.isdir('bin'): os.mkdir('bin')

system(f"cp {executable} bin/{executable_out}")

print(f"Reality compiler installed to bin/{executable_out}")

# Installing LF project manager

cmd = f"bin/rl{ext} packages/lf/main.rl -p std=$REALITY_DIR/std -p core=$REALITY_DIR/packages/lf -I \"<stdio.h>\" -I \"<string.h>\""

system(cmd)

gcc_cmd = f"bin/rl{ext} c packages/lf/main.c -l packages/lf/libc/System.c -f lm -f w -o bin/lf{ext} -f DRELEASE_MODE -f O3"

system(gcc_cmd)

print("LF project manager installed to packages/lf/output/program")
