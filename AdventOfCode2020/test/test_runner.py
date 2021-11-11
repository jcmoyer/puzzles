import sys
import subprocess
import difflib
import textwrap

if len(sys.argv) is not 4:
  sys.exit(1)

binary = sys.argv[1]
input_filename = sys.argv[2]
output_filename = sys.argv[3]

try:
  with open(output_filename, "r") as f:
    expected = f.read().strip()
except FileNotFoundError:
  print('output file does not exist')
  sys.exit(1)

proc = subprocess.run([binary, input_filename], stdout=subprocess.PIPE, encoding='utf-8')
results = proc.stdout.strip()

if proc.returncode != 0:
  print('process exited with non-zero return code')
  sys.exit(1)
if results != expected:
  print('result differs from expected; context diff:\n')
  diff = difflib.context_diff(expected.splitlines(), results.splitlines(), n=2, lineterm='', fromfile='expected', tofile='actual')
  print(textwrap.indent('\n'.join(diff), '  '))
  sys.exit(1)

sys.exit(0)

