import sys
import subprocess

if len(sys.argv) is not 4:
  sys.exit(1)

binary = sys.argv[1]
input_filename = sys.argv[2]
output_filename = sys.argv[3]

try:
  with open(output_filename, "r") as f:
    expected_part1 = f.readline().rstrip('\n')
    expected_part2 = f.readline().rstrip('\n')
except FileNotFoundError:
  print('output file does not exist')
  sys.exit(1)

proc = subprocess.run([binary, input_filename], stdout=subprocess.PIPE, encoding='utf-8')
results = proc.stdout.splitlines()

if proc.returncode != 0:
  print('process exited with non-zero return code')
  sys.exit(1)
if results[0] != expected_part1:
  print('PART 1:')
  print('  expected: {}'.format(expected_part1))
  print('  actual:   {}'.format(results[0]))
  sys.exit(1)
if results[1] != expected_part2:
  print('PART 2:')
  print('  expected: {}'.format(expected_part2))
  print('  actual:   {}'.format(results[0]))
  sys.exit(1)

sys.exit(0)

