#!/usr/bin/env python3
import sys
import re
import os

link = 'git@github.com:ITMO-MPP/possible-executions-analysis-cde8eae8.git'
link = sys.argv[1]
dir = re.match('git@github.com:ITMO-MPP/(.*)-cde8eae8.git', link).group(1)
cmd = f'git clone "{link}" "{dir}"'
print(f'--> {cmd}')
os.system(cmd)
