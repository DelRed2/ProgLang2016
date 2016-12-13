import sys
import csv
import re

from math import *


def try_num_val(v):
    try:
        return int(v)
    except ValueError:
        try:
            return float(v)
        except ValueError:
            return v


def replace_table_ref(ref_match):
    ref = ref_match.group(0)
    letter = ref[0]
    num = ref[1:]
    col = ord(letter) - ord('A')
    row = int(num) - 1
    return "table[" + str(row) + "][" + str(col) + "]"


def eval_expression(expr_text):
    processed_expr = re.sub('[A-Z][1-9][0-9]*', replace_table_ref, expr_text)
    return eval(processed_expr, globals(), {})


inArg = sys.argv[1]
outArg = sys.argv[2]

if len(sys.argv) == 4:
    scriptFile = open(sys.argv[3])
    try:
        exec(scriptFile.read(), globals(), {})
    finally:
        scriptFile.close()


table = []
inFile = open(inArg, newline='')
try:
    for line in csv.reader(inFile):
        table.append(line)
finally:
    inFile.close()


for i in range(len(table)):
    for j in range(len(table[i])):
        try:
            table[i][j] = eval_expression(table[i][j][1:]) if table[i][j][0] == '=' else try_num_val(table[i][j])
        except:
            table[i][j] = "ERROR"


outFile = open(outArg, 'w', newline='')
try:
    csv.writer(outFile).writerows(table)
finally:
    outFile.close()
