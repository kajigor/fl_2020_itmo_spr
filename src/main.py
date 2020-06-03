import parser
import argparse

p = argparse.ArgumentParser()
p.add_argument("-f", dest="filename")
args = p.parse_args()
path = args.filename
with open(path, "r") as f:
    result = parser.parser.parse(f.read())
    print(result)













