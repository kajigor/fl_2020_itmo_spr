import argparse as ag
from src.grammar import Grammar
from src.grammar_parsing import grammar_parser


arg_parser = ag.ArgumentParser()
arg_parser.add_argument("-g",
                        type=str,
                        dest="grammar",
                        required=True)
arg_parser.add_argument("-i",
                        type=str,
                        dest="input",
                        required=False)

def main():
    args = vars(arg_parser.parse_args())
    with open(args["grammar"], "r") as g_istream:
        grammar = Grammar(grammar_parser.parse(g_istream.read().strip()))
    print("Parsed grammar:", sep="\n\n")
    print(grammar)
    # Пока что не доделано:
    if args["input"] is None:
        return    

if __name__ == "__main__":
    main()
