from ply import lex, yacc
from collections import deque
from src.grammar import Rule


# Lexer:

tokens = ("NonTerminal",
          "StartNonTerminal",
          "Terminal",
          "Epsilon",
          "TransitionOp",
          "AlternativeOp",
          "ConcatOp",
          "NewLine")


t_NonTerminal = r"(?!(START|EPSILON))[A-Z]+"
t_StartNonTerminal = r"START"
t_Terminal = r"[a-z0-9]+"
t_Epsilon = r"EPSILON"
t_TransitionOp = r">"
t_AlternativeOp = r"\|"
t_ConcatOp = r"\+"


def t_NewLine(token):
    r"\n+"
    token.lexer.lineno += len(token.value)
    return token


t_ignore  = " \t"


grammar_lexer = lex.lex()


# Parser:

def p_grammar_recursive(p):
    "grammar : rule NewLine grammar"
    p[3].appendleft(p[1])
    p[0] = p[3]


def p_grammar_base(p):
    "grammar : rule"
    p[0] = deque([p[1]])


def p_rule(p):
    """rule : NonTerminal TransitionOp lhs
            | StartNonTerminal TransitionOp lhs"""
    p[0] = Rule(p[1], p[3])


def p_lhs_base(p):
    "lhs : unit"
    p[0] = deque([p[1]])


def p_lhs_recursion(p):
    "lhs : unit AlternativeOp lhs"
    p[3].appendleft(p[1])
    p[0] = p[3]


def p_unit_base(p):
    """unit : NonTerminal
            | StartNonTerminal
            | Terminal
            | Epsilon"""
    if p[1] == "EPSILON":
        p[0] = deque([])
    else:
        p[0] = deque([p[1]])


def p_unit_recursion(p):
    """unit : NonTerminal ConcatOp unit
            | Terminal ConcatOp unit
            | StartNonTerminal ConcatOp unit
            | Epsilon ConcatOp unit"""
    if p[1] != "EPSILON":
        p[3].appendleft(p[1])
    p[0] = p[3]


grammar_parser = yacc.yacc()
