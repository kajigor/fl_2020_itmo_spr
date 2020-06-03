from ply import lex, yacc
import ast

tokens = (
    'Var',
    'Ident',
    'TargetSymb',
    'RelationSymb',
    'Point',
    'Left',
    'Right',
    'Separator'
)


t_Var = r'[A-Z]([a-z]|[A-Z]|[0-9])*'
t_Ident = r'[a-z]([a-z]|[A-Z]|[0-9])*'
t_TargetSymb = r'\?-'
t_RelationSymb = r':-'
t_Point = r'\.'
t_Left = r'\('
t_Right = r'\)'
t_Separator = r','


def t_error(t):
  print("Invalid Token:", t.value[0])
  t.lexer.skip(1)

t_ignore  = " \t\n"

lexer = lex.lex()


def p_p(p) :
    """P : Relations TargetSymb Target
         | TargetSymb Target"""
    if len(p) == 4:
        p[0] = ast.P(p[3], p[1])
    else:
        p[0] = ast.P(p[2])


def p_relations(p):
    """Relations : Relation 
                 | Relation Relations""" 
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[2].append(p[1])
        p[0] = p[2]


def p_relation(p):
    """Relation : Atom Point
              | Atom RelationSymb Body"""
    if len(p) == 3:
        p[0] = ast.Relation(p[1])
    else:
        p[0] = ast.Relation(p[1], p[3])


def p_atom(p):
    """Atom : Ident
            | Ident Left Args Right"""
    if len(p) == 2:
        p[0] = ast.Atom(p[1])
    else:
        p[0] = ast.Atom(p[1], p[3])


def p_args(p):
    """Args : Arg
            | Arg Separator Args"""
    if len(p) == 2:
       p[0] = [p[1]]
    else:
       p[3].append(p[1])
       p[0] = p[3]

def p_arg(p):
    """Arg : Var
           | Atom"""
    p[0] = ast.Arg(p[1])


def p_Body(p):
    """Body : Atoms"""
    p[0] = ast.Body(p[1])


def p_atoms(p): 
    """Atoms : Atom Point
             | Atom Separator Atoms"""
    if len(p) == 3:
        p[0] = [p[1]]
    else:
        p[3].append(p[1])
        p[0] = p[3]


def p_target(p):
    """Target : Atoms"""
    p[0] = ast.Target(p[1])


def p_error(p):
    print(p)
    print("Syntax error in input!")


parser = yacc.yacc()

