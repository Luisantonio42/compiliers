# -----------------------------------------------------------------------------
# calc.py
#
# A simple calculator with variables.   This is from O'Reilly's
# "Lex and Yacc", p. 63.
# -----------------------------------------------------------------------------

import sys
sys.path.insert(0, "../..")

tokens = (
    'NAME', 'INUMBER','FNUMBER'
)

literals = ['=', '+', '-', 'i', 'f','p']
# Tokens

t_NAME = r'[a-eg-hj-oq-z]'

def t_FNUMBER(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t


t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
import ply.lex as lex
lexer = lex.lex()

# Parsing rules

precedence = (
    ('left', '+', '-'),
    ('left', '*', '/'),
    ('right', 'UMINUS'),
)

# dictionary of names
names = {}

def p_statement_declare_int(p):
    'statement : "i" NAME'
    names[p[2]] = {"type":"INT", "value":0}

def p_statement_declare_float(p):
    'statement : "f" NAME'
    names[p[2]] = {"type":"FLOAT", "value":0}

def p_statement_print(p):
    'statement : "p" expression'
    print(p[2])

def p_statement_assign(p):
    'statement : NAME "=" expression'
    if p [1] not in names:
        print("You must declare a variable before using it")
    
    names[p[1]]["value"] = p[3]


def p_statement_expr(p):
    'statement : expression'
    #print(p[1])


def p_expression_binop(p):
    '''expression : expression '+' expression
                  | expression '-' expression
                  | expression '*' expression
                  | expression '/' expression'''
    if p[2] == '+':
        p[0] = p[1] + p[3]
    elif p[2] == '-':
        p[0] = p[1] - p[3]


def p_expression_uminus(p):
    "expression : '-' expression %prec UMINUS"
    p[0] = -p[2]


def p_expression_group(p):
    "expression : '(' expression ')'"
    p[0] = p[2]


def p_expression_inumber(p):
    "expression : INUMBER"
    print("Integer")
    p[0] = p[1]

def p_expression_fnumber(p):
    "expression : FNUMBER"
    print("Float")
    p[0] = p[1]

def p_expression_name(p):
    "expression : NAME"
    try:
        p[0] = names[p[1]]["value"]
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0


def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")

import ply.yacc as yacc
parser = yacc.yacc()

f = open("test.txt")
content = f.read()
#print(content)

yacc.parse(content)

# while True:
#     try:
#         s = input('calc > ')
#     except EOFError:
#         break
#     if not s:
#         continue
#     yacc.parse(s)
