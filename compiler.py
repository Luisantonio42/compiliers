import ply.yacc as yacc
import ply.lex as lex

# literals = ['=', '+', '-', '*', '/','^', '(', ')','==','!=', '>=','<=','>','<','{','}']

reserved = { 
    'int' : 'INTDEC',
    'float' : 'FLOATDEC',
    'print' : 'PRINT',
    'string': 'STRINGDEC'
 }

tokens = [
    'INUMBER', 'FNUMBER', 'NAME', 'PLUS', 'TIMES', 'LPAREN', 
    'RPAREN', 'MINUS', 'DIVIDE','EQUALS', 'ASSIGN', 'STRING', 
    'NOT_EQUALS', 'M_EQUALS', 'L_EQUALS', 'MORE', 'LESS'
] + list(reserved.values())

t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_EQUALS  = r'=='
t_NOT_EQUALS  = r'!='
t_M_EQUALS = r'>='
t_L_EQUALS = r'<='
t_LESS = r'<'
t_MORE = r'>'
t_ASSIGN = r'='
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
# t_NAME    = r'[a-zA-Z_][a-zA-Z0-9_]*'

# Tokens
def t_NAME(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved.get(t.value,'NAME')    # Check for reserved words
    return t

def t_FNUMBER(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t


def t_INUMBER(t):
    r'\d+'
    t.value = int(t.value)
    return t

def t_STRING(t):
    r'\"([^\\\n]|(\\.))*?\"'
    t.value = t.value.replace("\"","")
    return t

t_ignore = " \t"

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

# Build the lexer
lexer = lex.lex()

# Parsing rules

precedence = (
    ('nonassoc','EQUALS','NOT_EQUALS', 'M_EQUALS', 'L_EQUALS', 'MORE', 'LESS'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('right', 'UMINUS'),
)

# dictionary of names
names = {}
abstractTree = []

def p_statement_declare_int(p):
    '''statement : INTDEC NAME is_assing
    '''
    if type(p[3]) == float:
        print("You can not assing a float to an integer")
    else:
        names[p[2]] = { "type": "INT", "value": p[3]}

def p_statement_declare_float(p):
    'statement : FLOATDEC NAME is_assing'
    names[p[2]] = { "type": "FLOAT", "value":p[3]}

def p_statement_declare_string(p):
    'statement : STRINGDEC NAME is_assing'
    if type(p[3]) == float or type(p[3]) == int:
        print("You need to use quotes("") to declare a string ")
    else:
        names[p[2]] = { "type": "STRING", "value":p[3]}

def p_is_assing(p):
    '''is_assing : ASSIGN expression 
                | '''
    p[0] = 0
    if len(p) > 2:
        p[0] = p[2]

def p_statement_print(p):
    '''statement : PRINT LPAREN expression RPAREN '''
    print(p[3])

def p_statement_assign(p):
    'statement : NAME ASSIGN expression'
    if p[1] not in names:
        print ( "You must declare a variable before using it")
    names[p[1]]["value"] = p[3]


def p_statement_expr(p):
    'statement : expression'
    print(p[1])


def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression EQUALS expression
                  | expression NOT_EQUALS expression
                  | expression M_EQUALS expression
                  | expression L_EQUALS expression
                  | expression MORE expression
                  | expression LESS expression'''
    # p[0] = ('operation',p[1],p[2],p[3])
    if p[2] == '+':
        p[0] = p[1] + p[3]
    elif p[2] == '-':
        p[0] = p[1] - p[3]
    elif p[2] == '*':
        p[0] = p[1] * p[3]
    elif p[2] == '/':
        p[0] = p[1] / p[3]
    elif p[2] == '==':
        p[0] = p[1] == p[3]
    elif p[2] == '!=':
        p[0] = p[1] != p[3]
    elif p[2] == '>':
        p[0] = p[1] > p[3]
    elif p[2] == '<':
        p[0] = p[1] < p[3]
    elif p[2] == '>=':
        p[0] = p[1] >= p[3]
    elif p[2] == '<=':
        p[0] = p[1] <= p[3]

def p_expression_uminus(p):
    "expression : '-' expression %prec UMINUS"
    p[0] = -p[2]


def p_expression_group(p):
    "expression : '(' expression ')'"
    p[0] = p[2]


def p_expression_inumber(p):
    "expression : INUMBER"
    p[0] = p[1]


def p_expression_fnumber(p):
    "expression : FNUMBER"
    p[0] = p[1]

def p_expression_string(p):
    "expression : STRING"
    p[0] = p[1]
    # print(p[0])

def p_expression_name(p):
    "expression : NAME"
    try:
        p[0] = names[p[1]]["value"]
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0


def p_error(p):
    if p:
        print(p)
        print("Syntax error at line '%s' character '%s'" % (p.lineno, p.lexpos) )
    else:
        print("Syntax error at EOF")


parser = yacc.yacc()

while True:
    try:
        s = input('calc > ')
    except EOFError:
        break
    if not s:
        continue
    yacc.parse(s)

# # File
# inputData = []
# with open('data.txt') as file:
#     inputData = file.readlines()

# for data in inputData:
#     yacc.parse(data)