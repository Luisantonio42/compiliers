import ply.yacc as yacc
import ply.lex as lex

reserved = {
    # datatypes 
    'int' : 'INTDEC',
    'float' : 'FLOATDEC',
    'string': 'STRINGDEC',
    'boolean': 'BOOLEANDEC',

    # booleans
    'true': 'TRUE',
    'false':'FALSE',
    'and': 'AND',
    'or': 'OR',

    # cotrol flow
    'if': 'IF',
    'elif': 'ELIF',
    'else': 'ELSE',

    # print
    'print' : 'PRINT'
 }

tokens = [
    'INUMBER', 'FNUMBER', 'NAME', 'PLUS', 'TIMES','EXP',
    'LPAREN', 'RPAREN', 'MINUS', 'DIVIDE','EQUALS', 'ASSIGN', 
    'STRING', 'NOT_EQUALS', 'M_EQUALS', 'L_EQUALS', 'MORE', 
    'LESS', 'LKEY', 'RKEY', 'FINISH'
] + list(reserved.values())

# arithmetic ops 
t_PLUS    = r'\+'
t_MINUS   = r'-'
t_TIMES   = r'\*'
t_DIVIDE  = r'/'
t_EXP = r'\^'

# comparassion
t_EQUALS  = r'=='
t_NOT_EQUALS  = r'!='
t_M_EQUALS = r'>='
t_L_EQUALS = r'<='
t_LESS = r'<'
t_MORE = r'>'

# assigns
t_ASSIGN = r'='

# block ops
t_LPAREN  = r'\('
t_RPAREN  = r'\)'
t_LKEY = r'{'
t_RKEY = r'}'
t_FINISH = r';'

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
    ('left','AND','OR'),
    ('nonassoc','EQUALS','NOT_EQUALS', 'M_EQUALS', 'L_EQUALS', 'MORE', 'LESS'),
    ('left', 'PLUS', 'MINUS'),
    ('left', 'TIMES', 'DIVIDE'),
    ('left', 'EXP'),
    ('right', 'UMINUS'),
)

# dictionary of names
names = {}
abstractTree = []

# declarations 
def p_statement_declare_var(p):
    'statement : FLOATDEC NAME is_assing'
    names[p[2]] = { "type": "FLOAT", "value":p[3]}

# assigns
def p_is_assing(p):
    '''is_assing : ASSIGN expression 
                | '''
    p[0] = 0
    if len(p) > 2:
        p[0] = p[2]

def p_statement_assign(p):
    'statement : NAME ASSIGN expression'
    if p[1] not in names:
        print ( "You must declare a variable before using it")
    else:
        names[p[1]]["value"] = p[3]

def p_statement_expr(p):
    'statement : expression'
    print("hola",p[1])


# print
def p_statement_print(p):
    '''statement : PRINT LPAREN expression RPAREN '''
    print(p[3])

# Control flow
# def p_statement_if_else(p):
#     '''statement_if_else : if_cond'''
#     p[0] = ('condition', p[1], p[2], p[3])

def p_statement(p):
    '''statement : statement FINISH statement'''
# Operations
def p_expression_binop(p):
    '''expression : expression PLUS expression
                  | expression MINUS expression
                  | expression TIMES expression
                  | expression DIVIDE expression
                  | expression EXP expression
                  | expression EQUALS expression
                  | expression NOT_EQUALS expression
                  | expression M_EQUALS expression
                  | expression L_EQUALS expression
                  | expression MORE expression
                  | expression LESS expression
                  | expression AND expression
                  | expression OR expression'''
    p[0] = ('operation',p[1],p[2],p[3])

# expressions
def p_expression_uminus(p):
    "expression : '-' expression %prec UMINUS"
    p[0] = -p[2]


def p_expression_group(p):
    "expression : '(' expression ')'"
    p[0] = p[2]

def p_types(p):
    '''type : INT
            | FLOAT
            | STRING
            | BOOLEAN'''
    p[0] = p[1]

def p_expression_dec(p):
    """expression : INUMBER
                | FNUMBER
                | STRING
                | boolean_dec"""
    p[0] = p[1]

def p_expression_boolean(p):
    '''boolean_dec : TRUE
              | FALSE'''
    if p[1] == "true":
        p[0] = True
    elif p[1] == "false":
        p[0] = False

def p_expression_name(p):
    "expression : NAME"
    try:
        p[0] = names[p[1]]["value"]
    except LookupError:
        print("Undefined name '%s'" % p[1])
        p[0] = 0

# control flow
# def p_expression_if(p):
#     """if_cond : IF LPAREN expression RPAREN LKEY statement RKEY"""
#     p[0] = ('if', p[3],p[6])

# def p_expression_elif(p):
#     """elif_cond : ELIF LPAREN expression RPAREN LKEY statement RKEY elif_cond"""
#     if len(p) > 2:
#         p[0] = (('elif',p[3], p[6]), ) + p[8]
#     else:
#         p[0] = ()

# def p_expression_else(p):
#     """else_cond : ELSE LKEY statement RKEY"""
#     if len(p) > 2:
#         p[0] = ('else',p[3])

# Errors
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