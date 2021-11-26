import ply.yacc as yacc
import ply.lex as lex

commands = []

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
    'for': 'FOR',
    'while': 'WHILE',

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

def p_start(p):
    '''start : statement'''
    global commands
    commands = p[1]

def p_statement(p):
    '''statement : statement_print FINISH statement
                | statement_declare FINISH statement
                | statement_declare_assign FINISH statement
                | statement_assign FINISH statement
                | statement_condition statement
                | statement_for statement
                | statement_while statement
                | empty'''
    if len(p) > 2:
        if p[2] == ';':
            p[2] = p[3]
        p[0] = (p[1],) + p[2]
    else:
        p[0]=()
        
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

# print
def p_statement_print(p):
    '''statement_print : PRINT expression '''
    p[0] = ('print', p[2])
    # print(p[0])

# expressions
def p_expression_uminus(p):
    "expression : MINUS expression %prec UMINUS"
    p[0] = -p[2]


def p_expression_group(p):
    "expression : LPAREN expression RPAREN"
    p[0] = p[2]

def p_types(p):
    '''type : INTDEC
            | FLOATDEC
            | STRINGDEC
            | BOOLEANDEC'''
    p[0] = p[1]

# Nedded to create a empty production to avoid infinte loops
def p_empty(p):
    'empty :'
    pass

def p_expression_dec(p):
    """expression : FNUMBER
                | INUMBER
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
    p[0] = p[1]

# declaration statements 
def p_statement_declare(p):
    'statement_declare : type NAME'
    p[0] = ('declare', p[1] ,p[2])

def p_statement_declare_assign(p):
    'statement_declare_assign : type NAME ASSIGN expression'
    p[0] = ('declare assign', p[1],p[2],p[4])

def p_statement_assign(p):
    'statement_assign : NAME ASSIGN expression'
    p[0] = ('assign', p[1],p[3])

# Control flow statements
# IF - ELIF - ELSE
def p_statement_condition(p):
    '''statement_condition : if_condition elif_condition else_condition'''
    
    # p[1] = if
    # p[2] = elif
    # p[3] = else
    p[0] = ('condition', p[1], p[2], p[3])

# FOR
def p_statement_for(p):
    '''statement_for : FOR LPAREN statement_declare_assign FINISH expression FINISH statement_assign RPAREN LKEY statement RKEY
                    | empty'''
    
    # p[3] = int i = 0
    # p[3] = i < a
    # p[5] = i+=1
    # p[5] = print(i)
    p[0] = ('for', p[3], p[5], p[7], p[10])

# WHILE
def p_statement_while(p):
    '''statement_while : WHILE LPAREN expression RPAREN LKEY statement RKEY'''
    
    # p[3] = b <= a
    # p[6] = print(i) b++
    p[0] = ('while', p[3], p[6])

# control flow if- elif - else
def p_expression_if(p):
    """if_condition : IF LPAREN expression RPAREN LKEY statement RKEY"""
    p[0] = ('if', p[3], p[6])

def p_expression_elif(p):
    """elif_condition : ELIF LPAREN expression RPAREN LKEY statement RKEY elif_condition
                    | empty"""
    if len(p) > 2:
        p[0] = (('elif',p[3], p[6]), ) + p[8]
    else:
        p[0] = ()

def p_expression_else(p):
    """else_condition : ELSE LKEY statement RKEY
                | empty"""
    if len(p) > 2:
        p[0] = ('else', p[3])

# Errors
def p_error(p):
    if p:
        print(p)
        print("Syntax error at line '%s' character '%s' = '%s' " % (p.lineno, p.lexpos, p.value))
    else:
        print("Syntax error at EOF")


parser = yacc.yacc()

# while True:
#     try:
#         s = input('calc > ')
#     except EOFError:
#         break
#     if not s:
#         continue
#     yacc.parse(s)

# File
inputData = []
file = open("data.txt", "r")
s = file.read()
yacc.parse(s)


# ------------------- THREE WAY CODE ------------------- #

results = open("results.txt", "w")
print("total commands: ",len(commands))
print("Comandss -----------------------")
for i in commands:
    print(i)

label_cont = 1
var_cont = 1

def parse_commands(command):
    
    if type(command) is not tuple:
        return command

    # print(command[0])
    if command[0] == 'declare':
        var_type = command[1]
        name = command[2]
        return '{}dec ({})'.format(var_type, name)

    elif command[0] == 'declare assign':
        var_type = command[1]
        name = command[2]
        res = ''
        res += '{}dec ({})\n'.format(var_type, name)
        tmp_parse = parse_commands(command[3])
        res += '{} := {}\n'.format(name, command[3])
        return res
    
    elif command[0] == 'assign':
        name = command[1]
        tmp_parse = parse_commands(command[2])
        return '{} := {}\n'.format(name, tmp_parse)
    
    elif command[0] == 'print':
        return 'print ({})'.format(parse_commands(command[0]))

    elif command[0] == 'operation':
        global label_cont
        result = ''
        result += 't{} '.format(label_cont)
        label_cont += 1
        result += '{} {} {}'.format(parse_commands(command[1]),command[2],parse_commands(command[3]))

    elif command[0] == 'if':
        return
        
    elif command[0] == 'condition':
        return
    
    elif command[0] == 'for':
        return
    
    elif command[0] == 'while':
        return
    
    else:
        return 'Error'

for command in commands:
    print(parse_commands(command))