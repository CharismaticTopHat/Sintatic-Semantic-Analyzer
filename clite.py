import ply.lex as lex
import ply.yacc as yacc

from visitors import Calculator, Literal, BinaryOp

"""
  Program         ⇒  int  main ( ) { Declarations Statements }
  Declarations    ⇒  { Declaration }
  Declaration     ⇒  Type  Identifier  ;
  Type            ⇒  int | bool | float | char
  Statements      ⇒  { Statement }
  Statement       ⇒  ; | Block | Assignment | IfStatement | WhileStatement
  Block           ⇒  { Statements }
  Assignment      ⇒  Identifier = Expression ;
  IfStatement     ⇒  if ( Expression ) Statement [ else Statement ]

  WhileStatement  ⇒  while ( Expression ) Statement  
  Expression      ⇒  Conjunction { || Conjunction }
  Conjunction     ⇒  Equality { && Equality }

  Equality        ⇒  Relation [ EquOp Relation ]
  EquOp           ⇒  == | != 
  Relation        ⇒  Addition [ RelOp Addition ]

  RelOp           ⇒  < | <= | > | >= 

  Addition        ⇒  Term { AddOp Term }
  AddOp           ⇒  + | -
  Term            ⇒  Factor { MulOp Factor }
  MulOp           ⇒  * | / | %
  Factor          ⇒  [ UnaryOp ] Primary
  UnaryOp         ⇒  - | !
  Primary         ⇒  Identifier | IntLit | FloatLit |  ( Expression )
"""

reserved_words = {
    'int': 'INT',
    'while': 'WHILE',
    'return': 'RETURN'
}

tokens = [
    'ID',
    'INTLIT',
    'FLOATLIT',
    'LE',
    'GE',
    'EQ',
    'NEQ',
    'AND',
    'OR',
    'STR'
]+ list(reserved_words.values())

t_LE = r'<='
t_GE = r'>='
t_EQ = r'=='
t_NEQ = r'!='
t_AND = r'&&'
t_OR = r'\|\|'

literals = '!+*/-(){},;=<>%'

def t_ID(t):
    r'[a-z][a-zA-Z0-9]*'
    if t.value in reserved_words:
        t.type = reserved_words[t.value]
    return t

def t_FLOATLIT(t):
    r"\d(_?\d)*.\d(_?\d)*"
    t.value = float(t.value)
    return t

def t_INTLIT(t):
    r"\d(_?\d)*"
    t.value = int(t.value)
    return t

t_ignore=" \t\n"

lexer = lex.lex()

lexer.input("3.15 * r * r")

while True:
    t = lexer.token()
    print(t)
    if t == None:
        break

def p_Program(p):
    """
    Program : INT ID '(' ')' '{' Declarations Statements '}'
    """

def p_Declarations(p):
    """
    Declarations : EPSILON
    """

def p_Statements(p):
    """
    Statements : Statements Statement
        | EPSILON
    """

def p_Statement(p):
    """
    Statement : ';'
        | Block
        | Assignment
        | WhileStatement
    """

def p_WhileStatement(p):
    """
    WhileStatement : WHILE '(' Expression ')' Block
    """

def p_Block(p):
    """
    Block : '{' Statements '}'
    """

def p_EPSILON(p):
    """
    EPSILON :
    """

def p_Assignment(p):
    """
    Assignment : ID '=' Expression ';'
    """

def p_Expression(p):
    """
    Expression : Expression OR Conjunction
        | Conjunction
    """

def p_Conjunction(p):
    """
    Conjunction : Conjunction AND Equality
        | Equality
    """

def p_Equality(p):
    """
    Equality : Equality EquOp Relation
        | Relation
    """

def p_EquOp(p):
    """
    EquOp : EQ
        | NEQ
    """

def p_Relation(p):
    """
    Relation : Relation RelOp Addition
        | Addition
    """

def p_RelOp(p):
    """
    RelOp : '<' 
        | LE 
        | '>' 
        | GE
    """

def p_Addition(p):
    """
    Addition : Addition AddOp Term
        | Term
    """

def p_AddOp(p):
    """
    AddOp : '+' 
        | '-'
    """

def p_Term(p):
    """
    Term : Term MulOp Factor
        | Factor
    """

def p_Primary(p):
    """
    Primary : ID 
        | INTLIT 
        | FLOATLIT 
        | '(' Expression ')'
    """

def p_UnaryOp(p):
    """
    UnaryOp : '-'
        | '!'
    """

def p_MulOp(p):
    """
    MulOp : '*' 
        | '/' 
        | '%'
    """

def p_Factor(p):
    """
    Factor : UnaryOp Primary
        | Primary
    """

program = """
int factorial(){
    f = 1;
    i = 1;
    while (i<n){
        f = f*i
        i=i+1;
    }
}
"""

parser = yacc.yacc()

print(parser.parse(program))