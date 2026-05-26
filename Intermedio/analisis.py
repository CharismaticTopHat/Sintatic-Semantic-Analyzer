# %%
import ply.lex as lex
import ply.yacc as yacc
from arbol import (Literal, BinaryOp, UnaryOp, Program, Assignment, Parameter, Function, Call,
                   Declaration, Declarations, 
                   IfStatement, WhileStatement, SwitchStatement, 
                   Case,Variable, Block)

# Tokens

reserved_words = {
    'int':   'INT_TYPE',
    'bool':  'BOOL_TYPE',
    'float': 'FLOAT_TYPE',
    'char':  'CHAR_TYPE',
    'void':  'VOID_TYPE',
    'if':    'IF',
    'else':  'ELSE',
    'while': 'WHILE',
    'true':  'TRUE',
    'false': 'FALSE',
    'return':'RETURN',
    'switch':'SWITCH',
    'case':  'CASE',
    'default':'DEFAULT',
    'break': 'BREAK',
}

tokens = ['ID', 'INTLIT', 'FLOATLIT', 'CHARLIT',
          'INT_TYPE', 'BOOL_TYPE', 'FLOAT_TYPE', 'CHAR_TYPE', 'VOID_TYPE', 
          'IF', 'ELSE', 'WHILE',
          'OR', 'AND', 'EQ', 'DIF',
          'G', 'GE','L','LE',
          'ADD','SUB','MUL','DIV','MOD','EXC',
          'TRUE','FALSE','RETURN',
          'SWITCH','CASE','DEFAULT','BREAK']

t_OR=r'\|\|'
t_AND=r'\&\&'
t_EQ=r'\=\='
t_DIF=r'\!\='
t_GE = r'>='
t_G  = r'>'
t_LE = r'<='
t_L  = r'<'
t_ADD = r'\+'
t_SUB = r'-'
t_MUL = r'\*'
t_DIV = r'/'
t_MOD = r'%'
t_EXC = '!'
t_ignore = ' \t'
literals = '(){},;=:'

# 

def t_FLOATLIT(t):
    r'\d+\.\d+'
    t.value = float(t.value)
    return t

def t_INTLIT(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    t.type = reserved_words.get(t.value, 'ID')
    return t

def t_CHARLIT(t):
    r"'[^'\\]'"
    t.value = t.value[1]
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

# Statements, Declarations & Programs

def p_Program(p):
    """
    Program : FunctionList MainFunction
    """
    p[0] = p[2] 

def p_Declarations(p):
    """
    Declarations : Declarations Declaration
                 | Declaration
    """
    if len(p) == 2:
        p[0] = Declarations(None, p[1])
    else:
        p[0] = Declarations(p[1], p[2])

def p_Declaration(p):
    """
    Declaration : Type Identifier ';'
    """
    p[0] = Declaration(p[2].name, p[1])

def p_Type(p):
    """
    Type : INT_TYPE
         | BOOL_TYPE
         | FLOAT_TYPE
         | CHAR_TYPE
         | VOID_TYPE
    """
    p[0] = p[1]

def p_Block(p):
    """
    Block : '{' Statements '}'
    """
    p[0] = Block(p[2])

def p_Statements(p):
    """
    Statements : Statements Statement
               | Statement
    """
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

def p_Statement(p):
    """
    Statement : Block
              | Assignment
              | IfStatement
              | WhileStatement
              | CallStatement
              | ReturnStatement
              | SwitchStatement
    """
    p[0] = p[1]

def p_Assignment(p):
    """
    Assignment : Identifier '=' Expression ';'
    """
    p[0] = Assignment(p[1].name, p[3])

# Operators

def p_Expression(p):
    """
    Expression : Expression OR Conjunction
               | Conjunction
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = BinaryOp(p[2], p[1], p[3])

def p_Conjunction(p):
    """
    Conjunction : Conjunction AND Equality
                | Equality
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = BinaryOp(p[2], p[1], p[3])

def p_Equality(p):
    """
    Equality : Relation EquOp Relation
            | Relation
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = BinaryOp(p[2], p[1], p[3])

def p_EquOp(p):
    """
    EquOp : EQ
          | DIF
    """
    p[0] = p[1]

def p_Relation(p):
    """
    Relation : Addition
             | Addition RelOp Addition
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = BinaryOp(p[2], p[1], p[3])

def p_RelOp(p):
    """
    RelOp : G
          | GE
          | L
          | LE
    """
    p[0] = p[1]

def p_Addition(p):
    """
    Addition : Addition AddOp Term
             | Term
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = BinaryOp(p[2], p[1], p[3])

def p_AddOp(p):
    """
    AddOp : ADD
          | SUB
    """
    p[0] = p[1]

def p_IfStatement(p):
    """
    IfStatement : IF '(' Expression ')' Statement 
                | IF '(' Expression ')' Statement ELSE Statement
    """
    if len(p) == 6:
        p[0] = IfStatement(p[3], p[5], None)
    else:
        p[0] = IfStatement(p[3], p[5], p[7])

def p_WhileStatement(p):
    """
    WhileStatement : WHILE '(' Expression ')' Statement
    """
    p[0] = WhileStatement(p[3], p[5])

def p_SwitchStatement(p):
    """
    SwitchStatement : SWITCH '(' Expression ')' '{' CaseList DefaultCase '}'
    """
    p[0] = SwitchStatement(p[3], p[6], p[7])

def p_CaseList(p):
    """
    CaseList : CaseList Case
             | Case
    """
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

def p_Case(p):
    """
    Case : CASE Literal ':' Statements BREAK ';'
    """
    p[0] = Case(p[2], p[4])

def p_DefaultCase(p):
    """
    DefaultCase : DEFAULT ':' Statements
                | empty
    """
    if p[1] == []:
        p[0] = None
    else:
        p[0] = p[3]

def p_Term(p):
    """
    Term : Term MulOp Factor
         | Factor
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = BinaryOp(p[2], p[1], p[3])

def p_MulOp(p):
    """
    MulOp : MUL
          | DIV
          | MOD
    """
    p[0] = p[1]

def p_Factor(p):
    """
    Factor : UnaryOp Primary
           | Primary
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = UnaryOp(p[1], p[2])

def p_UnaryOp(p):
    """
    UnaryOp : SUB
            | EXC
    """
    p[0] = p[1]

def p_Primary(p):
    """
    Primary : Identifier
            | Literal
            | '(' Expression ')'
            | Call
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[2]

# Declarations

def p_Identifier(p):
    """
    Identifier : ID
    """
    p[0] = Variable(p[1], 'ID')

def p_Literal(p):
    """
    Literal : Integer
            | Float
            | Boolean
            | Char
    """
    p[0] = p[1]

def p_Integer(p):
    """
    Integer : INTLIT
    """
    p[0] = Literal(p[1], 'INT')

def p_Boolean(p):
    """
    Boolean : TRUE
            | FALSE
    """
    p[0] = Literal(p[1] == 'true', 'BOOL')

def p_Float(p):
    """
    Float : FLOATLIT
    """
    p[0] = Literal(p[1], 'FLOAT')

def p_Char(p):
    """
    Char : CHARLIT
    """
    p[0] = Literal(p[1], 'CHAR')

# Functions

def p_FunctionOrGlobal(p):
    """
    FunctionOrGlobal : '(' Parameters ')' '{' Declarations Statements '}'
                     | '(' ')' '{' Declarations Statements '}'
                     | Global
    """
    if len(p) == 2:
        p[0] = ('global', p[1])
    elif len(p) == 8:
        p[0] = Function('', [], p[4], p[5])
    else:
        p[0] = Function('', p[2], p[5], p[6])

def p_Parameters(p):
    """
    Parameters : Parameters ',' Parameter
               | Parameter
               | empty
    """
    if len(p) == 2:
        if p[1] == []:
            p[0] = []
        else:
            p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_Parameter(p):
    """
    Parameter : Type Identifier
    """
    p[0] = Parameter(p[2].name, p[1])

def p_Global(p):
    """
    Global : GlobalList ';'
    """
    p[0] = p[1]

def p_GlobalList(p):
    """
    GlobalList : GlobalList ',' Identifier
               | Identifier
    """
    if len(p) == 2:
        p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_FunctionList(p):
    """
    FunctionList : FunctionList Type Identifier FunctionOrGlobal
                 | empty
    """
    if len(p) == 2:
        p[0] = []
    else:
        p[0] = p[1] + [(p[2], p[3], p[4])]

def p_MainFunction(p):
    """
    MainFunction : INT_TYPE ID '(' ')' '{' Declarations Statements '}'
    """
    p[0] = Program(p[6], p[7])

def p_CallStatement(p):
    """
    CallStatement : Call ';'
    """
    p[0] = p[1]

def p_ReturnStatement(p):
    """
    ReturnStatement : RETURN Expression ';'
                    | RETURN ';'
    """
    if len(p) == 4:
        p[0] = ('return', p[2])
    else:
        p[0] = ('return', None)

def p_Call(p):
    """
    Call : Identifier '(' Arguments ')'
    """
    p[0] = Call(p[1].name, p[3])

def p_Arguments(p):
    """
    Arguments : Arguments ',' Expression
              | Expression
              | empty
    """
    if len(p) == 2:
        if p[1] == []:
            p[0] = []
        else:
            p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[3]]

def p_empty(p):
    """
    empty :
    """
    p[0] = []

def p_error(p):
    print("Syntax error in input!", p)

lexer  = lex.lex()
parser = yacc.yacc()

# %%
from arbol import Visitor, Variable
from llvmlite import ir

intType = ir.IntType(32)
module  = ir.Module(name="prog")

fnty    = ir.FunctionType(intType, [])
func    = ir.Function(module, fnty, name='main')
entry   = func.append_basic_block('entry')
builder = ir.IRBuilder(entry)

class IRGenerator(Visitor):
    def __init__(self):
        self.stack = []
        self.symbol_table = {}

    def visit_literal(self, node: Literal) -> None:
        type_map = {
            'INT':   ir.IntType(32),
            'FLOAT': ir.FloatType(),
            'BOOL':  ir.IntType(1),
            'CHAR':  ir.IntType(8),
        }
        llvm_type = type_map.get(node.type, ir.IntType(32))
        self.stack.append(ir.Constant(llvm_type, node.value))

    def visit_program(self, node: Program) -> None:
        node.decls.accept(self)
        for stmt in node.stmts:
            stmt.accept(self)
        builder.ret(ir.Constant(intType, 0))

    def visit_declaration(self, node: Declaration) -> None:
        type_map = {
            'int':   ir.IntType(32),
            'float': ir.FloatType(),
            'bool':  ir.IntType(1),
            'char':  ir.IntType(8),
        }
        llvm_type = type_map.get(node.type, ir.IntType(32))
        self.symbol_table[node.variable] = builder.alloca(llvm_type, name=node.variable)

    def visit_declarations(self, node: Declarations) -> None:
        if node.decls is not None:
            node.decls.accept(self)
        node.decl.accept(self)

    def visit_assignment(self, node: Assignment) -> None:
        node.assignment.accept(self)
        tmp = self.stack.pop()
        if node.variable not in self.symbol_table:
            raise KeyError(f"Undeclared variable: {node.variable}")
        else:
            builder.store(tmp, self.symbol_table[node.variable])
            
    def visit_variable(self, node: Variable) -> None:
        if node.name not in self.symbol_table:
            raise KeyError(f"Undeclared variable: {node.name}")
        val = builder.load(self.symbol_table[node.name], name=node.name)
        self.stack.append(val)
    
    def visit_block(self, node: Block) -> None:
        for stmt in node.stmts:
            stmt.accept(self)
    
    def visit_function(self, node: Function) -> None:
        for param in node.params:
            param.accept(self)

        node.decls.accept(self)

        for stmt in node.stmts:
            stmt.accept(self)

    def visit_parameter(self, node: Parameter) -> None:
        type_map = {
            'int': ir.IntType(32),
            'float': ir.FloatType(),
            'bool': ir.IntType(1),
            'char': ir.IntType(8),
        }

        llvm_type = type_map.get(node.type, ir.IntType(32))

        self.symbol_table[node.name] = builder.alloca(
            llvm_type,
            name=node.name
        )

    def visit_case(self, node):
        pass

    def visit_call(self, note):
        self.stack.append(ir.Constant(intType, 0))

    def visit_unary_op(self, node: UnaryOp) -> None:
        node.operand.accept(self)

        operand = self.stack.pop()

        if node.op == '-':
            self.stack.append(builder.neg(operand))

        elif node.op == '!':
            zero = ir.Constant(operand.type, 0)
            result = builder.icmp_signed('==', operand, zero)
            self.stack.append(result)

    def visit_binary_op(self, node: BinaryOp) -> None:
        node.lhs.accept(self)
        node.rhs.accept(self)
        rhs = self.stack.pop()
        lhs = self.stack.pop()
        
        if node.op == '+':
            self.stack.append(builder.add(lhs, rhs))
        elif node.op == '-':
            self.stack.append(builder.sub(lhs, rhs))
        elif node.op == '*':
            self.stack.append(builder.mul(lhs,rhs))
        elif node.op == '/':
            self.stack.append(builder.sdiv(lhs,rhs))
        elif node.op == "%":
            self.stack.append(builder.srem(lhs, rhs))
        elif node.op == '==':
            self.stack.append(builder.icmp_signed('==', lhs, rhs))
        elif node.op == '!=':
            self.stack.append(builder.icmp_signed('!=', lhs, rhs))
        elif node.op == '<':
            self.stack.append(builder.icmp_signed('<', lhs, rhs))
        elif node.op == '<=':
            self.stack.append(builder.icmp_signed('<=', lhs, rhs))
        elif node.op == '>':
            self.stack.append(builder.icmp_signed('>', lhs, rhs))
        elif node.op == '>=':
            self.stack.append(builder.icmp_signed('>=', lhs, rhs))
        elif node.op == '&&':
            self.stack.append(builder.and_(lhs, rhs))
        elif node.op == '||':
            self.stack.append(builder.or_(lhs, rhs))
            
    def visit_while_statement(self, node: WhileStatement) -> None:
        cond_block  = func.append_basic_block('while_cond')
        body_block  = func.append_basic_block('while_body')
        merge_block = func.append_basic_block('while_merge')

        builder.branch(cond_block)

        builder.position_at_end(cond_block)
        node.condition.accept(self)
        cond      = self.stack.pop()
        cond_bool = builder.icmp_signed('!=', cond, ir.Constant(intType, 0))
        builder.cbranch(cond_bool, body_block, merge_block)

        builder.position_at_end(body_block)
        node.body.accept(self)
        builder.branch(cond_block)

        builder.position_at_end(merge_block)

    def visit_if_statement(self, node: IfStatement) -> None:
        node.condition.accept(self)
        cond = self.stack.pop()
        cond_bool = builder.icmp_signed('!=', cond, ir.Constant(intType, 0))

        if node.else_stmt is None:
            then_block  = func.append_basic_block('then')
            merge_block = func.append_basic_block('merge')

            builder.cbranch(cond_bool, then_block, merge_block)

            builder.position_at_end(then_block)
            node.then_stmt.accept(self)
            builder.branch(merge_block)

            builder.position_at_end(merge_block)
        else:
            then_block  = func.append_basic_block('then')
            else_block  = func.append_basic_block('else')
            merge_block = func.append_basic_block('merge')

            builder.cbranch(cond_bool, then_block, else_block)

            builder.position_at_end(then_block)
            node.then_stmt.accept(self)
            builder.branch(merge_block)

            builder.position_at_end(else_block)
            
            node.else_stmt.accept(self)
            builder.branch(merge_block)

            builder.position_at_end(merge_block)
    
    def visit_switch_statement(self, node: SwitchStatement):

        node.expression.accept(self)
        switch_value = self.stack.pop()

        end_block = func.append_basic_block('switch_end')
        default_block = func.append_basic_block('default')

        switch_inst = builder.switch(
            switch_value,
            default_block
        )

        case_blocks = []

        for case in node.cases:

            block = func.append_basic_block('case')
            case_blocks.append((case, block))

            case.value.accept(self)
            case_value = self.stack.pop()

            switch_inst.add_case(case_value, block)

        for case, block in case_blocks:

            builder.position_at_end(block)

            for stmt in case.stmts:
                stmt.accept(self)

            builder.branch(end_block)

        builder.position_at_end(default_block)

        if node.default is not None:
            for stmt in node.default:
                stmt.accept(self)

        builder.branch(end_block)

        builder.position_at_end(end_block)

# Fibonacci

data1 = """
int fibonacci(int n)
{
    int result;

    if (n <= 1)
    {
        result = n;
    }
    else
    {
        result = fibonacci(n - 1) + fibonacci(n - 2);
    }

    return result;
}
"""

root = parser.parse(data1)
print(root)
irgen = IRGenerator()
root.accept(irgen)
print(module)

# %%
print(irgen.stack)
# %%