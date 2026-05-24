# %%
import ply.lex as lex
import ply.yacc as yacc
from arbol import Literal, BinaryOp, Program, Assignment, Declaration, Declarations

tokens   = ['ID', 'INTLIT']
t_ignore = ' \t'
literals = '+-*/%(){},;='

def t_ID(t):
    r'[a-zA-Z_][a-zA-Z_0-9]*'
    return t

def t_INTLIT(t):
    r'[0-9]+'
    t.value = int(t.value)
    return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

def t_error(t):
    print(f"Illegal character '{t.value[0]}'")
    t.lexer.skip(1)

def p_Program(p):
    """
    Program : ID ID '(' ')' '{' Declarations Statements '}'
    """
    p[0] = Program(p[6], p[7])

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
    Declaration : ID ID ';'
    """
    p[0] = Declaration(p[2], p[1])

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
    Statement : Assignment
    """
    p[0] = p[1]

def p_Assignment(p):
    """
    Assignment : ID '=' Expression ';'
    """
    p[0] = Assignment(p[1], p[3])

def p_Expression(p):
    """
    Expression : Expression '+' Term
               | Expression '-' Term
               | Term
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = BinaryOp(p[2], p[1], p[3])

def p_Term(p):
    """
    Term : Term '*' Factor
         | Term '/' Factor
         | Term '%' Factor
         | Factor
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = BinaryOp(p[2], p[1], p[3])

def p_Factor(p):
    """
    Factor : INTLIT
           | '(' Expression ')'
    """
    if len(p) == 2:
        p[0] = Literal(p[1], 'INT')
    else:
        p[0] = p[2]

def p_error(p):
    print("Syntax error in input!", p)

lexer  = lex.lex()
parser = yacc.yacc()

# %%
"""
from arbol import Calculator

data   = '10 + 5 * 3'
lexer  = lex.lex()
parser = yacc.yacc()

root = parser.parse(data)
calc = Calculator()
root.accept(calc)

print(calc.stack)
"""
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
        self.stack.append(ir.Constant(intType, node.value))

    def visit_program(self, node: Program) -> None:
        node.decls.accept(self)
        for stmt in node.stmts:
            stmt.accept(self)

    def visit_declaration(self, node: Declaration) -> None:
        self.symbol_table[node.variable] = builder.alloca(intType, name=node.variable)

    def visit_declarations(self, node: Declarations) -> None:
        if node.decls is not None:
            node.decls.accept(self)
        node.decl.accept(self)

    def visit_assignment(self, node: Assignment) -> None:
        node.expression.accept(self)
        tmp = self.stack.pop()
        if node.variable not in self.symbol_table:
            raise KeyError(f"Undeclared variable: {node.variable}")
        else:
            builder.store(tmp, self.symbol_table[node.variable])

    def visit_variable(self, node: Variable) -> None:
        pass

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

data   = """
int main()
{
    int f;
    int i;

    f = 10;
    i = 1;
}
"""

root = parser.parse(data)

print(root)

irgen = IRGenerator()
root.accept(irgen)
#builder.ret(irgen.stack.pop())

print(module)

# %%
print(irgen.stack)
# %%