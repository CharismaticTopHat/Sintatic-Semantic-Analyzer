# %%
import ply.lex as lex
import ply.yacc as yacc
from arbol import (Literal, BinaryOp, UnaryOp, Program, Assignment, Parameter, Call, Function,
                   Declaration, Declarations, 
                   IfStatement, WhileStatement, SwitchStatement, ReturnStatement, 
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
    'main':  'MAIN'
}

tokens = ['ID', 'INTLIT', 'FLOATLIT', 'CHARLIT',
          'INT_TYPE', 'BOOL_TYPE', 'FLOAT_TYPE', 'CHAR_TYPE', 'VOID_TYPE', 
          'IF', 'ELSE', 'WHILE',
          'OR', 'AND', 'EQ', 'DIF',
          'G', 'GE','L','LE',
          'ADD','SUB','MUL','DIV','MOD','EXC',
          'TRUE','FALSE','RETURN',
          'SWITCH','CASE','DEFAULT','BREAK', 'MAIN']

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

precedence = (
    ('nonassoc', 'IFX'),
    ('nonassoc', 'ELSE'),
)

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
    p[0] = Program(p[1], p[2])

def p_FunctionList(p):
    """
    FunctionList : FunctionList Function
                 | empty
    """
    if len(p) == 2:
        p[0] = p[1]
    else:
        p[0] = p[1] + [p[2]]

def p_Declarations(p):
    """
    Declarations : Declarations Declaration
                 | Declaration
                 | empty
    """
    if len(p) == 2:
        if p[1] == []:
            p[0] = []
        else:
            p[0] = [p[1]]
    else:
        p[0] = p[1] + [p[2]]

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
    Block : '{' Declarations Statements '}'
    """
    p[0] = Block(p[2],p[3])

def p_Statements(p):
    """
    Statements : Statements Statement
               | Statement
               | empty
    """
    if len(p) == 2:
        if p[1] == []:
            p[0] = []
        else:
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
    IfStatement : IF '(' Expression ')' Statement %prec IFX
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
    FunctionOrGlobal : '(' Parameters ')' Block
                     | '(' ')' Block
                     | Global
    """
    if len(p) == 2:
        p[0] = ("global", p[1])
    elif len(p) == 4:
        p[0] = ("function", [], p[3])
    else:
        p[0] = ("function", p[2], p[4])

def p_Function(p):
    """
    Function : Type Identifier FunctionOrGlobal
    """

    kind = p[3][0]

    if kind == "function":

        params = p[3][1]
        block  = p[3][2]

        p[0] = Function(
            p[2].name,
            params,
            block.decls,
            block.stmts
        )

    else:
        p[0] = None

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
    Global : ',' Identifier ';'
    """
    p[0] = p[1]

def p_MainFunction(p):
    """
    MainFunction : INT_TYPE MAIN '(' ')' Block
    """
    p[0] = Function("main",[],p[5].decls,p[5].stmts)

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
        p[0] = ReturnStatement(p[2])
    else:
        p[0] = ReturnStatement(None)

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
from arbol import (
    Visitor,
    Variable,
    Literal,
    BinaryOp,
    UnaryOp,
    Program,
    Assignment,
    Parameter,
    Call,
    Function,
    Declaration,
    Declarations,
    IfStatement,
    WhileStatement,
    SwitchStatement,
    ReturnStatement,
    Case,
    Block
)

from llvmlite import ir


class IRGenerator(Visitor):

    def __init__(self):

        # =====================================================
        # LLVM MODULE
        # =====================================================

        self.module = ir.Module(name="prog")

        # =====================================================
        # COMMON TYPES
        # =====================================================

        self.intType   = ir.IntType(32)
        self.boolType  = ir.IntType(1)
        self.charType  = ir.IntType(8)
        self.floatType = ir.FloatType()
        self.voidType  = ir.VoidType()

        # =====================================================
        # CURRENT STATE
        # =====================================================

        self.builder = None
        self.current_function = None

        # =====================================================
        # SYMBOLS / FUNCTIONS
        # =====================================================

        self.symbol_table = {}
        self.function_table = {}

        # =====================================================
        # EXPRESSION STACK
        # =====================================================

        self.stack = []

        # =====================================================
        # printf SUPPORT
        # =====================================================

        voidptr_ty = ir.IntType(8).as_pointer()

        printf_ty = ir.FunctionType(
            ir.IntType(32),
            [voidptr_ty],
            var_arg=True
        )

        self.printf = ir.Function(
            self.module,
            printf_ty,
            name="printf"
        )

    # =========================================================
    # UTILITY FUNCTIONS
    # =========================================================

    def get_llvm_type(self, type_name):

        type_map = {
            'int': self.intType,
            'INT': self.intType,

            'bool': self.boolType,
            'BOOL': self.boolType,

            'char': self.charType,
            'CHAR': self.charType,

            'float': self.floatType,
            'FLOAT': self.floatType,

            'void': self.voidType
        }

        return type_map.get(type_name, self.intType)

    def create_global_string(self, text, name="str"):

        text += '\0'

        string_type = ir.ArrayType(
            ir.IntType(8),
            len(text)
        )

        string_const = ir.Constant(
            string_type,
            bytearray(text.encode("utf8"))
        )

        global_var = ir.GlobalVariable(
            self.module,
            string_type,
            name=name
        )

        global_var.linkage = 'internal'
        global_var.global_constant = True
        global_var.initializer = string_const

        return global_var

    # =========================================================
    # PROGRAM
    # =========================================================

    def visit_program(self, node: Program) -> None:

        # ---------------------------------------------
        # FIRST PASS:
        # CREATE LLVM FUNCTION SIGNATURES
        # ---------------------------------------------

        if node.functions:

            for func in node.functions:

                if func is not None:

                    param_types = []

                    for param in func.params:
                        param_types.append(
                            self.get_llvm_type(param.type)
                        )

                    func_type = ir.FunctionType(
                        self.intType,
                        param_types
                    )

                    llvm_func = ir.Function(
                        self.module,
                        func_type,
                        name=func.name
                    )

                    self.function_table[func.name] = llvm_func

        # MAIN FUNCTION

        main_type = ir.FunctionType(
            self.intType,
            []
        )

        llvm_main = ir.Function(
            self.module,
            main_type,
            name="main"
        )

        self.function_table["main"] = llvm_main

        # ---------------------------------------------
        # SECOND PASS:
        # GENERATE FUNCTION BODIES
        # ---------------------------------------------

        if node.functions:

            for func in node.functions:

                if func is not None:
                    func.accept(self)

        node.main.accept(self)

    # =========================================================
    # FUNCTION
    # =========================================================

    def visit_function(self, node: Function) -> None:

        llvm_func = self.function_table[node.name]

        self.current_function = llvm_func

        entry_block = llvm_func.append_basic_block('entry')

        self.builder = ir.IRBuilder(entry_block)

        # NEW SYMBOL TABLE PER FUNCTION
        self.symbol_table = {}

        # ---------------------------------------------
        # STORE PARAMETERS
        # ---------------------------------------------

        for i, param in enumerate(node.params):

            llvm_type = self.get_llvm_type(param.type)

            ptr = self.builder.alloca(
                llvm_type,
                name=param.variable
            )

            self.builder.store(
                llvm_func.args[i],
                ptr
            )

            self.symbol_table[param.variable] = ptr

        # ---------------------------------------------
        # DECLARATIONS
        # ---------------------------------------------

        for decl in node.decls:
            decl.accept(self)

        # ---------------------------------------------
        # STATEMENTS
        # ---------------------------------------------

        for stmt in node.stmts:
            stmt.accept(self)

        # ---------------------------------------------
        # DEFAULT RETURN
        # ---------------------------------------------

        if not self.builder.block.is_terminated:

            self.builder.ret(
                ir.Constant(self.intType, 0)
            )

    # =========================================================
    # DECLARATIONS
    # =========================================================

    def visit_declaration(self, node: Declaration) -> None:

        llvm_type = self.get_llvm_type(node.type)

        self.symbol_table[node.variable] = self.builder.alloca(
            llvm_type,
            name=node.variable
        )

    # =========================================================
    # VARIABLES
    # =========================================================

    def visit_variable(self, node: Variable) -> None:

        if node.name not in self.symbol_table:
            raise KeyError(f"Undeclared variable: {node.name}")

        value = self.builder.load(
            self.symbol_table[node.name],
            name=node.name
        )

        self.stack.append(value)

    # =========================================================
    # LITERALS
    # =========================================================

    def visit_literal(self, node: Literal) -> None:

        llvm_type = self.get_llvm_type(node.type)

        constant = ir.Constant(
            llvm_type,
            node.value
        )

        self.stack.append(constant)

    # =========================================================
    # ASSIGNMENT
    # =========================================================

    def visit_assignment(self, node: Assignment) -> None:

        node.assignment.accept(self)

        value = self.stack.pop()

        if node.variable not in self.symbol_table:
            raise KeyError(
                f"Undeclared variable: {node.variable}"
            )

        self.builder.store(
            value,
            self.symbol_table[node.variable]
        )

    # =========================================================
    # FUNCTION CALLS
    # =========================================================

    def visit_call(self, node: Call) -> None:

        if node.name == "printf":

            fmt = self.create_global_string(
                "%i\n",
                "fmt"
            )

            fmt_ptr = self.builder.bitcast(
                fmt,
                ir.IntType(8).as_pointer()
            )

            node.args[0].accept(self)

            value = self.stack.pop()

            self.builder.call(
                self.printf,
                [fmt_ptr, value]
            )

            self.stack.append(
                ir.Constant(self.intType, 0)
            )

            return

        llvm_func = self.function_table[node.name]

        args = []

        for arg in node.args:

            arg.accept(self)

            args.append(
                self.stack.pop()
            )

        result = self.builder.call(
            llvm_func,
            args
        )

        self.stack.append(result)

    # =========================================================
    # UNARY OPERATIONS
    # =========================================================

    def visit_unary_op(self, node: UnaryOp) -> None:

        node.operand.accept(self)

        operand = self.stack.pop()

        if node.op == '-':

            result = self.builder.neg(operand)

            self.stack.append(result)

        elif node.op == '!':

            zero = ir.Constant(
                operand.type,
                0
            )

            result = self.builder.icmp_signed(
                '==',
                operand,
                zero
            )

            self.stack.append(result)

    # =========================================================
    # BINARY OPERATIONS
    # =========================================================

    def visit_binary_op(self, node: BinaryOp) -> None:

        node.lhs.accept(self)
        node.rhs.accept(self)

        rhs = self.stack.pop()
        lhs = self.stack.pop()

        if node.op == '+':

            result = self.builder.add(lhs, rhs)

        elif node.op == '-':

            result = self.builder.sub(lhs, rhs)

        elif node.op == '*':

            result = self.builder.mul(lhs, rhs)

        elif node.op == '/':

            result = self.builder.sdiv(lhs, rhs)

        elif node.op == '%':

            result = self.builder.srem(lhs, rhs)

        elif node.op == '==':

            result = self.builder.icmp_signed(
                '==',
                lhs,
                rhs
            )

        elif node.op == '!=':

            result = self.builder.icmp_signed(
                '!=',
                lhs,
                rhs
            )

        elif node.op == '<':

            result = self.builder.icmp_signed(
                '<',
                lhs,
                rhs
            )

        elif node.op == '<=':

            result = self.builder.icmp_signed(
                '<=',
                lhs,
                rhs
            )

        elif node.op == '>':

            result = self.builder.icmp_signed(
                '>',
                lhs,
                rhs
            )

        elif node.op == '>=':

            result = self.builder.icmp_signed(
                '>=',
                lhs,
                rhs
            )

        elif node.op == '&&':

            result = self.builder.and_(lhs, rhs)

        elif node.op == '||':

            result = self.builder.or_(lhs, rhs)

        self.stack.append(result)

    # =========================================================
    # BLOCK
    # =========================================================

    def visit_block(self, node: Block) -> None:

        for decl in node.decls:
            decl.accept(self)

        for stmt in node.stmts:
            stmt.accept(self)

    # =========================================================
    # IF STATEMENT
    # =========================================================

    def visit_if_statement(self, node: IfStatement) -> None:

        node.condition.accept(self)

        cond = self.stack.pop()

        cond_bool = self.builder.icmp_signed(
            '!=',
            cond,
            ir.Constant(self.intType, 0)
        )

        if node.else_stmt is None:

            then_block = self.current_function.append_basic_block(
                'then'
            )

            merge_block = self.current_function.append_basic_block(
                'merge'
            )

            self.builder.cbranch(
                cond_bool,
                then_block,
                merge_block
            )

            # THEN

            self.builder.position_at_end(then_block)

            node.then_stmt.accept(self)

            if not self.builder.block.is_terminated:
                self.builder.branch(merge_block)

            # MERGE

            self.builder.position_at_end(merge_block)

        else:

            then_block = self.current_function.append_basic_block(
                'then'
            )

            else_block = self.current_function.append_basic_block(
                'else'
            )

            merge_block = self.current_function.append_basic_block(
                'merge'
            )

            self.builder.cbranch(
                cond_bool,
                then_block,
                else_block
            )

            # THEN

            self.builder.position_at_end(then_block)

            node.then_stmt.accept(self)

            if not self.builder.block.is_terminated:
                self.builder.branch(merge_block)

            # ELSE

            self.builder.position_at_end(else_block)

            node.else_stmt.accept(self)

            if not self.builder.block.is_terminated:
                self.builder.branch(merge_block)

            # MERGE

            self.builder.position_at_end(merge_block)

    # =========================================================
    # WHILE
    # =========================================================

    def visit_while_statement(self, node: WhileStatement) -> None:

        cond_block = self.current_function.append_basic_block(
            'while_cond'
        )

        body_block = self.current_function.append_basic_block(
            'while_body'
        )

        merge_block = self.current_function.append_basic_block(
            'while_merge'
        )

        self.builder.branch(cond_block)

        # CONDITION

        self.builder.position_at_end(cond_block)

        node.condition.accept(self)

        cond = self.stack.pop()

        cond_bool = self.builder.icmp_signed(
            '!=',
            cond,
            ir.Constant(self.intType, 0)
        )

        self.builder.cbranch(
            cond_bool,
            body_block,
            merge_block
        )

        # BODY

        self.builder.position_at_end(body_block)

        node.body.accept(self)

        if not self.builder.block.is_terminated:
            self.builder.branch(cond_block)

        # MERGE

        self.builder.position_at_end(merge_block)

    # =========================================================
    # SWITCH
    # =========================================================

    def visit_switch_statement(self, node: SwitchStatement):

        node.expression.accept(self)

        switch_value = self.stack.pop()

        end_block = self.current_function.append_basic_block(
            'switch_end'
        )

        default_block = self.current_function.append_basic_block(
            'default'
        )

        switch_inst = self.builder.switch(
            switch_value,
            default_block
        )

        case_blocks = []

        for case in node.cases:

            block = self.current_function.append_basic_block(
                'case'
            )

            case_blocks.append((case, block))

            case.value.accept(self)

            case_value = self.stack.pop()

            switch_inst.add_case(
                case_value,
                block
            )

        for case, block in case_blocks:

            self.builder.position_at_end(block)

            for stmt in case.stmts:
                stmt.accept(self)

            if not self.builder.block.is_terminated:
                self.builder.branch(end_block)

        # DEFAULT

        self.builder.position_at_end(default_block)

        if node.default is not None:

            for stmt in node.default:
                stmt.accept(self)

        if not self.builder.block.is_terminated:
            self.builder.branch(end_block)

        # END

        self.builder.position_at_end(end_block)

    # =========================================================
    # RETURN
    # =========================================================

    def visit_return_statement(self, node: ReturnStatement):

        if node.expression is not None:

            node.expression.accept(self)

            value = self.stack.pop()

            self.builder.ret(value)

        else:

            self.builder.ret_void()

    # =========================================================
    # UNUSED
    # =========================================================

    def visit_case(self, node):
        pass

    def visit_parameter(self, node):
        pass

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

int main()
{
    int x;
    x = fibonacci(5);
}
"""

# Factorial

data2 = """
int factorial(int n)
{
    if (n == 0)
    {
        return 1;
    }
    else
    {
        return n * factorial(n - 1);
    }
}

int main()
{
    int result;

    result = factorial(5);
}
"""

# Recursive Sum
data3 = """
int sum(int n)
{
    if (n == 0)
    {
        return 0;
    }
    else
    {
        return n + sum(n - 1);
    }
}

int main()
{
    int total;

    total = sum(10);
}
"""

# Recursive Power

data4 = """
int power(int base, int exp)
{
    if (exp == 0)
    {
        return 1;
    }
    else
    {
        return base * power(base, exp - 1);
    }
}

int main()
{
    int result;

    result = power(2, 5);
}
"""

# Greatest Common Divisor
data5 = """
int gcd(int a, int b)
{
    if (a == b)
    {
        return a;
    }
    else
    {
        if (a > b)
        {
            return gcd(a - b, b);
        }
        else
        {
            return gcd(a, b - a);
        }
    }
}

int main()
{
    int result;

    result = gcd(48, 18);
}
"""

root = parser.parse(data5)
print(root)
irgen = IRGenerator()
root.accept(irgen)
print(irgen.module)

# %%
print(irgen.stack)
# %%