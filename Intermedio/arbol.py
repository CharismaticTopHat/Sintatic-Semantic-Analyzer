from __future__ import annotations
from abc import ABC, abstractmethod
from typing import Any

class ASTNode(ABC):
    @abstractmethod
    def accept(self, visitor: Visitor) -> None:
        pass

class Program(ASTNode):
    def __init__(self, functions: Any, main: Any) -> None:
        self.functions = functions
        self.main = main

    def accept(self, visitor: Visitor):
        visitor.visit_program(self)

    def __str__(self):
        return f"Program(functions={self.functions}, main={self.main})"
    
class Assignment(ASTNode):
    def __init__(self, variable: Any, assignment: ASTNode) -> None:
        self.variable   = variable
        self.assignment = assignment

    def accept(self, visitor: Visitor):
        visitor.visit_assignment(self)

    def __str__(self):
        return f"Assignment(var={self.variable}, expr={self.assignment})"

class IfStatement(ASTNode):
    def __init__(self, condition: ASTNode, then_stmt: ASTNode, else_stmt: ASTNode) -> None:
        self.condition = condition
        self.then_stmt = then_stmt
        self.else_stmt = else_stmt

    def accept(self, visitor: Visitor):
        visitor.visit_if_statement(self)

    def __str__(self):
        return f"IfStatement(cond={self.condition}, then={self.then_stmt}, else={self.else_stmt})"

class WhileStatement(ASTNode):
    def __init__(self, condition: ASTNode, body: ASTNode) -> None:
        self.condition = condition
        self.body      = body

    def accept(self, visitor: Visitor):
        visitor.visit_while_statement(self)

    def __str__(self):
        return f"WhileStatement(cond={self.condition}, body={self.body})"

class Case(ASTNode):
    def __init__(self, value, stmts):
        self.value = value
        self.stmts = stmts

    def accept(self, visitor):
        visitor.visit_case(self)

    def __str__(self):
        return f"Case(value={self.value}, stmts={self.stmts})"

class SwitchStatement(ASTNode):
    def __init__(self, expression, cases, default):
        self.expression = expression
        self.cases = cases
        self.default = default

    def accept(self, visitor):
        visitor.visit_switch_statement(self)

    def __str__(self):
        return f"Switch(expr={self.expression})"

class ReturnStatement(ASTNode):

    def __init__(self, expression) -> None:
        self.expression = expression

    def accept(self, visitor: Visitor):
        visitor.visit_return_statement(self)

    def __str__(self):
        return f"ReturnStatement(expr={self.expression})"

class Declaration(ASTNode):
    def __init__(self, variable: Any, type: str) -> None:
        self.variable = variable
        self.type     = type

    def accept(self, visitor: Visitor):
        visitor.visit_declaration(self)

    def __str__(self):
        return f"Declaration(var={self.variable}, type={self.type})"

class Declarations(ASTNode):
    def __init__(self, decls: Declarations, decl: Declaration) -> None:
        self.decls = decls
        self.decl  = decl

    def accept(self, visitor: Visitor):
        visitor.visit_declarations(self)

    def __str__(self):
        return f"Declarations(decls={self.decls}, decl={self.decl})"

class Literal(ASTNode):
    def __init__(self, value: Any, type: str) -> None:
        self.value = value
        self.type  = type

    def accept(self, visitor: Visitor):
        visitor.visit_literal(self)

    def __str__(self):
        return f"[LIT, {self.value}]"

class Variable(ASTNode):
    def __init__(self, name: Any, type: str) -> None:
        self.name = name
        self.type = type

    def accept(self, visitor: Visitor):
        visitor.visit_variable(self)

    def __str__(self):
        return f"[VAR, {self.name}]"
    
class Block(ASTNode):

    def __init__(self, decls, stmts) -> None:
        self.decls = decls
        self.stmts = stmts

    def accept(self, visitor: Visitor):
        visitor.visit_block(self)

    def __str__(self):
        return f"Block(decls={self.decls}, stmts={self.stmts})"

class Parameter(ASTNode):
    def __init__(self, variable: str, type: str) -> None:
        self.variable = variable
        self.type     = type

    def accept(self, visitor: Visitor):
        visitor.visit_parameter(self)

    def __str__(self):
        return f"Parameter(var={self.variable}, type={self.type})"

class Function(ASTNode):
    def __init__(self, name: str, params: list,
                 decls: Any, stmts: Any) -> None:
        self.name   = name
        self.params = params
        self.decls  = decls
        self.stmts  = stmts

    def accept(self, visitor: Visitor):
        visitor.visit_function(self)

    def __str__(self):
        return f"Function(name={self.name}, params={self.params})"

class Call(ASTNode):
    def __init__(self, name: str, args: list) -> None:
        self.name = name
        self.args = args

    def accept(self, visitor: Visitor):
        visitor.visit_call(self)

    def __str__(self):
        return f"Call(name={self.name}, args={self.args})"

class UnaryOp(ASTNode):
    def __init__(self, op: str, operand: ASTNode):
        self.op = op
        self.operand = operand

    def accept(self, visitor):
        visitor.visit_unary_op(self)

    def __str__(self):
        return f"[{self.op}, {self.operand}]"

class BinaryOp(ASTNode):
    def __init__(self, op: str, lhs: ASTNode, rhs: ASTNode) -> None:
        self.op  = op
        self.lhs = lhs
        self.rhs = rhs

    def accept(self, visitor: Visitor):
        visitor.visit_binary_op(self)

    def __str__(self):
        return f"[{self.op}, {self.lhs}, {self.rhs}]"

class Visitor(ABC):
    @abstractmethod
    def visit_literal(self, node: Literal) -> None:
        pass

    @abstractmethod
    def visit_variable(self, node: Variable) -> None:
        pass

    @abstractmethod
    def visit_binary_op(self, node: BinaryOp) -> None:
        pass

    @abstractmethod
    def visit_while_statement(self, node: WhileStatement) -> None:
        pass

    @abstractmethod
    def visit_if_statement(self, node: IfStatement) -> None:
        pass

    @abstractmethod
    def visit_program(self, node: Program) -> None:
        pass

    @abstractmethod
    def visit_declaration(self, node: Declaration) -> None:
        pass

    @abstractmethod
    def visit_declarations(self, node: Declarations) -> None:
        pass

    @abstractmethod
    def visit_assignment(self, node: Assignment) -> None:
        pass

    @abstractmethod
    def visit_block(self, node: Block) -> None:
        pass

    @abstractmethod
    def visit_parameter(self, node: Parameter) -> None:
        pass

    @abstractmethod
    def visit_function(self, node: Function) -> None:
        pass

    @abstractmethod
    def visit_switch_statement(self, node):
        pass

    @abstractmethod
    def visit_case(self, node):
        pass

"""
class Calculator(Visitor):
    def __init__(self):
        self.stack = []

    def visit_literal(self, node: Literal) -> None:
        self.stack.append(node.value)
    
    def visit_variable(self, node: Variable) -> None:
        pass

    def visit_binary_op(self, node: BinaryOp) -> None:
        node.lhs.accept(self)
        node.rhs.accept(self)
        rhs = self.stack.pop()
        lhs = self.stack.pop()
        if node.op == '+':
            self.stack.append(lhs + rhs)
        elif node.op == '-':
            self.stack.append(lhs - rhs)
        elif node.op == '*':
            self.stack.append(lhs * rhs)
        elif node.op == '/':
            self.stack.append(lhs / rhs)
        elif node.op == '%':
            self.stack.append(lhs % rhs)
"""