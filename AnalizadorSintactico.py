import ply.yacc as yacc
import os
import codecs
import re
import sys
from AnalizadorLexico1 import tokens
from sys import  stdin

VERBOSE = 1

'''
module = "MODULE" identifier ";"
{TypeDeclaration ";"}
["CONST" {ConstDeclaration}]
["IN" {VarDeclaration}]
["INOUT" {VarDeclaration}]
["OUT" {VarDeclaration}]
["VAR" {VarDeclaration}]
["BEGIN" StatementSequence]
"END" identifier "." .

FormalType = {"[" [expression] "]"} "BIT".

FormalBusType = {"[" [expression] "]"} ("TS" | "OC").

TypeDeclaration = "TYPE" identifier ["*"] ["(" IdList ")"] ";"
["CONST" {ConstDeclaration}]
["IN" {IdList ":" FormalType ";"}]
["INOUT" {IdList ":" FormalBusType ";"}]
["OUT" {VarDeclaration}]
["VAR" {VarDeclaration}]
["BEGIN" StatementSequence]
"END" identifier.

identifier = letter {letter | digit} ["'"].
integer = digit {digit}.
LogicValue = " '0" | " '1".
SimpleType = BasicType | identifier ["(" ExpressionList ")"].
BasicType = "BIT" | "TS" | "OC".
ExpressionList = expression {"," expression}.
type = { "[" expression "]" } SimpleType.
ConstDeclaration = identifier ":=" expression ";".
VarDeclaration = IdList ":" type ";".
IdList = identifier {"," identifier}.
selector = {"." identifier | "." integer | "[" expression "]"}.
factor = identifier selector | LogicValue | integer |
    "˜" factor | "↑" factor | "(" expression ")" |
    "MUX" "(" expression ":" expression "," expression ")" |
    "MUX" "(" expression "," expression ":" expression "," expression "," expression "," expression ")" |
    "REG" "(" [expression ","] expression ")" |
     "LATCH" "(" expression "," expression ")" |
     "SR" "(" expression "," expression ")" .
term = factor {("*" | "/" | "DIV" | "MOD") factor}.
expression = term {("+" | "−") term}.
assignment = identifier selector ":=" [condition "|"] expression.
condition = expression.
relation = expression ("=" | "#" | "<" | "<=" | ">" | ">=") expression.
IfStatement = "IF" relation "THEN" StatementSequence
{"ELSIF" relation "THEN" StatementSequence}
["ELSE" StatementSequence]
"END" .
ForStatement = "FOR" identifier ":=" expression ".." expression "DO" StatementSequence "END" .
statement = [assignment | UnitAssignment | IfStatement | ForStatement].
StatementSequence = statement {";" statement}.


UnitAssignment = identifier selector "(" ExpressionList ")".'''

'''precedence =(
('right','EQUAL'),
('right','ASIGNATION'),
('left','DISTINT'),
('left','LESS','LESSEQUAL','GREATER','GREATEREQUAL'),
('left','PLUS','MINUS'),
('left','TIMES','DIVIDE'),
('left','LPARENT','RPARENT'),
('left','LBRACKET','RBRACKET'),
('left','LBLOCK','RBLOCK'),
)'''



def p_SimpleType(p):
    '''SimpleType : BasicType
                    | ID  LPARENT ExpressionList RPARENT
                    | module '''
    pass

def p_integer(p):
    '''integer : NUMBER'''
    pass

def p_BasicType(p):
    '''BasicType : BIT
                   | TS'''
    pass
def p_ExpresionList(p):
    '''ExpressionList : expression  SEMICOLON
                        | expression COMMA expression  SEMICOLON
                        | expression COMMA expression COMMA expression  SEMICOLON
                        | expression COMMA expression COMMA expression COMMA expression  SEMICOLON'''
    pass
def p_type(p):
    '''type :  expression  SimpleType
               | TypeDeclaration'''
    pass
def p_ConstDeclaration(p):
    '''ConstDeclaration : ID ASIGNATION expression SEMICOLON'''
    pass
def p_VarDeclaration (p):
     '''VarDeclaration : IdList COLON type SEMICOLON
                         | IdList COLON LBRACKET ID RBRACKET FormalType SEMICOLON ID COLON FormalType SEMICOLON'''
     pass
def p_IdList(p):
     '''IdList : ID
                 | ID COMMA ID
                 | ID COMMA ID COMMA ID
                 | ID COMMA ID COMMA ID COMMA ID'''
     pass

def p_selector(p):
     '''selector :  DOT ID
                    | DOT  integer
                    | expression '''
     pass
def p_factor(p):
    '''factor : ID selector
                | integer
                | TILDE factor
                | LPARENT expression RPARENT
                | MUX LPARENT expression COLON expression COMMA expression RPARENT
                | MUX LPARENT expression COMMA expression COLON expression COMMA expression COMMA expression COMMA expression RPARENT
                | REG LPARENT  expression COMMA  expression RPARENT
                | LATCH LPARENT expression COMMA expression RPARENT
                | SR LPARENT expression COMMA expression RPARENT '''
    pass
def p_term(p):
    '''term : factor  TIMES factor
              | factor  DIVIDE factor
              | factor  MOD factor '''
    pass

def p_expression(p):
    '''expression : term  PLUS  term
                    | term  MINUS term'''
    pass
def p_assignment(p) :
     '''assignment : ID selector ASIGNATION condition OR expression'''
     pass
def p_condition(p):
    '''condition : expression'''
    pass
def p_relation(p):
     '''relation : expression  EQUAL  expression
                   | expression  HASHTAG  expression
                   | expression  LESS  expression
                   | expression  LESSEQUAL  expression
                   | expression  GREATER  expression
                   | expression  GREATEREQUAL  expression'''
     pass
def p_IfStatement(p):
    '''IfStatement : IF relation THEN StatementSequence LBLOCK ELSIF relation THEN StatementSequence RBLOCK LBRACKET ELSE StatementSequence RBRACKET END'''
    pass
def p_ForStatement(p):
     '''ForStatement : FOR ID ASIGNATION expression DOUBLEDOT expression DO StatementSequence END'''
     pass
def p_statement(p):
    '''statement : assignment
                   | UnitAssignment
                   | IfStatement
                   | ForStatement '''
    pass
def p_StatementSequence (p):
     '''StatementSequence : statement  SEMICOLON statement '''
     pass

def p_FormalType (p):
     '''FormalType : expression  BIT
                     | BIT'''
     pass
#def p_FormalBusType(p):
#     '''FormalBusType : expression  TS'''
#     pass

def p_module(p):
    '''module : MODULE ID SEMICOLON
                | TypeDeclaration
                | CONST ConstDeclaration
                | IN VarDeclaration
                | INOUT VarDeclaration
                | OUT VarDeclaration
                | VAR VarDeclaration
                | BEGIN StatementSequence
                | END ID DOT'''
    pass

def p_TypeDeclaration(p):
    '''TypeDeclaration : TYPE ID  SEMICOLON
                         | CONST ConstDeclaration
                         | IN IdList COLON FormalType SEMICOLON
                         | INOUT IdList COLON FormalType SEMICOLON
                         | OUT VarDeclaration
                         | VAR VarDeclaration
                         | BEGIN StatementSequence
                         | END ID DOT'''
    pass

def p_UnitAssignment (p):
     '''UnitAssignment : ID selector LPARENT ExpressionList RPARENT'''
     pass

def p_error(p):
	if VERBOSE:
		if p is not None:
			print ("ERROR SINTACTICO EN LA LINEA " + str(p.lexer.lineno) + " NO SE ESPERABA EL Token  " + str(p.value))
		else:
			print ("ERROR SINTACTICO EN LA LINEA: " + str(cminus_lexer.lexer.lineno))
	else:
		raise Exception('syntax', 'error')


parser = yacc.yacc()

if __name__ == '__main__':

	if (len(sys.argv) > 1):
		fin = sys.argv[1]
	else:
		fin = 'ejemplo.txt'

	f = open(fin, 'r')
	data = f.read()
	#print (data)
	parser.parse(data, tracking=True)
	print("Tu parser reconocio correctamente todo")
	#input()
