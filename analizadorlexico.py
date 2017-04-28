import ply.lex as lex
import sys

#Lista de Tokens en lenguaje LOLA
tokens = (
    #lista de palabras reservadas del lenuaje
    'BEGIN',
    'CONST',
    'BOOLEAN',
    'STRING',
    'IMPORT',
    'POINTER',
    'TO',
    'LOOP',
    'ELSE',
    'IF',
    'ELSIF',
    'DEFINITION',
    'RECORD',
    'OF',
    'CHAR',
    'EXIT',
    'END',
    'IN',
    'INOUT',
    'MODULE',
    'OUT',
    'REG',
    'TS',
    'TYPE',
    'VAR',
	'BIT',
	'PROCEDURE',
	'SHORTINT',
	'NEW',
	'RETURN',
	'THEN',
	'New',
	'Next',
	'NIL',
	'Variable',
	'This',
	'MUX',
	'LATCH',
	'SR',
	#'DIV',
	'MOD',
	'FOR',
	'DO',


    #Simbolos para operaciones binarias
    'PLUS',
    'MINUS',
    'DIVIDE',
    'LESS',
    'LESSEQUAL',
    'GREATER',
    'GREATEREQUAL',
    'EQUAL',
    'ASIGNATION',
    'COMMA',
    'LPARENT',
    'RPARENT',
    'LBRACKET',
    'RBRACKET',
    'LBLOCK',
    'RBLOCK',
    'AMPERSANT',
    'HASHTAG',
    'TIMES',
    'TILDE',
    'OR',
    'CARET', #^
    'DOT', #.
    'DOTDOT',
    'COLON', #:
    'SEMICOLON', #;
    #'APOSTROPHE', # '
    'DISTINT', #!
    'MINUSGREATER', # ->
    'COLONEQUAL', #:=

    #Other Simbols
    'INVALIDID',
    'ID',
    'NUMBER',
    #'LOGICVALUE',
)

#Expresiones regulares simpes para los tokens
t_PLUS   = r'\+'
t_MINUS  = r'-'
t_TIMES  = r'\*'
t_DIVIDE = r'/'
t_EQUAL  = r'='
t_LESS   = r'<'
t_GREATER = r'>'
t_COMMA  = r','
t_ASIGNATION = r'\:='
t_LPARENT = r'\('
t_RPARENT  = r'\)'
t_LBRACKET = r'\['
t_RBRACKET = r'\]'
t_LBLOCK   = r'{'
t_RBLOCK   = r'}'
t_COLON   = r':'
t_SEMICOLON = r';'
t_DISTINT  = r'!'
t_AMPERSANT = r'\&'
t_HASHTAG = r'\#'
t_DOT = r'\.'
t_DOTDOT = r'\.\.'
t_TILDE =r'\~'
t_OR = r'\|'
t_CARET =r'\^'
#t_APOSTROPHE = r'\''



def t_BEGIN(t):
	r'BEGIN'
	return t
def t_CONST(t):
	r'CONST'
	return t
def t_END(t):
    r'END'
    return t

def t_BOOLEAN(t):
    r'BOOLEAN'
    return t
def t_STRING(t):
    r'STRING'
    return t
def t_IMPORT(t):
    r'IMPORT'
    return t
def t_POINTER(t):
    r'POINTER'
    return t
def t_TO(t):
    r'TO'
    return t
def t_LOOP(t):
    r'LOOP'
    return t
def t_IF(t):
    r'IF'
    return t
def t_ELSE(t):
    r'ELSE'
    return t
def t_ELSIF(t):
    r'ELSIF'
    return t
def t_DEFINITION(t):
    r'DEFINITION'
    return t
def t_RECORD(t):
    r'RECORD'
    return t
def t_OF(t):
    r'OF'
    return t
def t_CHAR(t):
    r'CHAR'
    return t
def t_EXIT(t):
    r'EXIT'
    return t

def t_IN(t):
	r'IN'
	return t
def t_INOUT(t):
	r'INOUT'
	return t
def t_MODULE(t):
	r'MODULE'
	return t
def t_OUT(t):
	r'OUT'
	return t
def t_REG(t):
	r'REG'
	return t
def t_TS(t):
	r'TS'
	return t
def t_TYPE(t):
	r'TYPE'
	return t
def t_VAR(t):
	r'VAR'
	return t
def t_BIT(t):
	r'BIT'
	return t
def t_PROCEDURE(t):
	r'PROCEDURE'
	return t
def t_SHORTINT(t):
	r'SHORTINT'
	return t
def t_NEW(t):
	r'NEW'
	return t
def t_RETURN(t):
	r'RETURN'
	return t
def t_THEN(t):
	r'THEN'
	return t
def t_New(t):
	r'New'
	return t
#def t_ARRAY_OF_CHAR(t):
#	r'ARRAY OF CHAR'
#	return t
def t_Next(t):
	r'Next'
	return t
def t_NIL(t):
	r'NIL'
	return t
def t_Variable(t):
	r'Variable'
	return t
def t_This(t):
	r'This'
	return t

def t_MUX(t):
	r'MUX'
	return t
def t_LATCH(t):
	r'LATCH'
	return t
def t_SR(t):
	r'SR'
	return t
def t_DIV(t):
	r'DIV'
	return t
def t_MOD(t):
	r'MOD'
	return t

def t_FOR(t):
	r'FOR'
	return t
def t_DO(t):
	r'DO'
	return t
#def t_POINTER(t):
#	r'POINTER'
#	return t


def t_INVALIDID(t):
 r'\d+(\d*\w+)'
 return t

def t_NUMBER(t):
    r'\d+(\.\d+)?'
    t.value = float(t.value)
    return t



def t_ID(t):
    r'\w+(_\d\w)*'
    return t



def t_LESSEQUAL(t):
	r'<='
	return t

def t_GREATEREQUAL(t):
	r'>='
	return t

def t_newline(t):
    r'\n+'
    t.lexer.lineno += len(t.value)

t_ignore = ' \t'

def t_comments_C99(t):
    r'\(\*(.)*?\*\)'
    t.lexer.lineno += 1

def t_error(t):
    print ("Lexical error: " + str(t.value[0]))
    t.lexer.skip(1)

def test(data, lexer):
	lexer.input(data)
	while True:
		tok = lexer.token()
		if not tok:
			break
		print (tok)

lexer = lex.lex()

if __name__ == '__main__':
	if (len(sys.argv) > 1):
		fin = sys.argv[1]
	else:
		fin = 'ejemplo.txt'
	f = open(fin, 'r')
	data = f.read()
	print (data)
	lexer.input(data)
	test(data, lexer)
	input()
