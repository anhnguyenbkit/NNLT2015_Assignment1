/*
 * student ID:
 */

grammar BKOOL;

@lexer::header{
	package bkool.parser;
}

@parser::header{
	package bkool.parser;
}

options{
	language=Java;
}

program: classDeclaration+ ;

// student for recognizer start from here
variableModifier
	:	'static'
	;
classDeclaration: 'class' Identifier
					('extends' Identifier)? 
					classBody
					;
classBody: '{' classBodyDeclaration* '}'
			;
classBodyDeclaration
	: ';'
	| block
	| memberDeclartion
	;

memberDeclartion
	: methodDeclaration
	| fieldDeclaration
	| constructorDeclaration
	;
fieldDeclaration
	: variableDeclarators ':' type
	;
methodDeclaration
	:	(type | 'void') Identifier formalParameters ('[' ']')*	
		( 	methodBody
		|	';'
		)
	;
constructorDeclaration
    :   Identifier formalParameters
        constructorBody
    ;
constructorBody
    :   block
    ;
formalParameters
	:	'(' formalParameterList? ')'
	;
formalParameterList
	:	formalParameter	(',' formalParameter)* (','lastFormalParameter)?
	|	lastFormalParameter
	;
formalParameter
	:	variableDeclarators ':' type
	;
variableDeclarators
	:	variableDeclaratorId (',' variableDeclaratorId)*
	;
variableDeclaratorId
	: Identifier
	;

lastFormalParameter
	: variableModifier* type '...' variableDeclaratorId
	;
type
	: primitiveType ('[' ']')*
	| classType ('[' ']')*
	;
classType
    :   Identifier
    ;
primitiveType
	:	'bool'
	|	'float'
	|	'integer'
	|	'string'
	;	
methodBody
	: block
	;
// STATEMENTS / BLOCKS
block
	: '{' blockStatement* '}'
	;
blockStatement
	: 	localVariableDeclarationStatement
	| 	statement
	|	classDeclaration
	;
localVariableDeclarationStatement
	:	localVariableDeclaration ';'
	;
localVariableDeclaration
	:	variableModifier? variableDeclarators ':' type
	;
statement
	: 	block
	| 	'if' parExpression 'then' statement ('else' statement)?
	| 	'while' parExpression 'do' statement
	|	'break' ';'
	|	'continue' ';'
	|	'return' expression? ';'
	|	statemenExpression ';'
	;
parExpression
	:	'(' expression ')'
	;
statemenExpression
	: 	expression
	;
expressionList
	: 	expression (',' expression)*
	;
expression
	:	primary
	|	expression '.' Identifier
	| 	Identifier '.' Identifier
	|	expression '.' Identifier '(' expressionList? ')'
	|	Identifier '.' Identifier '(' expressionList? ')'
	|	expression '.' 'self'
	|	'self' '.' Identifier
	| 	expression '[' expression ']'
	|	'new' creator
	|	expression ('+'|'-')
	| 	expression '!'
	|	expression '^' expression
	|	expression ('*'|'/'|'%') expression
	|	expression ('+'|'-') expression
	|	expression ('&&'|'||') expression
	|	expression ('=='|'<>') expression
	|	expression ('<'|'>'|'<='|'>=') expression
	|	<assoc=right> expression
		':='
		expression
	;
primary
    :   '(' expression ')'
    |   'self'
    |   literal
    |   Identifier
    ;
literal
    :   IntegerLiteral
    |   FloatingPointLiteral
    |   StringLiteral
    |   BooleanLiteral
    | 	NULL
    ;
creator
	: Identifier arguments
	;
arguments
	:	'(' expressionList? ')'
	;

// student for Lexer start from here
//Keywords
BOOL	:	'bool';
BREAK	:	'break';
CLASS	:	'class';
CONTINUE:	'continue';
DO		:	'do';
ELSE	:	'else';
EXTENDS	:	'extends';
FLOAT	:	'float';
IF		:	'if';
INTEGER	:	'integer';
NEW		:	'new';
STRING	:	'string';
THEN	:	'then';
WHILE	:	'while';
RETURN	:	'return';
TRUE	:	'true';
FALSE	:	'false';
VOID	:	'void';
NULL	:	'null';
SELF	:	'self';
FINAL	:	'final';
STATIC	:	'static';

//Integer Literals
IntegerLiteral
	: DecimalIntegerLiteral
	;
fragment
DecimalIntegerLiteral
	:	DecimalNumeral
	;
fragment
DecimalNumeral
	: '0'
	| NonZeroDigit Digits?
	;
fragment
Digits
	:	Digit Digit*
	;
fragment
Digit
	:	'0'
	| NonZeroDigit
	;
fragment
NonZeroDigit
	:	[1-9]
	;
//Floating-Point Literals
FloatingPointLiteral
	:	DecimalFloatingPointLiteral
	;
fragment
DecimalFloatingPointLiteral
	:	Digits '.' Digit? ExponentPart?
	|	Digits ExponentPart
	;
fragment
ExponentPart
	:	ExponentIndicator SignedInteger
	;
fragment
ExponentIndicator
	:	[eE]
	;
fragment
SignedInteger
	:	Sign? Digits
	;
fragment
Sign
	:	[+-]
	;
//Boolean Literals
BooleanLiteral
	:	'true'
	|	'false'
	;
//String Literals
StringLiteral
	: '"' StringCharacters? '"'
	;
fragment
StringCharacters
	: StringCharacter+
	;
fragment
StringCharacter
	: 	~["\\]
	|	EscapseSequence
	;
fragment
EscapseSequence
	:	'\\' [btnfr"\\]
	;
//The Null Literal
NullLiteral
	: 'null'
	;
//Separators
LPAREN		: '(';
RPAREN		: ')';
LBRACE		: '{';
RBRACE		: '}';
LBRACK		: '[';
RBRACK		: ']';
SEMI		: ';';
COLON		: ':';
COMMA		: ',';
DOT			: '.';


//Operators
ASSIGN		: '=';
GT			: '>';
LT			: '<';
BANG		: '!';
EQUAL		: '++';
LE			: '<=';
GE			: '>=';
NOTEQUAL	: '<>';
AND			: '&&';
OR			: '||';
INC_OR_ADD			: '+';
DEC_OR_SUB			: '-';
MUL			: '*';
DIV			: '/';
CARET		: '^';
MOD			: '%';
Identifier  : [a-zA-Z][a-zA-Z0-9]*
			;
// Whitespace and comments
WS	: [ \t\r\n]+ -> skip;
COMMENT: '(*' .*? '*)' -> skip;
LINE_COMMENT
	:	'#' ~[\r\n]* -> skip
	;
	
