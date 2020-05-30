grammar C;
compilationUnit: start? EOF;
start: externalDeclaration | start externalDeclaration;
externalDeclaration: functionDefinition | declaration | Semi;
statement:
	labeledStatement
	| compoundStatement
	| expressionStatement
	| selectionStatement
	| iterationStatement
	| jumpStatement;
labeledStatement:
	Identifier Colon statement
	| Case conditionExpression Colon statement
	| Default Colon statement;
compoundStatement: LeftBrace blockItemList RightBrace;
blockItemList: blockItemList blockItem | blockItem;
blockItem: statement | declaration;
selectionStatement:
	If LeftParen expression RightParen statement (Else statement)?
	| Switch LeftParen expression RightParen statement;
iterationStatement:
	While LeftParen expression RightParen statement
	| Do statement While LeftParen expression RightParen Semi
	| For LeftParen expression? Semi expression? Semi expression? RightParen statement
	| iterationDeclaredStatement;
iterationDeclaredStatement:
	For LeftParen declaration expression? Semi expression? RightParen statement;
jumpStatement:
	breakStatement
	| continueStatement
	| gotoStatement
	| returnStatement;
breakStatement: Break Semi;
continueStatement: Continue Semi;
gotoStatement: Goto Identifier Semi;
returnStatement: Return expression? Semi;
functionDefinition:
	declarationSpecifier+? directDeclarator LeftParen declarationList? RightParen compoundStatement;
//no declaration in main
declarationList:
	declaration
	| declarationList Comma declaration;
declaration:
	declarationSpecifier initDeclaratorList Semi?; //enable functionDefinition
//	| declarationSpecifier+ Semi?;
declarationSpecifier:
	Typedef
	| Extern
	| Static
	| Auto
	| Register
	| Void
	| Char
	| Short
	| Int
	| Long
	| Float
	| Double
	| Signed
	| Unsigned
	| Const
	| Inline;
initDeclaratorList:
	initDeclarator
	| initDeclaratorList Comma initDeclarator;
initDeclarator:
	directDeclarator
	| directDeclarator Assign initializer;
directDeclarator:
	Identifier
	| Identifier LeftBracket assignmentExpression? RightBracket;
initializer:
	assignmentExpression
	| LeftBrace initializerList RightBrace; //int a[2] = {1,2};
initializerList:
	initializerList Comma initializer
	| initializer;
expressionStatement: expression? Semi;
expression:
	assignmentExpression
	| expression Comma assignmentExpression;
assignmentExpression:
	conditionExpression
	| unaryExpression Assign assignmentExpression
	| Digit+;
conditionExpression:
	Not? logicalExpression (
		Question expression Colon conditionExpression
	)?;
logicalExpression:
	logicalExpression AndAnd relationExpression	# AndAnd
	| logicalExpression OrOr relationExpression	# OrOr
	| relationExpression						# Relation;
relationExpression:
	relationExpression NotEqual shiftExpression			# NotEqual
	| relationExpression Equal shiftExpression			# Equal
	| relationExpression GreaterEqual shiftExpression	# GreaterEqual
	| relationExpression LessEqual shiftExpression		# LessEqual
	| relationExpression Greater shiftExpression		# Greater
	| relationExpression Less shiftExpression			# Less
	| shiftExpression									# ShiftExpr;
shiftExpression:
	shiftExpression LeftShift binaryExpression		# LeftShift
	| shiftExpression RightShift binaryExpression	# RightShift
	| binaryExpression								# BinaryExpr;
binaryExpression:
	binaryExpression And castExpression		# And
	| binaryExpression Or castExpression	# Or
	| binaryExpression Xor castExpression	# Xor
	| binaryExpression Mul castExpression	# Mul
	| binaryExpression Div castExpression	# Div
	| binaryExpression Mod castExpression	# Mod
	| binaryExpression Plus castExpression	# Add
	| binaryExpression Minus castExpression	# Sub
	| castExpression						# CastExpr;
castExpression:
	LeftParen unaryTypename RightParen castExpression
	| unaryExpression
	| Digit+;
unaryExpression: //一元前缀表达式
	PlusPlus unaryExpression		# AddAdd
	| MinusMinus unaryExpression	# SubSub
	| Plus castExpression			# Positive
	| Minus castExpression			# Negative
	| And castExpression			# FetchAddress
	| Mul castExpression			# Star
	| Tilde castExpression			# Tilde
	| Sizeof castExpression			# Sizeof
	| unaryTypename					# Sizeof
	| postfixExpression				# PostfixExpr;
unaryTypename: Sizeof LeftParen typeName RightParen;
typeName:
	Void
	| Char
	| Short
	| Int
	| Long
	| Float
	| Double
	| Signed
	| Unsigned;
postfixExpression:
	arrayAccess						# Array
	| functionCall					# Function
	| postfixExpression PlusPlus	# Inc
	| postfixExpression MinusMinus	# Dec
	| primaryExpression				# Var;
arrayAccess: Identifier LeftBracket expression RightBracket;
functionCall: Identifier LeftParen argumentList? RightParen;
argumentList:
	assignmentExpression
	| argumentList Comma assignmentExpression;
primaryExpression:
	Identifier							# ID
	| Constant							# Const
	| StringLiteral						# String
	| LeftParen expression RightParen	# Parens;

Auto: 'auto';
Break: 'break';
Case: 'case';
Char: 'char';
Const: 'const';
Continue: 'continue';
Default: 'default';
Do: 'do';
Double: 'double';
Else: 'else';
Enum: 'enum';
Extern: 'extern';
Float: 'float';
For: 'for';
Goto: 'goto';
If: 'if';
Inline: 'inline';
Int: 'int';
Long: 'long';
Register: 'register';
Return: 'return';
Short: 'short';
Signed: 'signed';
Sizeof: 'sizeof';
Static: 'static';
Struct: 'struct';
Switch: 'switch';
Typedef: 'typedef';
Union: 'union';
Unsigned: 'unsigned';
Void: 'void';
While: 'while';

LeftParen: '(';
RightParen: ')';
LeftBracket: '[';
RightBracket: ']';
LeftBrace: '{';
RightBrace: '}';

Less: '<';
LessEqual: '<=';
Greater: '>';
GreaterEqual: '>=';
Equal: '==';
NotEqual: '!=';
LeftShift: '<<';
RightShift: '>>';

Plus: '+';
PlusPlus: '++';
Minus: '-';
MinusMinus: '--';
Mul: '*';
Div: '/';
Mod: '%';

And: '&';
Or: '|';
AndAnd: '&&';
OrOr: '||';
Xor: '^';
Not: '!';
Tilde: '~';

Question: '?';
Colon: ':';
Semi: ';';
Comma: ',';
Dot: '.';

Assign:
	'='
	| '*='
	| '/='
	| '%='
	| '+='
	| '-='
	| '<<='
	| '>>='
	| '&='
	| '^='
	| '|=';

Identifier: Alphabet (Alphabet | Digit)*;
Constant: IntegerConstant | FloatConstant | CharConstant;

fragment IntegerConstant:
	DecimalConstant
	| OctalConstant
	| HexadecimalConstant
	| BinaryConstant;

fragment DecimalConstant: [1-9] Digit*;
fragment OctalConstant: '0' [0-7]*;
fragment HexadecimalConstant: ('0x' | '0X') [0-9a-fA-F]+;
fragment BinaryConstant: ('0b' | '0B') [0-1]+;
fragment FloatConstant:
	DecimalFloatConstant
	| HexadecimalFloatConstant;
fragment DecimalFloatConstant: Dot Digit+ | Digit+ Dot Digit+?;
fragment HexadecimalFloatConstant: ('0x' | '0X') (
		Dot [0-9a-fA-F]+
		| [0-9a-fA-F]+ Dot [0-9a-fA-F]+?
	);
fragment CharConstant:
	'\'' (~['\\\r\n] | '\\' ['"?abfnrtv\\] | Digit+ | Alphabet+) '\'';
StringLiteral: '"' StringChar+ '"';
fragment StringChar:
	~["\\\r\n]
	| '\\\n'
	| '\\\r\n'
	| '\\' ['"?abfnrtv\\]
	| Digit+
	| Alphabet+;
fragment Alphabet: [a-zA-Z_];
Digit: [0-9];

Whitespace: [ \t]+ -> skip;

Newline: ( '\r' '\n'? | '\n') -> skip;

BlockComment: '/*' .*? '*/' -> skip;

LineComment: '//' ~[\r\n]* -> skip;

