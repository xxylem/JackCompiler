module Data.AnalyserModel where

{-

data JackExpression =
    Constant 



A Jack expression is one of the following:
m A constant;
m A variable name in scope (the variable may be static, field, local, or
parameter);
m The this keyword, denoting the current object (cannot be used in functions);
m An array element using the syntax name[expression], where name is a variable
name of type Array in scope;
m A subroutine call that returns a non-void type;
m An expression prefixed by one of the unary operators - or ~:
- expression: arithmetic negation;
~ expression: boolean negation (bit-wise for integers);
m An expression of the form expression operator expression where operator is
one of the following binary operators:
+ - * / Integer arithmetic operators;
& | Boolean And and Boolean Or (bit-wise for integers) operators;
< > = Comparison operators;
m (expression): An expression in parentheses.

Statement Syntax Description
let let variable = expression;
or
let variable [expression] =
expression;
An assignment operation (where
variable is either single-valued or
an array). The variable kind may
be static, local, field, or parameter.
if if (expression) {
statements
}
else {
statements
}
Typical if statement with an
optional else clause.
The curly brackets are mandatory
even if statements is a single
statement.
while while (expression) {
statements
}
Typical while statement.
The curly brackets are mandatory
even if statements is a single
statement.
do do function-or-method-call;
Used to call a function or a
method for its effect, ignoring the
returned value.
return Return expression;
or
return;
Used to return a value from a
subroutine. The second form must
be used by functions and methods
that return a void value.
Constructors must return the
expression this.

Variable kind Definition/Description Declared in Scope
Static variables static type name1, name2, ...;
Only one copy of each static variable
exists, and this copy is shared by all the
object instances of the class (like private
static variables in Java)
Class
declaration.
The class in
which they are
declared.
Field variables field type name1, name2, ...;
Every object instance of the class has a
private copy of the field variables (like
private object variables in Java)
Class
declaration.
The class in
which they are
declared, except
for functions.
Local variables var type name1, name2, ...;
Local variables are allocated on the stack
when the subroutine is called and freed
when it returns (like local variables in
Java)
Subroutine
declaration.
The subroutine
in which they
are declared.
Parameter
variables
type name1, name2, ...
Used to specify inputs of subroutines, for
example:
function void drive (Car c, int miles)
Appear in
parameter lists
as part of
subroutine
declarations.
The subroutine
in which they
are declared.
Figure 9.7


Program structure: A Jack program is a collection of classes, each appearing in a separate file.
The compilation unit is a class. A class is a sequence of tokens structured
according to the following context free syntax:
class: 'class' className '{' classVarDec* subroutineDec* '}'
classVarDec: ('static' | 'field') type varName (',' varName)* ';'
type: 'int' | 'char' | 'boolean' | className
subroutineDec: ('constructor' | 'function' | 'method')
('void' | type) subroutineName '(' parameterList ')'
subroutineBody
parameterList: ((type varName) (',' type varName)*)?
subroutineBody: '{' varDec* statements '}'
varDec: 'var' type varName (',' varName)* ';'
className: identifier
subroutineName: identifier
varName: identifier

Statements:
statements: statement*
statement: letStatement | ifStatement | whileStatement |
doStatement | returnStatement
letStatement: 'let' varName ('[' expression ']')? '=' expression ';'
ifStatement: 'if' '(' expression ')' '{' statements '}'
('else' '{' statements '}')?
whileStatement: 'while' '(' expression ')' '{' statements '}'
doStatement: 'do' subroutineCall ';'
ReturnStatement 'return' expression? ';'

Expressions:
expression: term (op term)*
term: integerConstant | stringConstant | keywordConstant |
varName | varName '[' expression ']' | subroutineCall |
'(' expression ')' | unaryOp term
subroutineCall: subroutineName '(' expressionList ')' | (className |
varName) '.' subroutineName '(' expressionList ')'
expressionList: (expression (',' expression)* )?
op: '+' | '-' | '*' | '/' | '&' | '|' | '<' | '>' | '='
unaryOp: '-' | '~'
KeywordConstant: 'true' | 'false' | 'null' | 'this'
-- data VariableKind =
--     StaticKind
--   | FieldKind
--   | LocalKind

-- data Variables =
--   VariableKind VariableType [VariableName]

-- newtype ParamVariables = Param [(VariableType, VariableName)] -}