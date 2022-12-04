

%{
    #include <cstdio>
    #include <cstdlib>
    #include <iostream>

    #include "ast.hpp"
    
    #define YYDEBUG 1
    #define YYINITDEPTH 99999
    int yylex(void);
    void yyerror(const char *);
    
    extern ASTNode* astRoot;
%}

%error-verbose

%token T_PRINT T_RETURN T_IF T_ELSE T_WHILE T_DO T_EXTENDS 
%token T_NEW T_AND T_OR T_NOT T_GEQ T_EQ T_ARROW
%token T_INT T_BOOLEAN T_NONE 
%token T_TRUE T_FALSE T_ID T_INTCONST

%right ':'
%left T_OR
%left T_AND
%left '>' T_GEQ T_EQ
%left '+' '-'
%left '*' '/'
%right T_NOT T_UMINUS

%type <program_ptr> program
%type <class_list_ptr> class_list
%type <class_ptr> class
%type <identifier_ptr> T_ID
%type <declaration_list_ptr> member_list declaration_list
%type <method_list_ptr> method_list
%type <method_ptr> method
%type <parameter_list_ptr> parameter_list parameter_list_nempty
%type <type_ptr> return_type type
%type <methodbody_ptr> method_body
%type <statement_list_ptr> statement_list
%type <returnstatement_ptr> return_statement
%type <parameter_ptr> parameter
%type <declaration_ptr> member declaration
%type <identifier_list_ptr> identifier_list
%type <expression_ptr> expression
%type <methodcall_ptr> method_call
%type <expression_list_ptr> argument_list argument_list_nempty
%type <statement_ptr> statement
%type <integer_ptr> T_INTCONST

%start program          

%%

program : class_list               { $$ = new ProgramNode($1); astRoot = $$; }
        ;       

class_list : class class_list   { $$ = $2; $$->push_front($1); }
        | class                 { $$ = new std::list<ClassNode*>(); $$->push_front($1); }
        ; 

class : T_ID '{' declaration_list method_list '}'    { $$ = new ClassNode($1, NULL, $3, $4); }
      | T_ID T_EXTENDS T_ID '{' declaration_list method_list '}'  { $$ = new ClassNode($1, $3, $5, $6);   }
      ;

member_list : member_list member         { $$ = new std::list<DeclarationNode*>(); $$->push_back($2); }
        | %empty                                        { $$ = new std::list<DeclarationNode*>(); }
        ;

method_list : method method_list        { $$ = $2; $$->push_front($1); }
        | %empty                        { $$ = new std::list<MethodNode*>(); }
        ;

// class : T_ID '{' class_contents '}'
//       | T_ID T_EXTENDS T_ID '{' class_contents '}'
//       ;

// class_contents : class_content class_contents
//                | %empty
//                ;

// class_content : member
//                 | method
//                 ;

member : type T_ID ';'                 { $$ = new DeclarationNode($1, new std::list<IdentifierNode*>()); 
                                         $$->identifier_list.push_back($2); }
       ;


method : T_ID '(' parameter_list ')' T_ARROW return_type '{' method_body '}' { $$ = new MethodNode($1,$3,$6,$8); }
       ;

parameter_list : %empty         { $$ = new std::list<ParameterNode*>(); }
           | parameter_list_nempty     { $$ = $1; }
           ;
           
parameter_list_nempty : parameter       { $$ = new std::list<ParameterNode*>(); $$->push_back($1); }
            | parameter ',' parameter_list_nempty      { $$ = $3; $$->push_front($1); }
            ;

parameter : type T_ID           { $$ = new ParameterNode($1, $2); }
          ;
  
method_body : declaration_list statement_list return_statement  { $$ = new MethodBodyNode($1,$2,$3); }
     ;

declaration_list : declaration_list declaration  { $$ = $1; $$->push_back($2); }
                 | %empty                       { $$ = new std::list<DeclarationNode*>(); }
                 ;
// method_contents : method_content method_contents
//                 | %empty
//                 ;

// method_content : declaration
//                 | statement
//                 ;

// declarations : %empty
//              | declarations declaration
//              ;

declaration : type identifier_list ';'  { $$ = new DeclarationNode($1,$2); }
            ;

identifier_list : T_ID                  { $$ = new std::list<IdentifierNode*>(); $$->push_back($1); }
            | T_ID ',' identifier_list  { $$ = $3; $$->push_front($1); }
            ;                         

statement_list : %empty                     { $$ = new std::list<StatementNode*>(); }
           | statement statement_list       { $$ = $2; $$->push_front($1); }
           ;

// statement : assignment_stmt ';'              { $$ = $1; }
//           | call_stmt ';'             { $$ = $1; }
//           | ifelse_stmt                { $$ = $1; }
//           | while_stmt                  { $$ = $1; }
//           | dowhile_stmt ';'            { $$ = $1; }
//           | print_stmt ';'         { $$ = $1; }
//           ;

statement : T_ID '=' expression ';'      { $$ = new AssignmentNode($1,NULL,$3); }
          | T_ID '.' T_ID '=' expression ';'  { $$ = new AssignmentNode($1,$3,$5); }
          | method_call ';'    { $$ = new CallNode($1); }
          | T_IF expression '{' statement statement_list '}'          { $5->push_front($4); $$ = new IfElseNode($2,$5,new std::list<StatementNode*>()); }
          | T_IF expression '{' statement statement_list '}' T_ELSE '{' statement statement_list '}'         { $5->push_front($4); $10->push_front($9); $$ = new IfElseNode($2,$5,$10); }
          | T_WHILE expression '{' statement statement_list '}'       { $5->push_front($4); $$ = new WhileNode($2,$5); }
          | T_DO '{' statement statement_list '}' T_WHILE '(' expression ')' ';'  { $4->push_front($3); $$ = new DoWhileNode($4,$8); }
          | T_PRINT expression ';' { $$ = new PrintNode($2); }
          ;

method_call : T_ID '.' T_ID '(' argument_list ')'       { $$ = new MethodCallNode($1,$3,$5); }
            | T_ID '(' argument_list ')'        { $$ = new MethodCallNode($1,NULL,$3); }
            ;

return_statement : %empty                       { $$ = NULL; }
                 | T_RETURN expression ';'      { $$ = new ReturnStatementNode($2); }
                 ;

expression : expression '+' expression          { $$ = new PlusNode($1,$3); }
           | expression '-' expression          { $$ = new MinusNode($1,$3); }
           | expression '*' expression          { $$ = new TimesNode($1,$3); }
           | expression '/' expression          { $$ = new DivideNode($1,$3); }
           | expression '>' expression          { $$ = new GreaterNode($1,$3); }
           | expression T_GEQ expression        { $$ = new GreaterEqualNode($1,$3); }
           | expression T_EQ expression         { $$ = new EqualNode($1,$3); }
           | expression T_AND expression        { $$ = new AndNode($1,$3); }
           | expression T_OR expression         { $$ = new OrNode($1,$3); }
           | T_NOT expression                   { $$ = new NotNode($2); }
           | expression '?' expression ':' expression   { $$ = new QMNode($1,$3,$5); }
           | '-' expression %prec T_UMINUS      { $$ = new NegationNode($2); }
           | T_ID                               { $$ = new VariableNode($1); }
           | T_ID '.' T_ID                      { $$ = new MemberAccessNode($1,$3); }
           | '(' expression ')'                 { $$ = $2; }
           | T_INTCONST                         { $$ = new IntegerLiteralNode($1); }
           | T_TRUE                             { $$ = new BooleanLiteralNode(new IntegerNode(1)); }         
           | T_FALSE                            { $$ = new BooleanLiteralNode(new IntegerNode(0)); }
           | T_NEW T_ID                         { $$ = new NewNode($2,new std::list<ExpressionNode*>()); }
           | T_NEW T_ID '(' argument_list ')'   { $$ = new NewNode($2,$4); }
           | method_call                        { $$ = $1; }
           ;

argument_list : %empty                          { $$ = new std::list<ExpressionNode*>(); }
          | argument_list_nempty                { $$ = $1; }
          ;

argument_list_nempty : expression ',' argument_list_nempty { $$ = $3; $$->push_front($1); }
           | expression                                    { $$ = new std::list<ExpressionNode*>(); $$->push_back($1); }
           ;             

type : T_INT            { $$ = new IntegerTypeNode(); $$->basetype = bt_integer; }
     | T_BOOLEAN        { $$ = new BooleanTypeNode(); $$->basetype = bt_boolean; }
     | T_ID             { $$ = new ObjectTypeNode($1); $$->basetype = bt_object; $$->objectClassName = $1->name; }
     ;

return_type : type      { $$ = $1; }
            | T_NONE    { $$ = new NoneNode(); $$->basetype = bt_none; }
            ;        

%%

extern int yylineno;

void yyerror(const char *s) {
  fprintf(stderr, "%s at line %d\n", s, yylineno);
  exit(1);
}