

%option yylineno
%pointer

%{
    #include <cstdlib>
    #include <cerrno>
    #include <climits>
    #include <limits>
    #include "ast.hpp"
    #include "parser.hpp"

	void yyerror(const char *);

    int value=0;
    char* id=NULL;
%}

letter      [A-Za-z]
digit       [0-9]


%x COMMENT



%%
  /* keywords */
print       return T_PRINT;
return      return T_RETURN;
if          return T_IF;
else        return T_ELSE;
while       return T_WHILE;
do          return T_DO;      
new         return T_NEW;
extends     return T_EXTENDS;
int         return T_INT;
boolean     return T_BOOLEAN;
none        return T_NONE;
True        return T_TRUE;
False       return T_FALSE; 
and         return T_AND;
or          return T_OR;
not         return T_NOT;

    /* symbols */
[\+\-\*\/\>\?\:\.\(\)\=\,\{\}\;]    return *yytext;
">="        return T_GEQ;
"=="        return T_EQ;
"->"        return T_ARROW;

    /* comments */
"/*"            BEGIN(COMMENT);
<COMMENT>"*/"   BEGIN(INITIAL);
<COMMENT>.      
<COMMENT>\n     
<COMMENT><<EOF>>    yyerror("unfinished comment");

    /* constants */
{letter}({letter}|{digit})* {
    std::string value = std::string(strdup(yytext));
    yylval.identifier_ptr = new IdentifierNode(value);
    return T_ID;
}

0|[1-9]{digit}*    { 
                        long value = strtol(yytext, NULL, 0); 
                        if (errno != 0 || value > INT_MAX || value < INT_MIN) 
                            yyerror("invalid integer");
                        yylval.integer_ptr = new IntegerNode((int) value); 
                        return T_INTCONST;
                    }

    /* whitespaces */
[ \t\r]+
\n      

    /* invalid */
.                 { yyerror("invalid character"); }

%%

int yywrap(void) {
  return 1;
}
