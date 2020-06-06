/* CS152 */
/* A flex scanner specification for the calculator language */

%{   
    #include <stdio.h>
    #include <string.h>
    #include <stdlib.h>
    #include "y.tab.h"
    int currLine = 1, currPos = 1;
%}

DIGIT      [0-9]
ALPHA   [a-zA-Z]
UNDERSCORE   [_]
ALL [0-9a-zA-Z_]
   
%%

               /* Reserved Words */

"function"     {return FUNCTION; currPos += yyleng;}
"beginparams"  {return BEGIN_PARAMS; currPos += yyleng;}
"endparams"    {return END_PARAMS; currPos += yyleng;}
"beginlocals"  {return BEGIN_LOCALS; currPos += yyleng;}
"endlocals"    {return END_LOCALS; currPos += yyleng;}
"beginbody"    {return BEGIN_BODY; currPos += yyleng;}
"endbody"      {return END_BODY; currPos += yyleng;}
"integer"      {return INTEGER; currPos += yyleng;}
"array"        {return ARRAY; currPos += yyleng;}
"of"           {return OF; currPos += yyleng;}
"if"           {return IF; currPos += yyleng;}
"then"         {return THEN; currPos += yyleng;}
"endif"        {return ENDIF; currPos += yyleng;}
"else"         {return ELSE; currPos += yyleng;}
"while"        {return WHILE; currPos += yyleng;}
"do"           {return DO; currPos += yyleng;}
"for"          {return FOR; currPos += yyleng;}
"beginloop"    {return BEGINLOOP; currPos += yyleng;}
"endloop"      {return ENDLOOP; currPos += yyleng;}
"continue"     {return CONTINUE; currPos += yyleng;}
"read"         {return READ; currPos += yyleng;}
"write"        {return WRITE; currPos += yyleng;}  
"and"          {return AND; currPos += yyleng;}
"or"           {return OR; currPos += yyleng;}
"not"          {return NOT; currPos += yyleng;}
"true"         {return TRUE; currPos += yyleng;}
"false"        {return FALSE; currPos += yyleng;}
"return"       {return RETURN; currPos += yyleng;}

               /* Arithmetic Operators */

"-"            {return SUB; currPos += yyleng;}
"+"            {return ADD; currPos += yyleng;}
"*"            {return MULT; currPos += yyleng;}
"/"            {return DIV; currPos += yyleng;}
"%"            {return MOD; currPos += yyleng;}

               /* Comparison Operators */

"=="           {return EQ; currPos += yyleng;}
"<>"           {return NEQ; currPos += yyleng;}
"<"            {return LT; currPos += yyleng;}
">"            {return GT; currPos += yyleng;}
"<="           {return LTE; currPos += yyleng;}
">="           {return GTE; currPos += yyleng;}

               /* Identifiers and Numbers */

{ALPHA}({ALL}*({DIGIT}|{ALPHA})+)*   {yylval.char_val = strdup(yytext); return IDENT; currPos += yyleng;}
{DIGIT}+   {yylval.int_val = atoi(yytext); return NUMBER; currPos += yyleng;}

               /* Other Special Symbols */

";"            {return SEMICOLON; currPos += yyleng;}
":"            {return COLON; currPos += yyleng;}
","            {return COMMA; currPos += yyleng;}
"("            {return L_PAREN; currPos += yyleng;}
")"            {return R_PAREN; currPos += yyleng;}
"["            {return L_SQUARE_BRACKET; currPos += yyleng;}
"]"            {return R_SQUARE_BRACKET; currPos += yyleng;}
":="           {return ASSIGN; currPos += yyleng;}

               /* Comments and White Spaces */

("##").*       {currPos += yyleng;}
[ \t]+         {currPos += yyleng;}
"\n"           {currLine++; currPos = 1;}

               /* Lexical Errors */

({DIGIT}|{UNDERSCORE}){ALL}*   {printf("Error at line %d, column %d: identifier \"%s\" must begin with a letter\n", currLine, currPos, yytext);}

{ALPHA}{ALL}*{UNDERSCORE}+   {printf("Error at line %d, column %d: identifier \"%s\" cannot end with an underscore\n", currLine, currPos, yytext);}

.   {printf("Error at line %d, column %d: unrecognized symbol \"%s\"\n", currLine, currPos, yytext);}

%%
