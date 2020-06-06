/* calculator. */
%{
 #include <iostream>
 #include <stdio.h>
 #include <stdlib.h>
 #include <string>
 #include <string.h>
 #include <fstream>
 #include <sstream>
 #include <map>
 #include <stack>
 #include <vector>

 using namespace std;

 void yyerror(const char *msg);
 int yylex(void);
 extern int currLine;
 extern int currPos;
 extern FILE * yyin;

 enum symbol_type {INT, INTARRAY, FUNC};

 struct Func 
 {
   string name;
   Func(): name() {}
   Func(string n) 
   : name(n) {}
 };

 struct Sym 
 {
   int val;
   int size;
   string name;
   symbol_type type; 
   Sym():val(0),size(0),name(),type() {}
   Sym(int v, int s, string n, symbol_type t) 
   :val(v), size(s), name(n), type(t) {}
 };

 stack <string> ident_stack;
 stack <string> var_stack;
 stack <int> var_type_stack;
 stack <char*> var_index_stack;
 stack <string> exp_stack;
 stack <string> param_stack;
 stack <string> label_stack;
 
 string make_temp();
 string make_label();

 void check_add_symbol(Sym sym);
 void check_add_func(Func func);
 void check_symbol(string name);
 void check_func(string name);
 void check_array_size(int size);
 
 map <string, Sym> symbol_table;
 map <string, Func> func_table;

 int temp_cnt = 0;
 int label_cnt = 0;
 int param_cnt = 0;
 bool main_exists = 0;
 bool error_detection = true;
%}

%union{
  int int_val;
  char *char_val;

  struct attributes {
    char name[255];
    char index[255];
    int type;
    int val;
    int size_attr;
  } attr;
}

%error-verbose
%start prog_start

/* Bison Declartions */ 

%token FUNCTION
%token BEGIN_PARAMS
%token END_PARAMS
%token BEGIN_LOCALS
%token END_LOCALS
%token BEGIN_BODY
%token END_BODY
%token INTEGER
%token ARRAY
%token OF
%token IF
%token THEN
%token ENDIF
%token ELSE
%token WHILE
%token DO
%token FOR
%token BEGINLOOP
%token ENDLOOP
%token CONTINUE
%token READ
%token WRITE
%left AND
%left OR
%right NOT
%token TRUE
%token FALSE
%token RETURN

%left SUB
%left ADD
%left MULT
%left DIV
%left MOD
%right UMINUS

%left EQ
%left NEQ
%left LT
%left GT
%left LTE
%left GTE

%token <int_val> NUMBER
%token <char_val> IDENT

%token SEMICOLON
%token COLON
%token COMMA
%left L_PAREN
%left R_PAREN
%left L_SQUARE_BRACKET
%left R_SQUARE_BRACKET
%right ASSIGN

%type <attr> Var Expression Term Term_branch TermIdentifier Declaration Identifier_loop Statement Statement1 Statement2 Statement3 Statement4 Statement5 Statement6 Statement7 Statement8 Statement9 Multiplicative_Expr Bool_Expr Relation_And_Expr Relation_Expr Relation_Expr_branch
%type <char_val> Comp

%% /* Grammar Rules */
prog_start
      : Program
      {
            if(!main_exists)
            {
                  yyerror("Main is not declared.");
            }
      }
      ;

Program
      : 
        | Function Program 
        ;

Function
      : FUNCTION IDENT 
      {
            Func f($2);
            check_add_func(f);
            if(error_detection)
            {
                  cout << "func " << string($2) << endl;
            }
      }
      SEMICOLON BEGIN_PARAMS Declaration_loop
      {
            while (!param_stack.empty())
            {
                  if(error_detection)
                  {
                        cout << "= " << param_stack.top() << ", " << "$" << param_cnt << endl;
                  }
                  param_cnt++;
                  param_stack.pop();
            }
            param_cnt = 0;
      } 
      END_PARAMS BEGIN_LOCALS Declaration_loop END_LOCALS BEGIN_BODY Statement_loop END_BODY 
      {
            if(error_detection)
            {
                  cout << "endfunc" << "\n" << endl;
            }
            symbol_table.clear();
            if (strcmp($2, "main") == 0) {
                  main_exists = 1;      
            }
            while (!param_stack.empty()) {
                  param_stack.pop();
            }
      }
      ;

Declaration_loop
      : Declaration SEMICOLON Declaration_loop 
      | 
      ;

Statement_loop
      : Statement SEMICOLON Statement_loop 
      | 
      ;

Declaration
      : Identifier_loop COLON Declaration_branch 
      ;

Identifier_loop
      : IDENT 
      {
            ident_stack.push($1);
            param_stack.push($1);
      }
      | IDENT COMMA Identifier_loop 
      {
            ident_stack.push($1);
            param_stack.push($1);
      }
      ;

Declaration_branch
      : INTEGER
      {
            while(!ident_stack.empty()) {
                  string temp = ident_stack.top();
                  Sym sym(0,0,temp,INT); 
                  check_add_symbol(sym);
                  if(error_detection)
                  {
                        cout << ". " << temp << endl;
                  }
                  ident_stack.pop(); 
            }
      }
      | ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
      {
            while(!ident_stack.empty()) 
            {
                  string temp = ident_stack.top();
                  check_array_size($3);
                  Sym sym(0,$3,temp,INTARRAY);
                  check_add_symbol(sym);
                  if(error_detection)
                  {
                        cout << ".[] " << temp << ", " << $3 << endl;
                  }
                  ident_stack.pop(); 
            }
      }
      | ARRAY L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET L_SQUARE_BRACKET NUMBER R_SQUARE_BRACKET OF INTEGER
      {
            while(!ident_stack.empty()) 
            {
                  string temp = ident_stack.top();
                  check_array_size($3);
                  check_array_size($6);
                  Sym sym(0,$3,temp,INTARRAY);
                  check_add_symbol(sym);
                  if(error_detection)
                  {
                        cout << ".[] " << temp << ", " << $3 << endl;
                  }
                  ident_stack.pop(); 
            }
      }
      ;

Statement
      : Statement1 
      | Statement2 
      | Statement3 
      | Statement4 
      | Statement5 
      | Statement6 
      | Statement7 
      | Statement8 
      | Statement9 
      {
            $$.val = $1.val;
            strcpy($$.name,$1.name);
      }
      ;

Statement1
      : Var ASSIGN Expression 
      {
            if ($1.type == 0) 
            {
                  if(error_detection)
                  {
                        cout << "= " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
                  }
            }
            else 
            {
                  if(error_detection)
                  {
                        cout << "[]= " << const_cast<char*>($1.name) << ", " << const_cast<char*>($1.index) << ", " << const_cast<char*>($3.name) << endl;
                  }
            } 
      }
      ;

Statement2
      : IF Bool_Expr THEN 
      {
            string IfTure = make_label();
            string EndIf = make_label();
            label_stack.push(EndIf); 
            if(error_detection)
            {
                  cout << "?:= " << IfTure << ", " << const_cast<char*>($2.name) << endl;
                  cout << ":= " << EndIf << endl;
                  cout << ": " << IfTure << endl;
            }
      }
      Statement_loop ENDIF 
      {
            if(error_detection)
            {
                  cout << ": " << label_stack.top() << endl;
            }
            label_stack.pop();
      }
      | IF Bool_Expr THEN
      {
            string IfTure = make_label();
            string IfFalse = make_label();
            label_stack.push(IfFalse); 
            if(error_detection)
            {
                  cout << "?:= " << IfTure << ", " << const_cast<char*>($2.name) << endl;
                  cout << ":= " << IfFalse << endl;
                  cout << ": " << IfTure << endl;
            }
      }
      Statement_loop ELSE 
      {
            string EndIf = make_label(); 
            if(error_detection)
            {
                  cout << ":= " << EndIf << endl;
                  cout << ": " << label_stack.top() << endl;
            }
            label_stack.pop();
            label_stack.push(EndIf);
      }
      Statement_loop ENDIF 
      {
            if(error_detection)
            {
                  cout << ": " << label_stack.top() << endl;
            }
            label_stack.pop();
      }
      ;

Statement3
      : WHILE Bool_Expr BEGINLOOP 
      {
            string conditional = make_label();
            string end = make_label();
            string start = make_label();

            if(error_detection)
            {
                  cout << ": " << start << endl;
                  cout << "?:= " << conditional << ", " << const_cast<char*>($2.name) << endl;
                  cout << ":= " << end << endl;
                  cout << ": " << conditional << endl;
            }

            label_stack.push(start);
            label_stack.push(end);
      }
      Statement_loop ENDLOOP 
      {
            string end = label_stack.top();
            label_stack.pop();

            string start = label_stack.top();
            label_stack.pop();

            if(error_detection)
            {
                  cout << ":= " << start << endl;
                  cout << ": " << end << endl;
            }
      }
      ;

Statement4
      : DO BEGINLOOP
      {
            string start = make_label();
            label_stack.push(start);
            if(error_detection)
            {
                  cout << ": " << start << endl;
            }
      } 
      Statement_loop ENDLOOP WHILE Bool_Expr 
      {
            string start = label_stack.top();
            if(error_detection)
            {
                  cout << "?:= " << start << ", " << const_cast<char*>($7.name) << endl;
            }
            label_stack.pop(); 
      }
      ;

Statement5
      : FOR Var ASSIGN NUMBER 
      {
            if(error_detection)
            {
                  cout << "= " << const_cast<char*>($2.name) << ", " << $4 << endl;
            }
      }
      SEMICOLON Bool_Expr 
      {
            string conditional = make_label();
            string end = make_label();
            string start = make_label();

            if(error_detection)
            {
                  cout << ": " << start << endl;
                  cout << "?:= " << conditional << ", " << const_cast<char*>($2.name) << endl;
                  cout << ":= " << end << endl;
                  cout << ": " << conditional << endl;
            }

            label_stack.push(start);
            label_stack.push(end);
      }
      SEMICOLON Var ASSIGN Expression 
      {
            if(error_detection)
            {
                  cout << "= " << const_cast<char*>($10.name) << ", " << const_cast<char*>($12.name) << endl;
            }
      }
      BEGINLOOP Statement_loop ENDLOOP 
      {
            string end = label_stack.top();
            label_stack.pop();

            string start = label_stack.top();
            label_stack.pop();

            if(error_detection)
            {
                  cout << ":= " << start << endl;
                  cout << ": " << end << endl;
            }
      }
      ;

Statement6
      : READ Var_loop 
      {
            while (!var_stack.empty()) {
                  if (var_type_stack.top() == 0) 
                  {
                        if(error_detection)
                        {
                              cout << ".< " << var_stack.top() << endl;
                        }
                        var_stack.pop();
                        var_type_stack.pop();
                        var_index_stack.pop();
                  }
                  else 
                  {
                        if(error_detection)
                        {
                              cout << ".[]< " << var_stack.top() << ", " << var_index_stack.top() << endl;
                        }
                        var_stack.pop();
                        var_type_stack.pop();
                        var_index_stack.pop();
                  }
            }
      }
      ;

Statement7
      : WRITE Var_loop 
      {
            while (!var_stack.empty()) {
                  if (var_type_stack.top() == 0) 
                  {
                        if(error_detection)
                        {
                              cout << ".< " << var_stack.top() << endl;
                        }
                        var_stack.pop();
                        var_type_stack.pop();
                        var_index_stack.pop();
                  }
                  else 
                  {
                        if(error_detection)
                        {
                              cout << ".[]< " << var_stack.top() << ", " << var_index_stack.top() << endl;
                        }
                        var_stack.pop();
                        var_type_stack.pop();
                        var_index_stack.pop();
                  }
            }
      }
      ;

Statement8
      : CONTINUE 
      {
            if (!label_stack.empty()) 
            {
                  if(error_detection)
                  {
                        cout << ":= " << label_stack.top() << endl;
                  }
            }
            else 
            {
                  yyerror("Continue statement not within a loop.");
            }
      }
      ;

Statement9
      : RETURN Expression 
      {
            $$.val = $2.val;
            strcpy($$.name,$2.name);
            if(error_detection)
            {
                  cout << "ret " << const_cast<char*>($2.name) << endl;
            }
      }
      ;

Var_loop
      : Var 
      {
            var_stack.push($1.name);
            var_type_stack.push($1.type);
            var_index_stack.push($1.index);
      }
      | Var COMMA Var_loop 
      {
            var_stack.push($1.name);
            var_type_stack.push($1.type);
            var_index_stack.push($1.index);
      }
      ;

Bool_Expr
      : Relation_And_Expr
      {
            strcpy($$.name, $1.name);
      }
      | Bool_Expr OR Relation_And_Expr
      {
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "|| " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
            }
      }
      ;

Relation_And_Expr
      : Relation_Expr
      {
            strcpy($$.name, $1.name);
      }
      | Relation_And_Expr AND Relation_Expr
      {
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "&& " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
            }
      }
      ;

Relation_Expr
      : Relation_Expr_branch 
      {
            strcpy($$.name, $1.name);
      }
      | NOT Relation_Expr_branch 
      {
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {
                  cout << "! " << temp << const_cast<char*>($2.name) << endl;
            }
      }
      ;

Relation_Expr_branch
      : Expression Comp Expression 
      {
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << $2 << " " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
            }
      }
      | TRUE 
      {
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "= " << temp << ", " << "1" << endl;
            }
      }
      | FALSE 
      {
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "= " << temp << ", " << "0" << endl;
            }
      }
      | L_PAREN Bool_Expr R_PAREN 
      {
            strcpy($$.name, $2.name);
      }
      ;

Comp
      : EQ { $$ = const_cast<char*>("=="); } 
      | NEQ { $$ = const_cast<char*>("!="); }
      | LT { $$ = const_cast<char*>("<"); }
      | GT { $$ = const_cast<char*>(">"); }
      | LTE { $$ = const_cast<char*>("<="); }
      | GTE { $$ = const_cast<char*>(">="); }
      ;

Expression
      : Expression ADD Multiplicative_Expr 
      {
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "+ " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
            }
      }
      | Expression SUB Multiplicative_Expr 
      {
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "- " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
            }
      }
      | Multiplicative_Expr
      {
            strcpy($$.name,$1.name);
            $$.type = $1.type;
      }
      ;

Multiplicative_Expr
      : Multiplicative_Expr MULT Term 
      {
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "* " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
            }
      }
      | Multiplicative_Expr DIV Term  
      {
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "/ " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
            }
      }
      | Multiplicative_Expr MOD Term 
      {
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {     
                  cout << ". " << temp << endl;
                  cout << "% " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($3.name) << endl;
            }
      } 
      | Term
      {
            strcpy($$.name,$1.name);
            $$.type = $1.type;
      }
      ;

Term
      : SUB Term_branch 
      {
            $$.val = $2.val * -1;
            $$.type = $2.type;
            string temp = make_temp();
            strcpy($$.name,temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "- " << temp <<  ", " << "0" << ", " << const_cast<char*>($2.name) << endl;
            }
      }
      | Term_branch 
      {
            strcpy($$.name,$1.name);
            $$.val = $1.val;
            $$.type = $1.type;
      }
      | TermIdentifier
      ;

Term_branch
      : Var 
      {
            $$.val = $1.val;
            $$.type = $1.type;
            string temp = make_temp();
            strcpy($$.name,temp.c_str());
            if ($1.type != 1) {
                  if(error_detection)
                  {
                        cout << ". " << temp << endl;
                        cout << "= " << temp <<  ", " << const_cast<char*>($1.name) << endl;
                  }
            }
            else if ($1.type == 1) { 
                  if(error_detection)
                  {
                        cout << ". " << temp << endl;
                        cout << "=[] " << temp << ", " << const_cast<char*>($1.name) << ", " << const_cast<char*>($1.index) << endl;
                  }
            }
      }
      | NUMBER 
      {
            $$.val = $1;
            $$.type = 0;    
            string temp = make_temp();
            strcpy($$.name, temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "= " << temp <<  ", " << $1 << endl;
            }
      }
      | L_PAREN Expression R_PAREN 
      {
            strcpy($$.name, $2.name);
      }
      ;

TermIdentifier
      : IDENT L_PAREN TermExpression_loop R_PAREN 
      {
            check_func(const_cast<char*>($1));
            while (!exp_stack.empty()){
                  if(error_detection)
                  {
                        cout << "param " << exp_stack.top() << endl;
                  }
                  exp_stack.pop();
            }
            string temp = make_temp();
            strcpy($$.name,temp.c_str());
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "call " << const_cast<char*>($1) << ", " << temp << endl;
            }
      }
      | IDENT L_PAREN R_PAREN 
      {
            check_func(const_cast<char*>($1));
            string temp = make_temp();
            if(error_detection)
            {
                  cout << ". " << temp << endl;
                  cout << "call " << const_cast<char*>($1) << ", " << temp << endl;
            }
            strcpy($$.name,temp.c_str());
      }
      ;

TermExpression_loop
      : Expression 
      {
            exp_stack.push($1.name);
      }
      | Expression COMMA TermExpression_loop
      {
            exp_stack.push($1.name);
      }
      ;

Var
      : IDENT 
      {
            check_symbol($1);
            if(symbol_table[$1].type == INTARRAY) 
            {
                  yyerror("Incompatible datatype.");
            }
            else 
            {
                  strcpy($$.name,$1);
                  $$.type = 0;
            }
      }
      | IDENT L_SQUARE_BRACKET Expression R_SQUARE_BRACKET 
      {
            check_symbol($1);
            if(symbol_table[$1].type == INT) 
            {
                  yyerror("Incompatible datatype.");
            }
            else 
            {
                  if ($3.type == 1) 
                  {
                        string temp = make_temp();
                        $$.type = 1;
                        strcpy($$.name, $1);
                        strcpy($$.index, temp.c_str());
                        if(error_detection)
                        {
                              cout << ". " << temp << endl; 
                              cout << "=[] " << temp << ", " << const_cast<char*>($3.name) << ", " << const_cast<char*>($3.index) << endl;
                        }
                  }
                  else 
                  {
                        strcpy($$.name, $1);
                        $$.type = 1;
                        strcpy($$.index, $3.name);
                  }
            }
      }
      | IDENT L_SQUARE_BRACKET Expression R_SQUARE_BRACKET L_SQUARE_BRACKET Expression R_SQUARE_BRACKET 
      {
            check_symbol($1);
            if(symbol_table[$1].type == INT) 
            {
                  yyerror("Incompatible datatype.");
            }
            else 
            {
                  if ($3.type == 1) 
                  {
                        string temp = make_temp();
                        $$.type = 1;
                        strcpy($$.name, $1);
                        strcpy($$.index, temp.c_str());
                        if(error_detection)
                        {
                              cout << ". " << temp << endl; 
                              cout << "=[] " << temp << ", " << const_cast<char*>($3.name) << ", " << const_cast<char*>($3.index) << endl;
                        }
                  }
                  else 
                  {
                        strcpy($$.name, $1);
                        $$.type = 1;
                        strcpy($$.index, $3.name);
                  }
            }
      }
      ;

%%

int main(int argc, char **argv) {
   if (argc > 1) {
      yyin = fopen(argv[1], "r");
      if (yyin == NULL){
         printf("syntax: %s filename\n", argv[0]);
      }//end if
   }//end if
   yyparse(); // Calls yylex() for tokens.
   return 0;
}

void yyerror(const char *msg) {
   printf("Error Line %d: %s\n", currLine, msg);
   error_detection = false;
}

void yyerror(string message) {
  cout << "Error line " << currLine << ": " << message << endl;
  error_detection = false;
}

string make_temp() {
  stringstream ss;
  ss << temp_cnt++;
  string temp = "__temp__" + ss.str();
  return temp;
}

string make_label() {
  stringstream ss;
  ss << label_cnt++;
  string temp = "__label__" + ss.str();
  return temp;
}

void check_add_symbol(Sym s) {
  if (symbol_table.find(s.name) == symbol_table.end()) {
    symbol_table[s.name] = s;
  }
  else {
    string error = "Symbol \"" + s.name + "\" is multiply-defined." ;
    yyerror(error);
  }
}

void check_add_func(Func f) {
  if (func_table.find(f.name) == func_table.end()) {
    func_table[f.name] = f;
  }
  else {
    string error = "Function \"" + f.name + "\" is multiply-defined.";
    yyerror(error);
  }
}

void check_symbol(string name) {
  if(symbol_table.find(name) == symbol_table.end()) {
    string error = "Used variable \"" + name + "\" was not previously declared.";
    yyerror(error);
  }
}

void check_func(string name) {
  if(func_table.find(name) == func_table.end()) {
    string error = "Function \"" + name + "\" was not previously declared.";
    yyerror(error);
  }
}

void check_array_size(int size) {
  if(size <= 0) {
    string error = "The size of array can not equal or less than 0.";
    yyerror(error);
  }
}
