%{
    #include <cstdio>
    #include "node.h"
    ExpressionList *file; /* the top level root node of our final AST */

    extern int yylex();
    void yyerror(const char *s) { printf("ERROR: %s\n", s); }
    static NExpression *clean(NExpression *arg) {
        NExpression *ret = arg;
        NComparisonOperator *x = dynamic_cast<NComparisonOperator *>(arg);
        while (x) {
            ret = x;
            if (x->l.size() == 1) {
                x = dynamic_cast<NComparisonOperator *>(x->l[0]);
                if (!x) { ret = dynamic_cast<NComparisonOperator *>(ret)->l[0]; break; }
            } else { break; }
        }
        return ret;
    }
%}

/* Represents the many different ways we can access our data */
%union {
    NExpression *expr;
    NTuple *tuple;
    NArray *array;
    ExpressionList *exprvec;
    std::string *string;
    int token;
}

/* Define our terminal symbols (tokens). This should
   match our tokens.l lex file. We also define the node type
   they represent.
 */
%token <string> TIDENTIFIER TINTEGER TSTRING
%token <token> TSPACE TARROW
%token <token> TQUES TCOL TASSIGN TSHRS TCOR TCXOR TCAND TCEQ TCNE TCLE TCGE
%token <token> TSHR TSHL TCGT TCLT TOR TXOR TAND TPLUS TMINUS TMUL TDIV TMOD
%token <token> TUNDER TLPAREN TRPAREN TLBRACK TRBRACK TCOMMA

/* Define the type of node our nonterminal symbols represent.
   The types refer to the %union declaration above. Ex: when
   we call an ident (defined by union type ident) we are really
   calling an (NIdentifier*). It makes the compiler happy.
 */

%type <exprvec> file exprs expr_list
%type <expr> expr expr_control expr_assign expr_lambda
%type <expr> expr_logical_or expr_logical_and expr_logical_xor
%type <expr> expr_comp_eq expr_comp_lt
%type <expr> expr_bit expr_add expr_mul expr_unary
%type <expr> expr_apply expr_func_type
%type <expr> expr_atom
%type <expr> expr_tuple expr_array
%type <expr> ident integer string
%type <token> COMP_EQ COMP_LT COMP_BIT COMP_ADD COMP_MUL

%start file

%expect 4
%glr-parser

%%

file : { file = new ExpressionList; }
     | exprs { file = $1; }
     ;

exprs : expr { $$ = new ExpressionList; $$->push_back($<expr>1); }
      | exprs expr { $1->push_back($<expr>2); }
      ;

expr : expr_assign
     | expr_control
     | expr_lambda
     ;

expr_control : expr TQUES expr TCOL expr { $$ = new NControl(*$1, *$3, *$5); } ;

expr_assign : expr_lambda TASSIGN expr_lambda { $$ = new NAssign(*$1, *$3); } ;

expr_lambda : expr_tuple TARROW expr_func_type TSPACE expr { $$ = new NLambda(*$1, *$3, *$5); }
            | expr_logical_or { $$ = clean($1); };
            ;

expr_logical_or : expr_logical_xor { $$ = new NComparisonOperator(TCOR); dynamic_cast<NComparisonOperator *>($$)->l.push_back(clean($<expr>1)); }
                | expr_logical_or TCOR expr_logical_xor { dynamic_cast<NComparisonOperator *>($1)->l.push_back(clean($<expr>3)); }
                ;

expr_logical_xor : expr_logical_and { $$ = new NComparisonOperator(TCXOR); dynamic_cast<NComparisonOperator *>($$)->l.push_back(clean($<expr>1)); }
                 | expr_logical_xor TCXOR expr_logical_and { dynamic_cast<NComparisonOperator *>($1)->l.push_back(clean($<expr>3)); }
                 ;

expr_logical_and : expr_comp_eq { $$ = new NComparisonOperator(TCAND); dynamic_cast<NComparisonOperator *>($$)->l.push_back($<expr>1); }
                 | expr_logical_and TCAND expr_comp_eq { dynamic_cast<NComparisonOperator *>($1)->l.push_back($<expr>3); }
                 ;

expr_comp_eq : expr_comp_lt COMP_EQ expr_comp_lt { $$ = new NBinaryOperator(*$1, $2, *$3); }
             | expr_comp_lt
             ;

expr_comp_lt : expr_bit COMP_LT expr_bit { $$ = new NBinaryOperator(*$1, $2, *$3); }
             | expr_bit
             ;

expr_bit : expr_add COMP_BIT expr_add { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr_add
         ;

expr_add : expr_mul COMP_ADD expr_mul { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr_mul
         ;

expr_mul : expr_unary COMP_MUL expr_unary { $$ = new NBinaryOperator(*$1, $2, *$3); }
         | expr_unary
         ;

expr_unary : TUNDER expr_apply { $$ = new NUnaryOperator(*$2); }
           | expr_apply
           ;

expr_apply : expr_func_type TSPACE expr_apply { $$ = new NApply(*$1, *$3); }
           | expr_func_type
           ;

expr_func_type : expr_atom TARROW expr_func_type { $$ = new NFuncType(*$1, *$3); }
               | expr_atom
               ;

expr_list : expr { $$ = new ExpressionList; $$->push_back($<expr>1); }
          | expr_list TCOMMA expr { $1->push_back($<expr>3); }
          ;

expr_tuple : TLPAREN expr_list TRPAREN { $$ = new NTuple; dynamic_cast<NTuple *>($$)->l = *$2; }
           ;

expr_array : TLBRACK expr_list TRBRACK { $$ = new NArray; dynamic_cast<NArray *>($$)->l = *$2; }
           ;

expr_atom : ident
          | integer
          | string
          | expr_tuple
          | expr_array
          ;

ident : TIDENTIFIER { $$ = new NIdentifier(*$1); delete $1; }
      ;

string : TSTRING { $$ = new NString(*$1); delete $1; }
       ;

integer : TINTEGER { $$ = new NInteger(atol($1->c_str())); delete $1; }
        ;

COMP_EQ  : TCEQ | TCNE ;
COMP_LT  : TCLE | TCGE | TCLT | TCGT ;
COMP_ADD : TPLUS | TMINUS ;
COMP_MUL : TMUL | TDIV | TMOD ;
COMP_BIT : TOR | TXOR | TAND | TSHL | TSHR | TSHRS ;

%%
