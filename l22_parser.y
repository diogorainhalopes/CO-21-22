%{
//-- don't change *any* of these: if you do, you'll break the compiler.
#include <algorithm>
#include <memory>
#include <cstring>
#include <cdk/compiler.h>
#include <cdk/types/types.h>
#include ".auto/all_nodes.h"
#define LINE                         compiler->scanner()->lineno()
#define yylex()                      compiler->scanner()->scan()
#define yyerror(compiler, s)         compiler->scanner()->error(s)
// -- don't change *any* of these --- END!
%}

%parse-param {std::shared_ptr<cdk::compiler> compiler}

%union {
  // --- don't change *any* of these: if you do, you'll break the compiler.
  YYSTYPE() : type(cdk::primitive_type::create(0, cdk::TYPE_VOID)) {}
  ~YYSTYPE() {}
  YYSTYPE(const YYSTYPE &other) { *this = other; }
  YYSTYPE& operator=(const YYSTYPE &other) { type = other.type; return *this; }

  std::shared_ptr<cdk::basic_type> type;        /* expression type */
  // -- don't change *any* of these --- END!

  int                   i;	/* integer value */
  double                d;
  std::string          *s;	/* symbol name or string literal */
  cdk::basic_node      *node;	/* node pointer */
  cdk::sequence_node   *sequence;
  cdk::expression_node *expression; /* expression nodes */
  cdk::lvalue_node     *lvalue;

  std::vector<std::shared_ptr<cdk::basic_type>> *types;
  l22::block_node      *bloco;

};

%token <i> tINTEGER
%token <d> tREAL
%token <s> tIDENTIFIER tSTRING
%token <expression> tNULL
%token tINPUT tWRITE tWRITELN
%token tINT tDOUBLE tTEXT tVOID
%token tFOREIGN tUSE tPUBLIC tPRIVATE tVAR
%token tIF tTHEN tELIF tWHILE tDO tSTOP tAGAIN tRETURN
%token tSIZEOF  tBEGIN tEND

%nonassoc tELSE tNOT tFUN

%right '='
%left tAND tOR
%left tGE tLE tEQ tNE '>' '<'
%left '+' '-'
%left '*' '/' '%'
%nonassoc tUNARY
%nonassoc '['
%left '('

%type <s> str
%type <i> qualifier 
%type <node> instr final_instr elif decl program fvar
%type <sequence> file decls instrs final_instrs exprs funcvars
%type <expression> expr function fcall
%type <lvalue> lval
%type <bloco> block
%type <type> dtype ftype vartype 

%type <types> dtypes


%{
//-- The rules below will be included in yyparse, the main parsing function.
%}
%%

file : /* empty */                                { compiler->ast($$ = new cdk::sequence_node(LINE)); }
     | program                                    { compiler->ast($$ = new cdk::sequence_node(LINE, $1)); }
     | decls ';' program                          { compiler->ast($$ = new cdk::sequence_node(LINE, $3, $1)); }
     | decls program                              { compiler->ast($$ = new cdk::sequence_node(LINE, $2, $1)); }
     | decls                                      { compiler->ast($$ = $1); }
     ;

program	: tBEGIN block tEND                     { $$ = new l22::program_node(LINE, $2); }
	     ;

decls: decls ';' decl                              { $$ = new cdk::sequence_node(LINE, $3, $1); }
     | decls decl                                  { $$ = new cdk::sequence_node(LINE, $2, $1); }
     | decl      	                               { $$ = new cdk::sequence_node(LINE, $1); }
     ;


decl : qualifier dtype tIDENTIFIER '=' expr          { $$ = new l22::variable_declaration_node(LINE, $1, $2, *$3, $5); }
     | qualifier dtype tIDENTIFIER '=' function      { $$ = new l22::variable_declaration_node(LINE, $1, $2, *$3, $5); }
     | dtype tIDENTIFIER '=' expr                    { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $4); }
     | dtype tIDENTIFIER '=' function                { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $4); }
     | qualifier vartype tIDENTIFIER '=' expr        { $$ = new l22::variable_declaration_node(LINE, $1, $2, *$3, $5); }
     | qualifier vartype tIDENTIFIER '=' function    { $$ = new l22::variable_declaration_node(LINE, $1, $2, *$3, $5); }
     | vartype tIDENTIFIER '=' expr                  { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $4); }
     | vartype tIDENTIFIER '=' function              { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, $1, *$2, $4); }
     | qualifier tIDENTIFIER '=' expr                { $$ = new l22::variable_declaration_node(LINE, $1, nullptr, *$2, $4); } 
     | qualifier tIDENTIFIER '=' function            { $$ = new l22::variable_declaration_node(LINE, $1, nullptr, *$2, $4); } 
     | qualifier dtype tIDENTIFIER                   { $$ = new l22::variable_declaration_node(LINE, $1, $2, *$3, nullptr); }
     | dtype tIDENTIFIER                             { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); }
     ;

instrs : instrs ';' instr                               { $$ = new cdk::sequence_node(LINE, $3, $1); }
     |  instrs instr                              { $$ = new cdk::sequence_node(LINE, $2, $1); }
     |  instr      	                                 { $$ = new cdk::sequence_node(LINE, $1); }
     ;

instr : tWRITE exprs                              { $$ = new l22::print_node(LINE, $2, false); }
     | tWRITELN exprs                             { $$ = new l22::print_node(LINE, $2, true); }
     | tWHILE '(' expr ')' tDO block              { $$ = new l22::while_node(LINE, $3, $6); }
     | tIF '(' expr ')' tTHEN block               { $$ = new l22::if_node(LINE, $3, $6); }
     | tIF '(' expr ')' tTHEN block elif          { $$ = new l22::if_else_node(LINE, $3, $6, $7); } 
     | expr                                       { $$ = new l22::evaluation_node(LINE, $1); }
     | block                                      { $$ = $1; }
     ;

final_instr : tAGAIN                             { $$ = new l22::again_node(LINE); }
     | tSTOP                                     { $$ = new l22::stop_node(LINE); }
     | tRETURN expr                              { $$ = new l22::return_node(LINE, $2); }
     | tRETURN function                          { $$ = new l22::return_node(LINE, $2); }
     | tRETURN                                   { $$ = new l22::return_node(LINE, nullptr); }
     ;

final_instrs : instrs ';' final_instr                { $$ = new cdk::sequence_node(LINE, $3, $1); }
     | instrs final_instr                           { $$ = new cdk::sequence_node(LINE, $2, $1); }
     | final_instr                                   { $$ = new cdk::sequence_node(LINE, $1); }
     | instrs                                        { $$ = $1; }
     ;

block : '{' decls final_instrs '}'              { $$ = new l22::block_node(LINE, $2, $3); }
     |  '{' decls ';' final_instrs '}'           { $$ = new l22::block_node(LINE, $2, $4); }
     |  '{' decls '}'                        { $$ = new l22::block_node(LINE, $2, nullptr); }
     |  '{' final_instrs '}'                     { $$ = new l22::block_node(LINE, nullptr, $2); }
     |  '{' '}'                               { $$ = new l22::block_node(LINE, nullptr, nullptr); } 
     ;

funcvars : fvar                                   { $$ = new cdk::sequence_node(LINE, $1); }
          | funcvars ',' fvar                     { $$ = new cdk::sequence_node(LINE, $3, $1); }
          ;

fvar: dtype tIDENTIFIER                           { $$ = new l22::variable_declaration_node(LINE, tPRIVATE, $1, *$2, nullptr); }
     ;

dtype : tINT                                      { $$ = cdk::primitive_type::create(4, cdk::TYPE_INT); }
     | tDOUBLE                                    { $$ = cdk::primitive_type::create(8, cdk::TYPE_DOUBLE); }
     | tTEXT                                      { $$ = cdk::primitive_type::create(4, cdk::TYPE_STRING); }
     | tVOID                                      { $$ = cdk::primitive_type::create(0, cdk::TYPE_VOID); }
     | '[' dtype ']'                              { $$ = cdk::reference_type::create(4, $2); }
     | ftype                                      { $$ = $1; }
     ;

dtypes  : dtype                                   { $$ = new std::vector<std::shared_ptr<cdk::basic_type>>(); $$->push_back($1); }
	   | dtypes ',' dtype                        { $$ = $1; $$->push_back($3); }    // sem delete?
        ;

ftype : dtype '<' '>'                             {$$ = cdk::functional_type::create($1); }
      | dtype '<' dtypes '>'                      {$$ = cdk::functional_type::create(*$3, $1); }
     ;

function :  '(' ')' tFUN dtype ':'  block          { $$ = new l22::function_body_node(LINE, $6, $4); }
          | '(' funcvars ')' tFUN dtype ':' block  { $$ = new l22::function_body_node(LINE, $2, $7, $5); }
          ;

fcall : '@' '(' exprs ')'                         { $$ = new l22::function_call_node(LINE, $3); }
      | '@' '(' ')'                               { $$ = new l22::function_call_node(LINE); }
      | expr '(' exprs ')'                        { $$ = new l22::function_call_node(LINE, $3, $1); }
      | expr '(' ')'                              { $$ = new l22::function_call_node(LINE, $1); }
      | '(' function ')' '(' exprs ')'            { $$ = new l22::function_call_node(LINE, $5, $2); }
      | '(' function ')' '('  ')'                 { $$ = new l22::function_call_node(LINE, $2); }
      ;

qualifier : tPUBLIC                               { $$ = tPUBLIC; }
          | tUSE                                  { $$ = tUSE; }
          | tFOREIGN                              { $$ = tFOREIGN; }
          ;

vartype : tVAR                                    { $$ = cdk::primitive_type::create(0, cdk::TYPE_UNSPEC); }
        ;

expr : tINTEGER                                   { $$ = new cdk::integer_node(LINE, $1); }
	| str                                        { $$ = new cdk::string_node(LINE, $1); }
     | tREAL                                      { $$ = new cdk::double_node(LINE, $1); }
     | tNULL                                      { $$ = new l22::nullptr_node(LINE); }
     | tINPUT                                     { $$ = new l22::read_node(LINE); }
     | tNOT expr                                  { $$ = new cdk::not_node(LINE, $2); } 
     | '-' expr %prec tUNARY                      { $$ = new cdk::neg_node(LINE, $2); }
     | '+' expr %prec tUNARY                      { $$ = new l22::identity_node(LINE, $2); }
     | expr '+' expr	                         { $$ = new cdk::add_node(LINE, $1, $3); }
     | expr '-' expr	                         { $$ = new cdk::sub_node(LINE, $1, $3); }
     | expr '*' expr	                         { $$ = new cdk::mul_node(LINE, $1, $3); }
     | expr '/' expr	                         { $$ = new cdk::div_node(LINE, $1, $3); }
     | expr '%' expr	                         { $$ = new cdk::mod_node(LINE, $1, $3); }
     | expr '<' expr	                         { $$ = new cdk::lt_node(LINE, $1, $3); }
     | expr '>' expr	                         { $$ = new cdk::gt_node(LINE, $1, $3); }
     | expr tGE expr	                         { $$ = new cdk::ge_node(LINE, $1, $3); }
     | expr tLE expr                              { $$ = new cdk::le_node(LINE, $1, $3); }
     | expr tEQ expr	                         { $$ = new cdk::eq_node(LINE, $1, $3); }
     | expr tNE expr	                         { $$ = new cdk::ne_node(LINE, $1, $3); }
     | expr tAND expr	                         { $$ = new cdk::and_node(LINE, $1, $3); }
     | expr tOR expr	                         { $$ = new cdk::or_node(LINE, $1, $3); }
     | '(' expr ')'                               { $$ = $2; }
     | '[' expr ']'                               { $$ = new l22::stack_alloc_node(LINE, $2); }
     | tSIZEOF '(' expr ')'                       { $$ = new l22::sizeof_node(LINE, $3); }
     | lval '?'                                   { $$ = new l22::address_of_node(LINE, $1); }
     | lval                                       { $$ = new cdk::rvalue_node(LINE, $1); } 
     | lval '=' expr                              { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | lval '=' function                          { $$ = new cdk::assignment_node(LINE, $1, $3); }
     | fcall                                      { $$ = $1; }
     ;

exprs : expr                                      { $$ = new cdk::sequence_node(LINE, $1);}
     | function                                   { $$ = new cdk::sequence_node(LINE, $1);}
     | exprs ',' expr                             { $$ = new cdk::sequence_node(LINE, $3, $1);}
     | exprs ',' function                         { $$ = new cdk::sequence_node(LINE, $3, $1);}
     ;

str : tSTRING                                     { $$ = new std::string( *$1); }
     | str tSTRING                                { $$ = new std::string(*$1 + *$2); delete($1); delete($2); }     
     ;      

lval : tIDENTIFIER                                { $$ = new cdk::variable_node(LINE, $1); }
     | expr '[' expr ']'                          { $$ = new l22::index_node(LINE, $1, $3); }
     ;
   
elif : tELSE block                               { $$ = $2; }
     | tELIF '(' expr ')' tTHEN block            { $$ = new l22::if_node(LINE, $3, $6); }
     | tELIF '(' expr ')' tTHEN block elif       { $$ = new l22::if_else_node(LINE, $3, $6, $7); }
     ;

%%
