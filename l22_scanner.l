%option c++ prefix="l22_scanner_" outfile="l22_scanner.cpp"
%option stack noyywrap yylineno 8bit debug
%{ 
// make relevant includes before including the parser's tab file
#include <string>
#include <sstream>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/expression_node.h>
#include <cdk/ast/lvalue_node.h>
#include "l22_parser.tab.h"

static bool restart_functions = false;

static int lLevelsToPop = 0, lIndex = 0;
static std::vector<int> indents;   // known indentation levels
static std::vector<int> deferred;  // deferred tokens

static std::ostringstream strlit;

inline bool valid(int indent) {
    lLevelsToPop = lIndex = -1;
    bool indentOk = false;
    for (size_t ix = 0; ix < indents.size(); ix++) {
        if (indent == indents[ix]) {
            lLevelsToPop = indents.size() - ix - 1;
            lIndex = ix;
            indentOk = true;
        }
    }
    return indentOk;
}

// don't change this
#define yyerror LexerError
%}
%x X_COMMENT X_STRING X_SPECIAL X_NEWLINE X_STRING_IGNORE

%%
  

  {
        yydebug=1; set_debug(1);
        //std::cerr << "################" << std::endl;

        if (restart_functions) {
          std::cerr << "POPPING FUNCTIONS (" << indents.size() << ")" << std::endl;
          indents.pop_back();
          if (indents.size() == 0) restart_functions = false;
          return '}';
        }
        else {
          if (lLevelsToPop > 0) {
            std::cerr << "POPPING LEVELS (" << lLevelsToPop << ")" << std::endl;
            lLevelsToPop--;
            indents.pop_back();
            return '}';
          }
        }
        
  }

  /* ====================================================================== */
  /* ====[                      INDENTATION                           ]==== */
  /* ====================================================================== */

\n                   yy_push_state(X_NEWLINE); /* delay decision */

<X_NEWLINE>^[ ]+  { yy_pop_state();
    if (valid(yyleng)) {
          std::cerr << "CASE 1" << std::endl;
          if (lLevelsToPop == 0) {
            // at the same level: ignore index
            // do not return tINDENT;
            std::cerr << "CASE 1a (levels to pop = 0)" << std::endl;
            return ';';
          }
          else {
            // nothing is done here: the scanner will ignore the input while levels > 0
            // and send block-end tokens to the parser
            std::cerr << "CASE 1b (levels to pop = " << lLevelsToPop << ")" << std::endl;
            // at least one has to be returned here...
            if (lLevelsToPop > 0) {
              std::cerr << "POPPING LEVELS (" << lLevelsToPop << ")" << std::endl;
              lLevelsToPop--;
              indents.pop_back();
              return '}';
            }
          }
    }
    else if (indents.size() == 0) {
          // first block to open
          std::cerr << "CASE 2" << std::endl;
          indents.push_back(yyleng);
          return '{';
    }
    else if (yyleng > indents.back()) {
          // this is like the one before, but not the first
          std::cerr << "CASE 3" << std::endl;
          indents.push_back(yyleng);
          return '{';
    }
    else {
          // something wrong: bad indent: communicate to user
          std::cerr << "CASE 4" << std::endl;
          std::cerr << "bad indent at line " << yylineno;
    }

}
<X_NEWLINE>\n      ; // ignore successive newline chars
<X_NEWLINE>.       {
  // in this case, we have a new function starting
  // first, return whatever we are seeing to the input
  yyless(0);
  yy_pop_state();
  if (indents.size() > 0) {
    std::cerr << "should restart " << indents.size() << " functions at " << yylineno << std::endl;
    indents.pop_back();
    if (indents.size() > 0) { restart_functions = true; }
    return '}';
  }
  else return ';'; // top-level declarations
}




 /* ====================================================================== */
 /* ====[                        OPERADORES                          ]==== */
 /* ====================================================================== */

">="             return tGE; 
"<="             return tLE; 
"=="             return tEQ; 
"!="             return tNE; 
"->"             return tFUN;

"and"            return tAND; 
"or"             return tOR; 
"not"            return tNOT; 

  /* ====================================================================== */
  /* ====[                       READ & WRITE	                        ]==== */
  /* ====================================================================== */

"input"          return tINPUT; 
"write"          return tWRITE; 
"writeln"        return tWRITELN;

  /* ====================================================================== */
  /* ====[                           TIPOS	                          ]==== */
  /* ====================================================================== */

"int"            return tINT; 
"double"         return tDOUBLE; 
"text"           return tTEXT; 
"void"           return tVOID; 

  /* ====================================================================== */
  /* ====[                        DECLARACOES                         ]==== */
  /* ====================================================================== */

"foreign"        return tFOREIGN;
"use"            return tUSE;
"public"         return tPUBLIC;
"var"            return tVAR;

  /* ====================================================================== */
  /* ====[                        INSTRUCOES	                        ]==== */
  /* ====================================================================== */

"if"             return tIF; 
"then:"           return tTHEN; 
"elif"           return tELIF; 
"else:"           return tELSE; 

"while"          return tWHILE; 
"do:"             return tDO; 

"stop"           return tSTOP; 
"again"          return tAGAIN; 
"return"         return tRETURN; 

  /* ====================================================================== */
  /* ====[                           OUTROS	                          ]==== */
  /* ====================================================================== */
"sizeof"         return tSIZEOF;
"null"           return tNULL; 
"begin"          return tBEGIN; 
"end"            return tEND; 


[A-Za-z][A-Za-z0-9_]*          yylval.s = new std::string(yytext); return tIDENTIFIER; 



  /* ====================================================================== */
  /* ====[                           STRING	                          ]==== */
  /* ====================================================================== */


"\""                          yy_push_state(X_STRING);
<X_STRING>"\\"                yy_push_state(X_SPECIAL);
<X_STRING>"..."\n             ;
<X_STRING>"\""                {
                                yylval.s = new std::string(strlit.str());
                                strlit.str("");
                                yy_pop_state();
                                return tSTRING;
                              }
<X_STRING>\n                  yyerror("Error in string");
<X_STRING>.                   strlit << *yytext;

<X_SPECIAL>0                  { yy_pop_state(); yy_pop_state(); yy_push_state(X_STRING_IGNORE);}
<X_SPECIAL>00                 { yy_pop_state(); yy_pop_state(); yy_push_state(X_STRING_IGNORE);}
<X_SPECIAL>000                { yy_pop_state(); yy_pop_state(); yy_push_state(X_STRING_IGNORE);}

<X_SPECIAL>n              { strlit << '\n'; yy_pop_state(); }
<X_SPECIAL>r              { strlit << '\r'; yy_pop_state(); }
<X_SPECIAL>t              { strlit << '\t'; yy_pop_state(); }
<X_SPECIAL>"\""           { strlit << '"'; yy_pop_state(); }
<X_SPECIAL>"\\"           { strlit << "\\"; yy_pop_state(); }
<X_SPECIAL>[0-6]{1,3}     { try { 
                              strlit << (char)(unsigned char)std::stoul(yytext, nullptr, 7);
                            } catch (std::out_of_range& e) { yyerror("String overflow.");}
                            yy_pop_state(); }

<X_SPECIAL>.			         { strlit << *yytext; yy_pop_state(); }

<X_STRING_IGNORE>"\""         {
                                yylval.s = new std::string(strlit.str());
                                strlit.str("");
                                yy_pop_state();
                                return tSTRING;
                              }
<X_STRING_IGNORE>\\\"        	;
<X_STRING_IGNORE>\\\\         ;
<X_STRING_IGNORE>.|\n 	     	;

  /* ====================================================================== */
  /* ====[                        COMENTARIOS                         ]==== */
  /* ====================================================================== */

"..."\n                         ;

";".*                         ; /* ignore comments */
"(*"                           yy_push_state(X_COMMENT); 
<X_COMMENT>"(*"                yy_push_state(X_COMMENT); 
<X_COMMENT>"*)"                yy_pop_state(); 
<X_COMMENT>.|\n                ; /* ignore comments */





  
  /* ====================================================================== */
  /* ====[                         INTEIROS                           ]==== */
  /* ====================================================================== */


[0][0-6]*                       {  try { 
                                    yylval.i = std::strtol(yytext, nullptr, 7);
                                    return tINTEGER;
                                  } catch (std::out_of_range& e) { yyerror("Integer overflow.");}
                                }

[1-9][0-9]*                     {  try { 
                                    yylval.i = std::strtol(yytext, nullptr, 10);
                                    return tINTEGER;
                                  } catch (std::out_of_range& e) { yyerror("Integer overflow.");}
                                }


  /* ====================================================================== */
  /* ====[                           REAIS                            ]==== */
  /* ====================================================================== */

(([0-9]*\.[0-9]+|[0-9]+\.[0-9]*)([eE][-+]?[0-9]+)?)|([0-9]+[eE][-+]?[0-9]+) { 
                                                            try { yylval.d = strtod(yytext, NULL); 
                                                              return tREAL; 
                                                            } catch (std::out_of_range& e) { yyerror("Real out of range."); }
                                                      }

 /* ====================================================================== */
 /* ====[                        OUTROS CHARS                        ]==== */
 /* ====================================================================== */

           
[-]               {return *yytext;} 
[+]               {return *yytext;} 
[%]               {return *yytext;} 
[]]               {return *yytext;} 
[(]               {return *yytext;} 
[)]               {return *yytext;} 
[<]               {return *yytext;} 
[>]               {return *yytext;} 
[*]               {return *yytext;} 
[/]               {return *yytext;} 
[?]               {return *yytext;} 
[,]               {return *yytext;} 
[[]               {return *yytext;} 
[:]               {return *yytext;} 
[=]               {return *yytext;} 
[@]               {return *yytext;} 

[ ]+                           ; /* ignore whitespace */
 
.                              yyerror("Unknown character"); 

%%
