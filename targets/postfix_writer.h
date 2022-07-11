#ifndef __L22_TARGETS_POSTFIX_WRITER_H__
#define __L22_TARGETS_POSTFIX_WRITER_H__

#include "targets/basic_ast_visitor.h"
#include <set>
#include <sstream>
#include <stack>
#include <cdk/emitters/basic_postfix_emitter.h>

namespace l22 {

  //!
  //! Traverse syntax tree and generate the corresponding assembly code.
  //!
  class postfix_writer: public basic_ast_visitor {
    cdk::symbol_table<l22::symbol> &_symtab;
    cdk::basic_postfix_emitter &_pf;
    int _lbl;
    int _offset;
    int _inFunctionBody;
    int _inFunctionArgs;
    bool _isFBodyVisited;
    cdk::typename_type _lvalueType;
    std::stack<int> _whileIni, _whileEnd; // while instruction break/repeat

    std::shared_ptr<l22::symbol> _function;
    std::set<std::string> _functions_to_declare; // print_node no postfix

    //std::string _currentBodyRetLabel; // where to jump when a return occurs of an exclusive section ends
    std::stack<std::string> _bodyRetLabels; // where to jump when a return occurs of an exclusive section ends

  public:
    postfix_writer(std::shared_ptr<cdk::compiler> compiler, cdk::symbol_table<l22::symbol> &symtab,
                   cdk::basic_postfix_emitter &pf) :
        basic_ast_visitor(compiler), _symtab(symtab), _pf(pf), _lbl(0), _offset(0), 
        _inFunctionBody(0), _inFunctionArgs(0), _isFBodyVisited(false), _lvalueType(
        cdk::TYPE_VOID), _function(nullptr) {
    }

  public:
    ~postfix_writer() {
      os().flush();
    }

  private:
    std::string type_name(std::shared_ptr<cdk::basic_type> type); // remover depois
    /** Method used to generate sequential labels. */
    inline std::string mklbl(int lbl) {
      std::ostringstream oss;
      if (lbl < 0)
        oss << ".L" << -lbl;
      else
        oss << "_L" << lbl;
      return oss.str();
    }

  public:
  // do not edit these lines
#define __IN_VISITOR_HEADER__
#include ".auto/visitor_decls.h"       // automatically generated
#undef __IN_VISITOR_HEADER__
  // do not edit these lines: end

  };

} // l22

#endif
