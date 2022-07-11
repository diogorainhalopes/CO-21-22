#ifndef __L22_AST_FUNCTION_CALL_H__
#define __L22_AST_FUNCTION_CALL_H__

#include <string>
#include <cdk/ast/basic_node.h>
#include <cdk/ast/sequence_node.h>
#include <cdk/ast/nil_node.h>
#include <cdk/ast/expression_node.h>

namespace l22 {

  class function_call_node: public cdk::expression_node {
    cdk::sequence_node *_arguments;
    cdk::expression_node *_function;

  public:

    function_call_node(int lineno) :
        cdk::expression_node(lineno), _arguments(new cdk::sequence_node(lineno)), _function(nullptr) {
    }

    function_call_node(int lineno, cdk::sequence_node *arguments) :
        cdk::expression_node(lineno), _arguments(arguments), _function(nullptr) {
    }

    function_call_node(int lineno, cdk::expression_node *function) :
        cdk::expression_node(lineno), _arguments(new cdk::sequence_node(lineno)), _function(function) {
    }

    function_call_node(int lineno, cdk::sequence_node *arguments, cdk::expression_node *function) :
        cdk::expression_node(lineno), _arguments(arguments), _function(function) {
    }

  public:
    cdk::sequence_node* arguments() {
      return _arguments;
    }
    cdk::typed_node *argument(size_t ix) {
      return dynamic_cast<cdk::typed_node*>(_arguments->node(ix));
    }

    cdk::expression_node *function() {
      return _function;
    }

    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_call_node(this, level);
    }

  };

} // l22

#endif