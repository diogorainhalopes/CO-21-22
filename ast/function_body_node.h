#ifndef __L22_AST_FUNCTION_DEFINITION_H__
#define __L22_AST_FUNCTION_DEFINITION_H__

#include <string>
#include <cdk/ast/typed_node.h>
#include <cdk/ast/sequence_node.h>
#include "ast/block_node.h"
#include <cdk/types/basic_type.h>

namespace l22 {


  class function_body_node: public cdk::expression_node {
    cdk::sequence_node *_arguments;
    l22::block_node *_block;

  public:
    function_body_node(int lineno, l22::block_node *block, std::shared_ptr<cdk::basic_type> type) :
        cdk::expression_node(lineno), _arguments(new cdk::sequence_node(lineno)), _block(block) {
          this->type(type);
    }

    function_body_node(int lineno, cdk::sequence_node *arguments, l22::block_node *block, std::shared_ptr<cdk::basic_type> type) :
        cdk::expression_node(lineno), _arguments(arguments), _block(block) {
          this->type(type);
    }
    
    function_body_node(int lineno) :
        cdk::expression_node(lineno), _arguments(new cdk::sequence_node(lineno)), _block(new l22::block_node(lineno)) {
    }

  public:
    inline cdk::sequence_node* arguments() {
      return _arguments;
    }
    
    inline cdk::typed_node *argument(size_t ix) {
      return dynamic_cast<cdk::typed_node*>(_arguments->node(ix));
    }

    inline l22::block_node* block() {
      return _block;
    }


    void accept(basic_ast_visitor *sp, int level) {
      sp->do_function_body_node(this, level);
    }

  };

} // l22

#endif