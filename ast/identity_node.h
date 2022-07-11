#ifndef __L22_IDENTITY_NODE_H__
#define __L22_IDENTITY_NODE_H__

#include <cdk/ast/unary_operation_node.h>
#include <cdk/ast/expression_node.h>

namespace l22 {

    /**
     * Class for describing the identity operator.
     */
    class identity_node: public cdk::unary_operation_node {

    public:
      identity_node(int lineno, cdk::expression_node *argument) :
        unary_operation_node(lineno, argument) {
      }

    public:
      void accept(basic_ast_visitor *sp, int level) {
        sp->do_identity_node(this, level);
      }

    };

} // l22

#endif