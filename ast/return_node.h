#ifndef __L22_AST_RETURN_NODE_H__
#define __L22_AST_RETURN_NODE_H__


namespace l22 {

  /**
   * Class for describing the function's return node.
   */
  class return_node: public cdk::basic_node {
  cdk::expression_node * _retval;

  public:
    return_node(int lineno, cdk::expression_node * retval):
    cdk::basic_node(lineno), _retval(retval) {
  }

  public:
    cdk::expression_node *retval() {
      return _retval;
    }

  public:
    void accept(basic_ast_visitor *sp, int level) {
      sp->do_return_node(this, level);
    }

  };

} // l22

#endif