#include <string>
#include <sstream>
#include "targets/type_checker.h"
#include "targets/postfix_writer.h"

#include ".auto/all_nodes.h"  // all_nodes.h is automatically generated
#include "l22_parser.tab.h"

//---------------------------------------------------------------------------

void l22::postfix_writer::do_nil_node(cdk::nil_node * const node, int lvl) {
  // EMPTY
}

void l22::postfix_writer::do_data_node(cdk::data_node * const node, int lvl) {
  // EMPTY
}

void l22::postfix_writer::do_double_node(cdk::double_node * const node, int lvl) {
  if (_inFunctionBody ) {
    _pf.DOUBLE(node->value()); // load number to the stack
  } else {
    _pf.SDOUBLE(node->value());    // double is on the DATA segment
  }
}

void l22::postfix_writer::do_identity_node(l22::identity_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl + 2);
}

void l22::postfix_writer::do_not_node(cdk::not_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl + 2);
  _pf.INT(0);
  _pf.EQ();
}

void l22::postfix_writer::do_and_node(cdk::and_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.AND();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

void l22::postfix_writer::do_or_node(cdk::or_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  int lbl = ++_lbl;
  node->left()->accept(this, lvl + 2);
  _pf.DUP32();
  _pf.JNZ(mklbl(lbl));
  node->right()->accept(this, lvl + 2);
  _pf.OR();
  _pf.ALIGN();
  _pf.LABEL(mklbl(lbl));
}

void l22::postfix_writer::do_address_of_node(l22::address_of_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->lvalue()->accept(this, lvl + 2);
}
void l22::postfix_writer::do_nullptr_node(l22::nullptr_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  // a pointer is a 32-bit integer
  if (_inFunctionBody) {
    _pf.INT(0);
  } else {
    _pf.SINT(0);
  }
}
void l22::postfix_writer::do_index_node(l22::index_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->base()->accept(this, lvl);
  node->index()->accept(this, lvl);
  _pf.INT(node->type()->size());
  _pf.MUL();
  _pf.ADD(); 
}
void l22::postfix_writer::do_variable_declaration_node(l22::variable_declaration_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  auto id = node->identifier();
  if(node->qualifier() == tUSE || node->qualifier() == tFOREIGN) {
    _functions_to_declare.insert(id);
    return;
  }
  _functions_to_declare.erase(id); // redefinition of variable


  // type size?
  int offset = 0, typesize = node->type()->size(); // in bytes
  if (_inFunctionArgs) {
    offset = _offset;
    _offset += typesize;
  } else if (_inFunctionBody) {
    _offset -= typesize;
    offset = _offset;
  } else {
    offset = 0; // global variable
  }

  auto symbol = new_symbol();
  if (symbol) {
    symbol->set_offset(offset);
    reset_new_symbol();
  }

  if (_inFunctionBody) {
    // if we are dealing with local variables, then no action is needed
    // unless an initializer exists
    if (node->initializer()) {
      node->initializer()->accept(this, lvl);
      if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_STRING) || node->is_typed(cdk::TYPE_POINTER) || node->is_typed(cdk::TYPE_FUNCTIONAL)) {
        _pf.LOCAL(symbol->offset());
        _pf.STINT();
      } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
        if (node->initializer()->is_typed(cdk::TYPE_INT))
          _pf.I2D();
        _pf.LOCAL(symbol->offset());
        _pf.STDOUBLE();
      } else {
        std::cerr << "cannot initialize" << std::endl;
      }
    }
  } else {
    if (node->initializer() == nullptr) {
      _pf.BSS();
      _pf.ALIGN();
      _pf.LABEL(id);
      _pf.SALLOC(typesize);
    } else {

      if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_DOUBLE) || node->is_typed(cdk::TYPE_POINTER)|| node->is_typed(cdk::TYPE_FUNCTIONAL)) {
        _isFBodyVisited = false;
        if (node->is_typed(cdk::TYPE_FUNCTIONAL) && node->initializer()) {
          node->initializer()->accept(this, lvl);
        }
        if (node->constant()) {
          _pf.RODATA();
        } else {
          _pf.DATA();
        }
        _pf.ALIGN();
        if(node->qualifier() == tPUBLIC) {
          _pf.GLOBAL(node->identifier(), _pf.OBJ());
        }
        _pf.LABEL(id);

        if (node->is_typed(cdk::TYPE_INT)) {
          node->initializer()->accept(this, lvl);
        } else if (node->is_typed(cdk::TYPE_POINTER)) {
          node->initializer()->accept(this, lvl);
        } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
          if (node->initializer()->is_typed(cdk::TYPE_DOUBLE)) {
            node->initializer()->accept(this, lvl);
          } else if (node->initializer()->is_typed(cdk::TYPE_INT)) {
            cdk::integer_node *dclini = dynamic_cast<cdk::integer_node*>(node->initializer());
            cdk::double_node ddi(dclini->lineno(), dclini->value());
            ddi.accept(this, lvl);
          }
        } else if (node->is_typed(cdk::TYPE_FUNCTIONAL) && _isFBodyVisited) { // func visited
          _pf.SADDR(_bodyRetLabels.top());
        }
      } else if (node->is_typed(cdk::TYPE_STRING)) {
        if (node->constant()) {
          int litlbl;
          // HACK!!! string literal initializers must be emitted before the string identifier
          _pf.RODATA();
          _pf.ALIGN();
          _pf.LABEL(mklbl(litlbl = ++_lbl));
          _pf.SSTRING(dynamic_cast<cdk::string_node*>(node->initializer())->value());
          _pf.ALIGN();
          _pf.LABEL(id);
          _pf.SADDR(mklbl(litlbl));
        } else {
          _pf.DATA();
          _pf.ALIGN();
          _pf.LABEL(id);
          node->initializer()->accept(this, lvl);
        }
      }
    }
  }
}
void l22::postfix_writer::do_block_node(l22::block_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _symtab.push(); // for block-local vars
  if (node->declarations()) node->declarations()->accept(this, lvl + 2);
  if (node->instructions()) node->instructions()->accept(this, lvl + 2);
  _symtab.pop();
}
void l22::postfix_writer::do_return_node(l22::return_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  if (_inFunctionBody && _function) {
    // should not reach here without returning a value (if not void)
    if (_function->type()->name() != cdk::TYPE_VOID) {
      node->retval()->accept(this, lvl + 2);

      if (_function->type()->name() == cdk::TYPE_INT || _function->type()->name() == cdk::TYPE_STRING
          || _function->type()->name() == cdk::TYPE_POINTER || _function->type()->name() == cdk::TYPE_FUNCTIONAL) {
        _pf.STFVAL32();
      } else if (_function->type()->name() == cdk::TYPE_DOUBLE) {
        if (node->retval()->type()->name() == cdk::TYPE_INT) _pf.I2D();
        _pf.STFVAL64();
      } else {
        std::cerr << node->lineno() << ": should not happen: unknown return type" << std::endl;
      }
    }
  }
  if(_inFunctionBody > 1) {
    _pf.JMP(_bodyRetLabels.top());
    _bodyRetLabels.pop();
  }  
}


void l22::postfix_writer::do_sizeof_node(l22::sizeof_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  _pf.INT(node->expression()->type()->size());
}
void l22::postfix_writer::do_stack_alloc_node(l22::stack_alloc_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->argument()->accept(this, lvl);
  if(cdk::reference_type::cast(node->type())->referenced()->name() == cdk::TYPE_DOUBLE)
    _pf.INT(3);
  else
    _pf.INT(2);
  _pf.SHTL();
  _pf.ALLOC(); // allocate
  _pf.SP();// put base pointer in stack
}
void l22::postfix_writer::do_stop_node(l22::stop_node * const node, int lvl) {
  if (_whileIni.size() != 0) {
    _pf.JMP(mklbl(_whileEnd.top())); // jump to for end
  }
}
void l22::postfix_writer::do_again_node(l22::again_node * const node, int lvl) {
  if (_whileIni.size() != 0) {
    _pf.JMP(mklbl(_whileIni.top())); // jump to next cycle
  }
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_function_body_node(l22::function_body_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  _isFBodyVisited = true;
  // remember symbol so that args and body know
  _function = new_symbol();
  _functions_to_declare.erase(_function->name());  // just in case
  reset_new_symbol();

  _bodyRetLabels.push(mklbl(++_lbl));

  _offset = 8; // prepare for arguments (4: remember to account for return address)
  _symtab.push(); // scope of args


  if (node->arguments()->size() > 0) {
    _inFunctionArgs = true;
    for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
      cdk::basic_node *arg = node->arguments()->node(ix);
      if (arg == nullptr) break; // this means an empty sequence of arguments
      arg->accept(this, lvl); // the function symbol is at the top of the stack
    }
    _inFunctionArgs = false;
  }

  _pf.TEXT();
  _pf.ALIGN();
  _pf.LABEL(_bodyRetLabels.top());
  _pf.ENTER(128); // total stack size reserved for local variables

  _offset = 0; // prepare for local variable

  _inFunctionBody += 1;

  node->block()->accept(this, lvl + 4); // block has its own scope

  _inFunctionBody -= 1;

  _pf.LEAVE();
  _pf.RET();

  _symtab.pop(); // scope of arguments
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_function_call_node(l22::function_call_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  
  std::shared_ptr<cdk::functional_type> derived;
  std::vector < std::shared_ptr < cdk::basic_type >> argtypes;
  if (node->function())
    derived = cdk::functional_type::cast(node->function()->type());
  else {  // @ recursive call
    for (size_t ax = 0; ax < _function->number_of_arguments(); ax++) {
      argtypes.push_back(_function->argument_type(ax));
    }
  }
  
  size_t argsSize = 0;
  if (node->arguments()->size() > 0) {
    for (int ax = node->arguments()->size() - 1; ax >= 0; ax--) {
      cdk::expression_node *arg = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ax));
      arg->accept(this, lvl + 2);
      if (derived) {
        if (derived->input(ax)->name() == cdk::TYPE_DOUBLE && arg->is_typed(cdk::TYPE_INT)) {
          _pf.I2D();
        }
        argsSize += derived->input(ax)->size();
      }
      else {
        if (argtypes[ax]->name() == cdk::TYPE_DOUBLE && arg->is_typed(cdk::TYPE_INT)) {
          _pf.I2D();
        }
        argsSize += argtypes[ax]->size();
      }
    }
  }

  if (node->function()) {
    node->function()->accept(this, lvl);
    _pf.BRANCH();
  }
  else {  // @ recursive call
    _pf.CALL(_bodyRetLabels.top()); // ?
  }
  if (argsSize != 0) {
    _pf.TRASH(argsSize);
  }

  if (node->is_typed(cdk::TYPE_INT) || node->is_typed(cdk::TYPE_POINTER)
      || node->is_typed(cdk::TYPE_STRING) || node->is_typed(cdk::TYPE_FUNCTIONAL)) {
    _pf.LDFVAL32();
  } else if (node->is_typed(cdk::TYPE_DOUBLE)) {
    _pf.LDFVAL64();
  }

}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_sequence_node(cdk::sequence_node * const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_integer_node(cdk::integer_node * const node, int lvl) {
  if (_inFunctionBody) {
    _pf.INT(node->value()); // integer literal is on the stack: push an integer
  } else {
    _pf.SINT(node->value()); // integer literal is on the DATA segment
  }
  
}

void l22::postfix_writer::do_string_node(cdk::string_node * const node, int lvl) {
  int lbl1;

  /* generate the string */
  _pf.RODATA(); // strings are DATA readonly
  _pf.ALIGN(); // make sure we are aligned
  _pf.LABEL(mklbl(lbl1 = ++_lbl)); // give the string a name
  _pf.SSTRING(node->value()); // output string characters
  if (_inFunctionBody) {     //?
    // local variable initializer
    _pf.TEXT();
    _pf.ADDR(mklbl(lbl1));
  } else {
    // global variable initializer
    _pf.DATA();
    _pf.SADDR(mklbl(lbl1));
  }
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_neg_node(cdk::neg_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl); // determine the value
  _pf.NEG(); // 2-complement
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_add_node(cdk::add_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->left()->type()->name() == cdk::TYPE_INT) {
    _pf.INT(3);
    _pf.SHTL();
  }

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.INT(3);
    _pf.SHTL();
  }

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DADD();
  else
    _pf.ADD();
}
void l22::postfix_writer::do_sub_node(cdk::sub_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.I2D();
  } else if (node->type()->name() == cdk::TYPE_POINTER && node->right()->type()->name() == cdk::TYPE_INT) {
    _pf.INT(3);
    _pf.SHTL();
  }

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DSUB();
  else
    _pf.SUB();
}
void l22::postfix_writer::do_mul_node(cdk::mul_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DMUL();
  else
    _pf.MUL();
}
void l22::postfix_writer::do_div_node(cdk::div_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->left()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE && node->right()->type()->name() == cdk::TYPE_INT) _pf.I2D();

  if (node->type()->name() == cdk::TYPE_DOUBLE)
    _pf.DDIV();
  else
    _pf.DIV();
}
void l22::postfix_writer::do_mod_node(cdk::mod_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->left()->accept(this, lvl);
  node->right()->accept(this, lvl);
  _pf.MOD();
}
void l22::postfix_writer::do_lt_node(cdk::lt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  _pf.LT();
}
void l22::postfix_writer::do_le_node(cdk::le_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  _pf.LE();
}
void l22::postfix_writer::do_ge_node(cdk::ge_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  _pf.GE();
}
void l22::postfix_writer::do_gt_node(cdk::gt_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  _pf.GT();
}
void l22::postfix_writer::do_ne_node(cdk::ne_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  _pf.NE();
}
void l22::postfix_writer::do_eq_node(cdk::eq_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->left()->accept(this, lvl + 2);
  if (node->left()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  node->right()->accept(this, lvl + 2);
  if (node->right()->type()->name() == cdk::TYPE_INT && node->right()->type()->name() == cdk::TYPE_DOUBLE) _pf.I2D();

  _pf.EQ();
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_variable_node(cdk::variable_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  const std::string &id = node->name();
  auto symbol = _symtab.find(id);
  if (symbol->global()) {
    
    _pf.ADDR(symbol->name());
  } else {
    _pf.LOCAL(symbol->offset());
  }
}

void l22::postfix_writer::do_rvalue_node(cdk::rvalue_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->lvalue()->accept(this, lvl);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.LDDOUBLE();
  } else {
    // integers, pointers, and strings
    _pf.LDINT();
  }
}

void l22::postfix_writer::do_assignment_node(cdk::assignment_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  node->rvalue()->accept(this, lvl + 2);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    if (node->rvalue()->type()->name() == cdk::TYPE_INT) _pf.I2D();
    _pf.DUP64();
  } else {
    _pf.DUP32();
  }

  node->lvalue()->accept(this, lvl);
  if (node->type()->name() == cdk::TYPE_DOUBLE) {
    _pf.STDOUBLE();
  } else {
    _pf.STINT();
  }
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_program_node(l22::program_node * const node, int lvl) {
  // Note that Simple doesn't have functions. Thus, it doesn't need
  // a function node. However, it must start in the main function.
  // The ProgramNode (representing the whole program) doubles as a
  // main function node.

  // generate the main function (RTS mandates that its name be "_main")
  _pf.TEXT();
  _pf.ALIGN();
  _pf.GLOBAL("_main", _pf.FUNC());
  _pf.LABEL("_main");
  _pf.ENTER(120);  // Simple doesn't implement local variables

  _inFunctionBody += 1;
  node->statements()->accept(this, lvl);
  _inFunctionBody -= 1;

  // end the main function
  _pf.INT(0);
  _pf.STFVAL32();
  _pf.LEAVE();
  _pf.RET();

  // these are just a few library function imports
  for (std::string s : _functions_to_declare)
    _pf.EXTERN(s);
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_evaluation_node(l22::evaluation_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  node->argument()->accept(this, lvl);
  _pf.TRASH(node->argument()->type()->size());
}

void l22::postfix_writer::do_print_node(l22::print_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  
  for (size_t ix = 0; ix < node->arguments()->size(); ix++) {
    auto child = dynamic_cast<cdk::expression_node*>(node->arguments()->node(ix));

    std::shared_ptr<cdk::basic_type> etype = child->type();
    child->accept(this, lvl); // expression to print
    if (etype->name() == cdk::TYPE_INT) {
      _functions_to_declare.insert("printi");
      _pf.CALL("printi");
      _pf.TRASH(4); // trash int
    } else if (etype->name() == cdk::TYPE_DOUBLE) {
      _functions_to_declare.insert("printd");
      _pf.CALL("printd");
      _pf.TRASH(8); // trash double
    } else if (etype->name() == cdk::TYPE_STRING) {
      _functions_to_declare.insert("prints");
      _pf.CALL("prints");
      _pf.TRASH(4); // trash char pointer
    } else {
      std::cerr << "cannot print expression of unknown type" << std::endl;
      return;
    }

  }
  if (node->newline()) {
    _functions_to_declare.insert("println");
    _pf.CALL("println");
  }
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_read_node(l22::read_node * const node, int lvl) {
  // ASSERT_SAFE_EXPRESSIONS;
  if (_lvalueType == cdk::TYPE_DOUBLE) {
    _functions_to_declare.insert("readd");
    _pf.CALL("readd");
    _pf.LDFVAL64();
  } else if (_lvalueType == cdk::TYPE_INT) {
    _functions_to_declare.insert("readi");
    _pf.CALL("readi");
    _pf.LDFVAL32();
  } else {
    std::cerr << "FATAL: " << node->lineno() << ": cannot read type" << std::endl;
    return;
  }
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_while_node(l22::while_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;

  _whileIni.push(++_lbl); //start of body
  _whileEnd.push(++_lbl); //after cycle

  _symtab.push();

  _pf.LABEL(mklbl(_whileIni.top()));
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(_whileEnd.top()));
  node->block()->accept(this, lvl + 2);
  _pf.JMP(mklbl(_whileIni.top()));
  _pf.LABEL(mklbl(_whileEnd.top()));

  _symtab.pop();

  _whileIni.pop();
  _whileEnd.pop();
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_if_node(l22::if_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->block()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1));
}

//---------------------------------------------------------------------------

void l22::postfix_writer::do_if_else_node(l22::if_else_node * const node, int lvl) {
  ASSERT_SAFE_EXPRESSIONS;
  int lbl1, lbl2;
  node->condition()->accept(this, lvl);
  _pf.JZ(mklbl(lbl1 = ++_lbl));
  node->thenblock()->accept(this, lvl + 2);
  _pf.JMP(mklbl(lbl2 = ++_lbl));
  _pf.LABEL(mklbl(lbl1));
  node->elseblock()->accept(this, lvl + 2);
  _pf.LABEL(mklbl(lbl1 = lbl2));
}




std::string l22::postfix_writer::type_name(std::shared_ptr<cdk::basic_type> type) {
  if (!type) {return "null";}
  if (type->name() == cdk::TYPE_STRING) return "text";
  if (type->name() == cdk::TYPE_UNSPEC) return "var";
  if (type->name() == cdk::TYPE_POINTER) {
    std::shared_ptr<cdk::reference_type> derived = cdk::reference_type::cast(type);
    std::string ret = type_name(derived->referenced());
    if(ret == "[void]") {
      return ret;
    }
    return "[" + type_name(derived->referenced()) + "]";
  }
  if (type->name() == cdk::TYPE_FUNCTIONAL) {
    std::shared_ptr<cdk::functional_type> derived = cdk::functional_type::cast(type);
    std::string ret;
    ret += type_name(derived->output(0)) + "<";
  
    for(size_t i = 0; i < derived->input_length(); i++) {
      if ( i > 0) ret += ", "; 
      ret += type_name(derived->input(i));
    }
    return ret + ">";
  }  
  else return cdk::to_string(type);
}