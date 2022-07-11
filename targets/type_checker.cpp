#include <string>
#include <iostream>
#include "targets/type_checker.h"
#include ".auto/all_nodes.h"  // automatically generated
#include <cdk/types/primitive_type.h>
#include "l22_parser.tab.h"

#define ASSERT_UNSPEC { if (node->type() != nullptr && !node->is_typed(cdk::TYPE_UNSPEC)) return; }

int _in_fbody = 0;
int _fbody_number = 0;
//---------------------------------------------------------------------------

void l22::type_checker::do_sequence_node(cdk::sequence_node *const node, int lvl) {
  for (size_t i = 0; i < node->size(); i++) {
    node->node(i)->accept(this, lvl);
  }
}

//---------------------------------------------------------------------------

void l22::type_checker::do_nil_node(cdk::nil_node *const node, int lvl) {
  // EMPTY
}
void l22::type_checker::do_data_node(cdk::data_node *const node, int lvl) {
  // EMPTY
}
void l22::type_checker::do_block_node(l22::block_node *const node, int lvl) {
  // EMPTY
}
void l22::type_checker::do_stop_node(l22::stop_node *const node, int lvl) {
  // EMPTY
}
void l22::type_checker::do_again_node(l22::again_node *const node, int lvl) {
  // EMPTY
}

//---------------------------------------------------------------------------


void l22::type_checker::do_double_node(cdk::double_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void l22::type_checker::do_not_node(cdk::not_node *const node, int lvl) {
   ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (node->argument()->is_typed(cdk::TYPE_INT)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else if (node->argument()->is_typed(cdk::TYPE_UNSPEC)) {
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
    node->argument()->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  } else {
    throw std::string("wrong type in unary logical expression");
  }
}

void l22::type_checker::do_address_of_node(l22::address_of_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->lvalue()->accept(this, lvl + 2);
  node->type(cdk::reference_type::create(4, node->lvalue()->type()));
}
void l22::type_checker::do_nullptr_node(l22::nullptr_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::reference_type::create(4, nullptr));
}
void l22::type_checker::do_index_node(l22::index_node *const node, int lvl) {
  ASSERT_UNSPEC;
  std::shared_ptr < cdk::reference_type > btype;

  node->base()->accept(this, lvl + 2);
  btype = cdk::reference_type::cast(node->base()->type());
  if (!node->base()->is_typed(cdk::TYPE_POINTER)) throw std::string("pointer expression expected in index left-value");

  node->index()->accept(this, lvl + 2);
  if (!node->index()->is_typed(cdk::TYPE_INT)) throw std::string("integer expression expected in left-value index");

  node->type(btype->referenced());
}
//---------------------------------------------------------------------------

void l22::type_checker::do_variable_declaration_node(l22::variable_declaration_node *const node, int lvl) {

  std::shared_ptr<l22::symbol> previous;
  bool foreign = false;

  if(!node->type()) {
    previous = _symtab.find(node->identifier());
  
  } else if(type_name(node->type()) == "[void]") {
    node->type(cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_VOID)));
  }
  
  if (node->qualifier() != tPRIVATE) {
    if(_in_fbody) {
      throw std::string("in function not private '" + node->identifier() + "'");
    }
    if(node->qualifier() == tUSE || node->qualifier() == tFOREIGN) {
      if (!node->type())
        throw std::string("foreign/use variable '" + node->identifier() + "' without type");
      foreign = true;
    }
  }

  if (node->initializer()) {
    
    int current_in_fbody = _in_fbody; // Is a function body visited?
    node->initializer()->accept(this, lvl + 2);
    if (_in_fbody > current_in_fbody) {current_in_fbody = 1;}
    else {current_in_fbody = 0;}

    std::shared_ptr<cdk::basic_type> type;
    if(!node->type()) {
      if(previous) {
        type = previous->type();
        node->type(type);
      }
      else {
        type = cdk::primitive_type::create(0, cdk::TYPE_UNSPEC);
      }
    }
    else if(node->type()) {
      type = node->type();
    }

    if (type->name() == cdk::TYPE_INT) {
      if (!node->initializer()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type for initializer (integer expected).");
    } else if (type->name() == cdk::TYPE_DOUBLE) {
      if (!node->initializer()->is_typed(cdk::TYPE_INT) && !node->initializer()->is_typed(cdk::TYPE_DOUBLE)) {
        throw std::string("wrong type for initializer (integer or double expected).");
      }
    } else if (type->name() == cdk::TYPE_STRING) {
      if (!node->initializer()->is_typed(cdk::TYPE_STRING)) {
        throw std::string("wrong type for initializer (string expected).");
      }
    } else if (type->name() == cdk::TYPE_POINTER) {
      if (type_name(node->initializer()->type()) != type_name(type)
        && type_name(node->initializer()->type()) != "[null]"
        && type_name(node->initializer()->type()) != "[var]") {
        throw std::string("wrong type for initializer (" + type_name(type) + "pointer expected).");
      }
      if (type_name(node->initializer()->type()) == "[var]") node->initializer()->type(type);

    } else if (type->name() == cdk::TYPE_FUNCTIONAL && !current_in_fbody) {
      if (node->initializer()->type()->name() != cdk::TYPE_FUNCTIONAL) {
        throw std::string("initializer not a function body.");
      }

      if (type_name(node->initializer()->type()) != type_name(type)) {
        std::shared_ptr<cdk::functional_type> derived_init = cdk::functional_type::cast(node->initializer()->type());
        std::shared_ptr<cdk::functional_type> derived = cdk::functional_type::cast(type);

        if (derived_init->input_length() != derived->input_length()) {
          throw std::string("wrong number of input arguments.");
        }
        if (type_name(derived_init->output(0)) != type_name(derived->output(0)) &&
              derived_init->output(0)->name() != cdk::TYPE_INT && derived->output(0)->name() != cdk::TYPE_DOUBLE) {
          throw std::string("wrong type for initializer output (" + type_name(derived->output(0)) + " expected).");
        }
        for(size_t i = 0; i < derived->input_length(); i++) {
          if (type_name(derived_init->input(i)) != type_name(derived->input(i)) &&
              derived_init->input(i)->name() != cdk::TYPE_INT && derived->input(i)->name() != cdk::TYPE_DOUBLE) {
            throw std::string("wrong type for initializer input (" + type_name(derived->input(i)) + " expected).");
          }
        }
      }
    } else if (type->name() == cdk::TYPE_FUNCTIONAL && current_in_fbody) {
        _fbody_number -= 1;
        _in_fbody -= 1;
        std::shared_ptr<cdk::functional_type> derived = cdk::functional_type::cast(type);

        std::shared_ptr<l22::symbol> fbody = _symtab.find("_fbody" + std::to_string(_fbody_number));
        if(fbody) {
          if (fbody->number_of_arguments() != derived->input_length()) {
            throw std::string("wrong number of input arguments.");
          }
          if(type_name(fbody->type()) != type_name(derived->output(0))) {        
            if (!node->initializer()->is_typed(cdk::TYPE_INT) && (type->name() != cdk::TYPE_DOUBLE)) {
              throw std::string("wrong type for initializer output (" + type_name(derived->output(0)) + " expected).");
            }
          }
          for(size_t i = 0; i < derived->input_length(); i++) {
            if(type_name(fbody->argument_type(i)) != type_name(derived->input(i))) {
              if (fbody->argument_is_typed(i, cdk::TYPE_INT) && derived->input(i)->name() == cdk::TYPE_DOUBLE){
                continue;
              }
              throw std::string("wrong type for initializer input (" + type_name(derived->input(i)) + " expected).");
            }
          }
        }
    } else if (type->name() == cdk::TYPE_UNSPEC){ // var
      if (current_in_fbody){ // Type can be a function
        _fbody_number -= 1;
        _in_fbody -= 1;
        std::vector<std::shared_ptr<cdk::basic_type>> inputs;
        
        std::shared_ptr<l22::symbol> fbody = _symtab.find("_fbody" + std::to_string(_fbody_number));
        if(fbody) {
          for(size_t i = 0; i < fbody->number_of_arguments(); i++) {
            inputs.push_back(fbody->argument_type(i));
          }
        }

        node->type(cdk::functional_type::create(inputs, node->initializer()->type()));
        
      } else {
        node->type(node->initializer()->type());
      }
    } else {
      throw std::string("unknown type for initializer.");
    }
  }

  const std::string &id = node->identifier();
  auto symbol = l22::make_symbol(node->qualifier(), node->type(), id, (bool)node->initializer(), foreign);
  if (_symtab.insert(id, symbol)) {
    _parent->set_new_symbol(symbol);  // advise parent that a symbol has been inserted
  } else {
    if(previous) {
      int qualifier;
      if (node->qualifier() != tPRIVATE) qualifier = node->qualifier();
      else qualifier = previous->qualifier();
      auto symbol = l22::make_symbol(qualifier, previous->type(), id, (bool)node->initializer(), foreign);
      _symtab.replace(id, symbol);
      _parent->set_new_symbol(symbol);
    }
    else {
      if (!node->initializer())
        throw std::string("variable '" + id + "' redeclared");
      else {
        _symtab.replace(id, symbol);
      }
    }
  }
}
//---------------------------------------------------------------------------

void l22::type_checker::do_return_node(l22::return_node *const node, int lvl) {
  if(_in_fbody) {
    _in_fbody -= 1;
     
  
    std::shared_ptr<l22::symbol> fbody= _symtab.find("_fbody" + std::to_string(_fbody_number - 1));
    if(node->retval() && fbody->type()->name() == cdk::TYPE_VOID) {
      throw std::string("type mismatch for return (no return value expected).");
    }
    else if (!node->retval() && fbody->type()->name() != cdk::TYPE_VOID) {
      throw std::string("type mismatch for return (return value expected).");
    }
    else if (!node->retval()) {
      return;
    }

    int current_in_fbody = _in_fbody; // Is a function body visited?
    node->retval()->accept(this, lvl + 4);
    
    if (_in_fbody > current_in_fbody) {
      current_in_fbody = 1;
    }
    else {
      current_in_fbody = 0;
    }

    std::shared_ptr<cdk::basic_type> type = fbody->type();

    if (type->name() == cdk::TYPE_INT) {
      if (!node->retval()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type for initializer (integer expected).");
    } else if (type->name() == cdk::TYPE_DOUBLE) {
      if (!node->retval()->is_typed(cdk::TYPE_INT) && !node->retval()->is_typed(cdk::TYPE_DOUBLE)) {
        throw std::string("wrong type for retval (integer or double expected).");
      }
    } else if (type->name() == cdk::TYPE_STRING) {
      if (!node->retval()->is_typed(cdk::TYPE_STRING)) {
        throw std::string("wrong type for retval (string expected).");
      }
    } else if (type->name() == cdk::TYPE_POINTER) {
      if (type_name(node->retval()->type()) != type_name(type)) {
        throw std::string("wrong type for retval (" + type_name(type) + "pointer expected).");
      }

    } else if (type->name() == cdk::TYPE_FUNCTIONAL && !current_in_fbody) {
      if (node->retval()->type()->name() != cdk::TYPE_FUNCTIONAL) {
        throw std::string("retval not a function body.");
      }

      if (type_name(node->retval()->type()) != type_name(type)) {
        std::shared_ptr<cdk::functional_type> derived_init = cdk::functional_type::cast(node->retval()->type());
        std::shared_ptr<cdk::functional_type> derived = cdk::functional_type::cast(type);

        if (derived_init->input_length() != derived->input_length()) {
          throw std::string("wrong number of input arguments.");
        }
        if (type_name(derived_init->output(0)) != type_name(derived->output(0)) &&
              derived_init->output(0)->name() != cdk::TYPE_INT && derived->output(0)->name() != cdk::TYPE_DOUBLE) {
          throw std::string("wrong type for retval output (" + type_name(derived->output(0)) + " expected).");
        }
        for(size_t i = 0; i < derived->input_length(); i++) {
          if (type_name(derived_init->input(i)) != type_name(derived->input(i)) &&
              derived_init->input(i)->name() != cdk::TYPE_INT && derived->input(i)->name() != cdk::TYPE_DOUBLE) {
            throw std::string("wrong type for retval input (" + type_name(derived->input(i)) + " expected).");
          }
        }
      }
    } 
    else if (type->name() == cdk::TYPE_FUNCTIONAL && current_in_fbody) {
      _fbody_number -= 1;
      _in_fbody -= 1;
      std::shared_ptr<cdk::functional_type> derived = cdk::functional_type::cast(type);

      if(fbody) {
        if (fbody->number_of_arguments() != derived->input_length()) {
          throw std::string("wrong number of input arguments.");
        }
        if(type_name(fbody->type()) != type_name(derived->output(0))) {        
          if (!node->retval()->is_typed(cdk::TYPE_INT) && (type->name() != cdk::TYPE_DOUBLE)) {
            throw std::string("wrong type for retval output (" + type_name(derived->output(0)) + " expected).");
          }
        }
        for(size_t i = 0; i < derived->input_length(); i++) {
          if(type_name(fbody->argument_type(i)) != type_name(derived->input(i))) {
            if (fbody->argument_is_typed(i, cdk::TYPE_INT) && derived->input(i)->name() == cdk::TYPE_DOUBLE){
              continue;
            }
            throw std::string("wrong type for retval input (" + type_name(derived->input(i)) + " expected).");
          }
        }
      }
    } 
  }
  else {
    if(node->retval()) {
      node->retval()->accept(this, lvl + 4);
      if(!node->retval()->is_typed(cdk::TYPE_INT)) {
        throw std::string("type mismatch for return.");
      }
    }
    else {
      throw std::string("type mismatch for return, expected return.");
    }
  }
  _returnSeen = true;
}

void l22::type_checker::do_sizeof_node(l22::sizeof_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->expression()->accept(this, lvl + 2);
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}
//---------------------------------------------------------------------------

void l22::type_checker::do_stack_alloc_node(l22::stack_alloc_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT)) {
    throw std::string("integer expression expected in allocation expression");
  }
  auto mytype = cdk::reference_type::create(4, cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
  node->type(mytype);
}

//---------------------------------------------------------------------------

void l22::type_checker::do_function_body_node(l22::function_body_node *const node, int lvl) {
  std::string id = "_fbody" + std::to_string(_fbody_number);
  _fbody_number += 1;

  std::cout << "id: " << id << "; " << type_name(node->type()) << std::endl;
  // remember symbol so that args know
  auto function = l22::make_symbol(0, node->type(), id, 0, false);
  std::vector < std::shared_ptr < cdk::basic_type >> argtypes;
  for (size_t ax = 0; ax < node->arguments()->size(); ax++) {
     if (node->argument(ax))  {
      argtypes.push_back(node->argument(ax)->type());
     }
     
  }
  _in_fbody += 1;
  function->set_argument_types(argtypes);
  if (!_symtab.insert(function->name(), function)) _symtab.replace(function->name(), function);
  _parent->set_new_symbol(function);
}
//---------------------------------------------------------------------------


void l22::type_checker::do_function_call_node(l22::function_call_node *const node, int lvl) {
  ASSERT_UNSPEC;
  std::shared_ptr<l22::symbol> fbody;
  std::shared_ptr<cdk::functional_type> derived;
  int current_in_fbody = 0;
  if(node->function()) {
    _returnSeen = false;
    current_in_fbody = _in_fbody; // Is a function body visited?
    node->function()->accept(this, lvl + 4);
    
    if (_in_fbody > current_in_fbody) {current_in_fbody = 1;}
    if(_returnSeen) { _in_fbody -= 1; }
  }
  if(!node->function() || current_in_fbody) { // @ recursive call
    _in_fbody -= 1;
    _fbody_number -= (_in_fbody+1);
    fbody = _symtab.find("_fbody" + std::to_string(_fbody_number));
    _fbody_number += _in_fbody;
    
  }
  else {
    if (!node->function()->is_typed(cdk::TYPE_FUNCTIONAL)) {
      throw std::string("type mismatch for argument, call variable not a function.");
    }
    derived = cdk::functional_type::cast(node->function()->type());

  }

  if (node->arguments()) {
    node->arguments()->accept(this, lvl + 4);
    
    if(fbody || derived ) {
      std::vector < std::shared_ptr < cdk::basic_type >> argtypes;
      size_t input_args;
      if(fbody) {
        input_args = fbody->number_of_arguments();
      }
      else{
        input_args = derived->input_length();
      }

      if (input_args != node->arguments()->size()) {
        throw std::string("wrong number of input arguments.");
      }
      for(size_t i = 0; i < node->arguments()->size(); i++) {
        argtypes.push_back(node->argument(i)->type());
        auto child = dynamic_cast<cdk::expression_node*>(node->arguments()->node(i));
        std::shared_ptr<cdk::basic_type> etype = child->type();
        std::shared_ptr<cdk::basic_type> type;
        if(fbody) {
          type = fbody->argument_type(i);
        }
        else{
          type = derived->input(i);
        }
        if(type_name(etype) != type_name(type)) {
          if (etype->name() == cdk::TYPE_INT && type->name() == cdk::TYPE_DOUBLE){
            continue;
          }
          throw std::string("wrong type for fcall input (" + type_name(type) + " expected).");
        }
      }
    }
    else {
       throw std::string( "invalid function call.");
    }
  } 
  else {
    if(fbody && fbody->number_of_arguments() != 0) {
      throw std::string(
          "number of arguments in call (" + std::to_string(node->arguments()->size()) + ") must match declaration ("
              + std::to_string(fbody->number_of_arguments()) + ").");
    }
    else if(derived && derived->input_length() != 0) {
      throw std::string(
          "number of arguments in call (" + std::to_string(node->arguments()->size()) + ") must match declaration ("
              + std::to_string(derived->input_length()) + ").");
    }
  }
  if(fbody) { // Func call type = output
    node->type(fbody->type());
  }
  else if (derived){
    node->type(derived->output(0));
  }
  else {throw std::string( "invalid function call.");}
}

//---------------------------------------------------------------------------

void l22::type_checker::do_integer_node(cdk::integer_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void l22::type_checker::do_string_node(cdk::string_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->type(cdk::primitive_type::create(4, cdk::TYPE_STRING));
}

//---------------------------------------------------------------------------

void l22::type_checker::processUnaryExpression(cdk::unary_operation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
  if (!node->argument()->is_typed(cdk::TYPE_INT) && !node->argument()->is_typed(cdk::TYPE_DOUBLE))
    throw std::string("wrong type in argument of unary expression");

  if(node->argument()->is_typed(cdk::TYPE_INT))
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  else
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void l22::type_checker::do_neg_node(cdk::neg_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

void l22::type_checker::do_identity_node(l22::identity_node *const node, int lvl) {
  processUnaryExpression(node, lvl);
}

//---------------------------------------------------------------------------

void l22::type_checker::processBinaryExpressionI(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in right argument of binary expression");

  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}

void l22::type_checker::processBinaryExpressionID(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);
  if (!node->left()->is_typed(cdk::TYPE_DOUBLE) && !node->left()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_DOUBLE) && !node->right()->is_typed(cdk::TYPE_INT))
    throw std::string("wrong type in right argument of binary expression");

  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT))
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  else
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
}

void l22::type_checker::processBinaryExpressionIDP(cdk::binary_operation_node *const node, int lvl) {
  ASSERT_UNSPEC;
  node->left()->accept(this, lvl + 2);

  if (!node->left()->is_typed(cdk::TYPE_DOUBLE) && !node->left()->is_typed(cdk::TYPE_INT) && !node->left()->is_typed(cdk::TYPE_POINTER))
    throw std::string("wrong type in left argument of binary expression");

  node->right()->accept(this, lvl + 2);
  if (!node->right()->is_typed(cdk::TYPE_DOUBLE) && !node->right()->is_typed(cdk::TYPE_INT) && !node->right()->is_typed(cdk::TYPE_POINTER))
    throw std::string("wrong type in right argument of binary expression");

  if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_INT))
    node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
  else if ((node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_INT))
      || (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_DOUBLE))
      || (node->left()->is_typed(cdk::TYPE_DOUBLE) && node->right()->is_typed(cdk::TYPE_DOUBLE)))
    node->type(cdk::primitive_type::create(8, cdk::TYPE_DOUBLE));
  else
    node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

void l22::type_checker::do_add_node(cdk::add_node *const node, int lvl) {
  processBinaryExpressionIDP(node, lvl);

  if (node->is_typed(cdk::TYPE_UNSPEC)) {
    if ((node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_INT)))
      node->type(node->left()->type());
    else if (node->left()->is_typed(cdk::TYPE_INT) && node->right()->is_typed(cdk::TYPE_POINTER))
      node->type(node->right()->type());
    else
      throw std::string("wrong types in binary expression");
  }
}
void l22::type_checker::do_sub_node(cdk::sub_node *const node, int lvl) {
  processBinaryExpressionIDP(node, lvl);

  if (node->is_typed(cdk::TYPE_UNSPEC)) {
    if ((node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) 
      && (type_name(node->left()->type()) == type_name(node->right()->type())))
      node->type(node->left()->type());
    else
      throw std::string("wrong types in binary expression");
  }
}
void l22::type_checker::do_mul_node(cdk::mul_node *const node, int lvl) {
  processBinaryExpressionID(node, lvl);
}
void l22::type_checker::do_div_node(cdk::div_node *const node, int lvl) {
  processBinaryExpressionID(node, lvl);
}
void l22::type_checker::do_mod_node(cdk::mod_node *const node, int lvl) {
  processBinaryExpressionI(node, lvl);
}
void l22::type_checker::do_lt_node(cdk::lt_node *const node, int lvl) {
  processBinaryExpressionID(node, lvl);
}
void l22::type_checker::do_le_node(cdk::le_node *const node, int lvl) {
  processBinaryExpressionID(node, lvl);
}
void l22::type_checker::do_ge_node(cdk::ge_node *const node, int lvl) {
  processBinaryExpressionID(node, lvl);
}
void l22::type_checker::do_gt_node(cdk::gt_node *const node, int lvl) {
  processBinaryExpressionID(node, lvl);
}
void l22::type_checker::do_ne_node(cdk::ne_node *const node, int lvl) {
  processBinaryExpressionIDP(node, lvl);

  if (node->is_typed(cdk::TYPE_UNSPEC)) {
    if ((node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) 
      && (type_name(node->left()->type()) == type_name(node->right()->type()))) {
        // vazio
      }
    else
      throw std::string("wrong types in binary expression");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}
void l22::type_checker::do_eq_node(cdk::eq_node *const node, int lvl) {
  processBinaryExpressionIDP(node, lvl);

  if (node->is_typed(cdk::TYPE_UNSPEC)) {
    if ((node->left()->is_typed(cdk::TYPE_POINTER) && node->right()->is_typed(cdk::TYPE_POINTER)) 
      && (type_name(node->left()->type()) == type_name(node->right()->type()))) {
        // vazio
      }
    else
      throw std::string("wrong types in binary expression");
  }
  node->type(cdk::primitive_type::create(4, cdk::TYPE_INT));
}
void l22::type_checker::do_and_node(cdk::and_node *const node, int lvl) {
  processBinaryExpressionI(node, lvl);
}
void l22::type_checker::do_or_node(cdk::or_node *const node, int lvl) {
  processBinaryExpressionI(node, lvl);
}

//---------------------------------------------------------------------------

void l22::type_checker::do_variable_node(cdk::variable_node *const node, int lvl) {
  ASSERT_UNSPEC;
  const std::string &id = node->name();
  std::shared_ptr<l22::symbol> symbol = _symtab.find(id);

  if (symbol != nullptr) {
    node->type(symbol->type());
  } else {
    throw "undeclared variable '" + id + "'";
  }
}

void l22::type_checker::do_rvalue_node(cdk::rvalue_node *const node, int lvl) {
  ASSERT_UNSPEC;
  try {
    node->lvalue()->accept(this, lvl);
    node->type(node->lvalue()->type());
  } catch (const std::string &id) {
    throw "undeclared variable '" + id + "'";
  }
}

void l22::type_checker::do_assignment_node(cdk::assignment_node *const node, int lvl) {
  ASSERT_UNSPEC;

  /* try {
    node->lvalue()->accept(this, lvl);
  } catch (const std::string &id) {
    auto symbol = std::make_shared<l22::symbol>(0, cdk::primitive_type::create(4, cdk::TYPE_INT), id, 0, 0);
    _symtab.insert(id, symbol);
    _parent->set_new_symbol(symbol);  // advise parent that a symbol has been inserted
    node->lvalue()->accept(this, lvl);  //DAVID: bah!
  } */

  node->lvalue()->accept(this, lvl);

  int current_in_fbody = _in_fbody; // Is a function body visited?
  node->rvalue()->accept(this, lvl + 2);
  if (_in_fbody > current_in_fbody) {current_in_fbody = 1;}
  else {current_in_fbody = 0;}

  if (node->lvalue()->type()->name() == cdk::TYPE_INT) {
    if (!node->rvalue()->is_typed(cdk::TYPE_INT)) throw std::string("wrong type for assignment (integer expected).");
  } else if (node->lvalue()->type()->name() == cdk::TYPE_DOUBLE) {
    if (!node->rvalue()->is_typed(cdk::TYPE_INT) && !node->rvalue()->is_typed(cdk::TYPE_DOUBLE)) {
      throw std::string("wrong type for assignment (integer or double expected).");
    }
  } else if (node->lvalue()->type()->name() == cdk::TYPE_STRING) {
    if (!node->rvalue()->is_typed(cdk::TYPE_STRING)) {
      throw std::string("wrong type for assignment (string expected).");
    }
  } else if (node->lvalue()->type()->name() == cdk::TYPE_POINTER) {
    if (type_name(node->rvalue()->type()) != type_name(node->lvalue()->type())
       && type_name(node->rvalue()->type()) != "[null]"
       && type_name(node->rvalue()->type()) != "[var]") {
      throw std::string("wrong type for assignment (" + type_name(node->lvalue()->type()) + "pointer expected).");
    }
    if (type_name(node->rvalue()->type()) == "[var]") node->rvalue()->type(node->rvalue()->type());

  } else if (node->lvalue()->type()->name() == cdk::TYPE_FUNCTIONAL && !current_in_fbody) {
    if (node->rvalue()->type()->name() != cdk::TYPE_FUNCTIONAL) {
        throw std::string("right assignment not a function body.");
    }
    if (type_name(node->rvalue()->type()) != type_name(node->lvalue()->type())) {
      std::shared_ptr<cdk::functional_type> derived_right = cdk::functional_type::cast(node->rvalue()->type());
      std::shared_ptr<cdk::functional_type> derived_left = cdk::functional_type::cast(node->lvalue()->type());

      if (derived_right->input_length() != derived_left->input_length()) {
        throw std::string("wrong number of input arguments.");
      }
      if (type_name(derived_right->output(0)) != type_name(derived_left->output(0)) &&
            derived_right->output(0)->name() != cdk::TYPE_INT && derived_left->output(0)->name() != cdk::TYPE_DOUBLE) {
        throw std::string("wrong type for assignment output (" + type_name(derived_left->output(0)) + " expected).");
      }
      for(size_t i = 0; i < derived_left->input_length(); i++) {
        if (type_name(derived_right->input(i)) != type_name(derived_left->input(i)) &&
            derived_right->input(i)->name() != cdk::TYPE_INT && derived_left->input(i)->name() != cdk::TYPE_DOUBLE) {
          throw std::string("wrong type for assignment input (" + type_name(derived_left->input(i)) + " expected).");
        }
      }      
    }
  } else if (node->lvalue()->type()->name() == cdk::TYPE_FUNCTIONAL && current_in_fbody) {
    _fbody_number -= 1;
    _in_fbody -= 1;
    std::shared_ptr<cdk::functional_type> derived_left = cdk::functional_type::cast(node->lvalue()->type());

    std::shared_ptr<l22::symbol> fbody = _symtab.find("_fbody" + std::to_string(_fbody_number));
    if(fbody) {
      if (fbody->number_of_arguments() != derived_left->input_length()) {
        throw std::string("wrong number of input arguments.");
      }
      if(type_name(fbody->type()) != type_name(derived_left->output(0))) {        
        if (!node->rvalue()->is_typed(cdk::TYPE_INT) && (node->lvalue()->type()->name() != cdk::TYPE_DOUBLE)) {
          throw std::string("wrong type for assignment output (" + type_name(derived_left->output(0)) + " expected).");
        }
      }
      for(size_t i = 0; i < derived_left->input_length(); i++) {
        if(type_name(fbody->argument_type(i)) != type_name(derived_left->input(i))) {
          if (fbody->argument_is_typed(i, cdk::TYPE_INT) && derived_left->input(i)->name() == cdk::TYPE_DOUBLE){
            continue;
          }
          throw std::string("wrong type for assignment input (" + type_name(derived_left->input(i)) + " expected).");
        }
      }
    }
  } else {
    throw std::string("unknown type for rvalue.");
  }

  node->type(node->lvalue()->type()); // useful?
}

//---------------------------------------------------------------------------

void l22::type_checker::do_program_node(l22::program_node *const node, int lvl) {
  // EMPTY
}

void l22::type_checker::do_evaluation_node(l22::evaluation_node *const node, int lvl) {
  node->argument()->accept(this, lvl + 2);
}

void l22::type_checker::do_print_node(l22::print_node *const node, int lvl) {
  node->arguments()->accept(this, lvl + 2);
}

//---------------------------------------------------------------------------

void l22::type_checker::do_read_node(l22::read_node *const node, int lvl) {
  node->type(cdk::primitive_type::create(0, cdk::TYPE_UNSPEC));
}

//---------------------------------------------------------------------------

void l22::type_checker::do_while_node(l22::while_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) throw std::string("while: expected integer condition");
}

//---------------------------------------------------------------------------

void l22::type_checker::do_if_node(l22::if_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) throw std::string("if: expected integer condition");
}

void l22::type_checker::do_if_else_node(l22::if_else_node *const node, int lvl) {
  node->condition()->accept(this, lvl + 4);
  if (!node->condition()->is_typed(cdk::TYPE_INT)) throw std::string("if_else: expected integer condition");
}


//---------------------------------------------------------------------------

//---------------------------------- AUXILIARES -----------------------------

std::string l22::type_checker::type_name(std::shared_ptr<cdk::basic_type> type) {
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