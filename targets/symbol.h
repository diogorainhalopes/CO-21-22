#ifndef __L22_TARGETS_SYMBOL_H__
#define __L22_TARGETS_SYMBOL_H__

#include <string>
#include <memory>
#include <cdk/types/basic_type.h>

namespace l22 {

  class symbol {
    int _qualifier; // qualifiers: use, public, foreign, "private" (i.e., none)
    std::shared_ptr<cdk::basic_type> _type;
    std::string _name;
    long _value; // hack!
    bool _foreign = false;
    
    std::vector<std::shared_ptr<cdk::basic_type>> _argument_types;

    int _offset = 0; // 0 (zero) means global variable/function
   

  public:
    symbol(int qualifier, std::shared_ptr<cdk::basic_type> type, const std::string &name, 
         long value, bool foreign = false) :
      _qualifier(qualifier), _type(type), _name(name), _value(value), _foreign(foreign) {
    }

    virtual ~symbol() {
      // EMPTY
    }

    std::shared_ptr<cdk::basic_type> type() const {
      return _type;
    }
    bool is_typed(cdk::typename_type name) const {
      return _type->name() == name;
    }
    void set_type(std::shared_ptr<cdk::basic_type> t) {
      _type = t;
    }
    const std::string &name() const {
      return _name;
    }
    long value() const {
      return _value;
    }
    long value(long v) {
      return _value = v;
    }
    bool foreign() {
      return _foreign;
    }
    int offset() const {
      return _offset;
    }
    int qualifier() const {
      return _qualifier;
    }
    void set_offset(int offset) {
      _offset = offset;
    }
    bool global() const {
      return _offset == 0;
    }
    void set_argument_types(const std::vector<std::shared_ptr<cdk::basic_type>> &types) {
      _argument_types = types;
    }

    bool argument_is_typed(size_t ax, cdk::typename_type name) const {
      return _argument_types[ax]->name() == name;
    }
    std::shared_ptr<cdk::basic_type> argument_type(size_t ax) const {
      return _argument_types[ax];
    }

    size_t argument_size(size_t ax) const {
      return _argument_types[ax]->size();
    }

    size_t number_of_arguments() const {
      return _argument_types.size();
    }
  };

  inline auto make_symbol(int qualifier, std::shared_ptr<cdk::basic_type> type, const std::string &name,
                           long value, bool foreign = false) {
    return std::make_shared<symbol>(qualifier, type, name, value, foreign);
  }
} // l22

#endif
