#ifndef BURNING_FFI_FFIOBJECT_HPP
#define BURNING_FFI_FFIOBJECT_HPP

#include <stddef.h>
#include "ffi_object.h"

namespace burning
{
  class FFIObject
  {
  public:
    virtual const unsigned char* type_uuid () const = 0;
    static ffi_object createFFI( FFIObject* object, void* realValue = NULL );

  };
}
#endif
