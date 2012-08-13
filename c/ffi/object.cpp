#include <stdlib.h>
#include <string.h>
#include "ffi_object.h"

ffi_object make_object( void* value, const unsigned char type[] )
{
  ffi_object object = (ffi_object)( malloc( sizeof( ffi_object_base )) );
  object->value = value;
  object->type = type;

  return object;
}

extern "C"
{
  void free_dynamic_string( char* string )
  {
    delete string;
  }
}


