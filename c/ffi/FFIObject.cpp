#include "FFIObject.hpp"

using namespace burning;

ffi_object FFIObject::createFFI( FFIObject* object, void* realValue )
{
  if( realValue == NULL )
    realValue = object;

  return make_object( realValue, object->type_uuid() );
}
