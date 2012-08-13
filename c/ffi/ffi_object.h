#ifndef BURNING_FFI_OBJECT_H
#define BURNING_FFI_OBJECT_H

typedef struct
{
  void* value;
  const unsigned char* type;
}ffi_object_base, *ffi_object;

ffi_object make_object( void* value, const unsigned char type[] );
#define MAKE_OBJECT(value, class) make_object( value, class ## _uuid )

#endif

  
