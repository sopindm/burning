#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "ffi_object.h"
#include "handler.uuid.h"

extern "C"
{
  int int_sum( int a, int b )
  {
    return a + b;
  }

  short short_sum( short a, short b )
  {
    return a + b;
  }

  long long_sum( long a, long b )
  {
    return a + b;
  }

  float float_sum( float a, float b )
  {
    return a + b;
  }

  unsigned uint_sum( unsigned a, unsigned b )
  {
    return a + b;
  }

  unsigned short ushort_sum( unsigned short a, unsigned short b )
  {
    return a + b;
  }

  unsigned long ulong_sum( unsigned long a, unsigned long b )
  {
    return a + b;
  }

  double double_sum( double a, double b )
  {
    return a + b;
  }

  int bool_and( int a, int b )
  {
    return a && b;
  }

  int sum_generated( int (*generator)() )
  {
    return (*generator)() + (*generator)();
  }

  int double_generated( int (*generator1)(), int (*generator2)() )
  {
    return sum_generated( generator1 ) + sum_generated( generator2 );
  }

  int ranged_generator( int (*generator)( int, int ) )
  {
    int gen1 = (*generator)( 0, 10 );
    int gen2 = (*generator)( 20, 100 );

    if( gen1 <0 || gen1 > 10 )
      return 0;
    if( gen2 <20 || gen2 > 100 )
      return 0;

    return 1;
  }

  int simple_generator()
  {
    static int value = -1;
    value = value + 1;

    return value;
  }

  int (*get_generator())()
  {
    return &simple_generator;
  }

  int string_length( const char* string )
  {
    return strlen( string );
  }

  const char* sample_string ()
  {
    return "Hello, Lisp!!!";
  }

  typedef struct
  {
    int value;
  } int_holder;

  void* make_holder()
  {
    void* ret = malloc( sizeof( int_holder ));
    return MAKE_OBJECT( ret, int_holder );
  }

  void delete_holder( int_holder* holder )
  {
    holder->value = -1; //Let's think that -1 means that we released holder :)
  }

  int hld_holded( int_holder* holder )
  {
    return holder->value;
  }

  void hld_set_holded( int_holder* holder, int value )
  {
    holder->value = value;
  }

  void* make_mega_holder ()
  {
    void* ret = malloc( sizeof( int_holder ));
    return MAKE_OBJECT( ret, mega_holder );
  }

  int sum_array( int* array, int size )
  {
    int sum = 0, i;

    for( i=0; i<size; i++ )
      sum += array[ i ];

    return sum;
  }

  int complex_sum_array( int** arrays, int* sizes, int s_size )
  {
    int sum = 0, i;

    for( i=0; i<s_size; i++ )
      sum += sum_array( arrays[ i ], sizes[ i ] );

    return sum;
  }

  char* generate_string( int value )
  {
    int length( 0 );

    if( value < 10 )
      length = 1;
    else 
      length = 2;

    char* string = new char[ length + 1 ];
    string[ length ] = 0;

    string[ 0 ] = '0' + value % 10;

    if( length == 2 )
      string[ 1 ] = '0' + (value / 10) % 10;

    return string;
  }
}
