#include <stdio.h>

void swap( int* a, int* b )
{
  int tmp = *a;
  *a = *b;
  *b = tmp;
}

int compare( int* a, int* b )
{
  return *b < *a;
}

void compare_and_swap( int* a, int* b )
{
  if( compare( a, b ) )
    swap( a, b );
}

void sort( int* array, int length )
{
  int i,j;

  for( i=length-1; i>=1; i-- )
    for( j=0; j<i; j++ )
      compare_and_swap( &array[ j ] , &array[ j + 1 ] );
}

int main()
{
  int* fa[] = { 7, 3, 5, 2, 6, 2, 3, 5, 7 };
  int array[] = { 7, 3, 5, 2, 6, 2, 3, 5, 7 };
  int i;

  {
    int i;
    int j = i;
  }

  sort( array, 9 );

  for( i=0; i<9; i++ )
    printf( "%d ", array[ i ] );
}
