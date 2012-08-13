#include <uuid/uuid.h>
#include <stdlib.h>

char* generate_uuid()
{
  uuid_t uuid;
  char* ret = malloc( 256 );

  uuid_generate( &uuid );
  uuid_unparse( uuid, ret );

  return ret;
}


