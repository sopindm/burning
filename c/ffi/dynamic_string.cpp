extern "C"
{
  void free_dynamic_string( char* string )
  {
    delete string;
  }
}
