macro( copy_file name destination )
  get_target_property( FILENAME ${name} LOCATION )
  add_custom_command( TARGET ${name} POST_BUILD
                      COMMAND ${CMAKE_COMMAND} -E copy ${FILENAME} ${destination} )
endmacro()

macro( copy_library name )
  copy_file( ${name} ${LIBRARY_PATH}/ )
endmacro()

macro( copy_binary name )
  copy_file( ${name} ${BINARY_PATH}/ )
endmacro()

