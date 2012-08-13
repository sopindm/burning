include(FindPackageHandleStandardArgs)

find_path( UUID_INCLUDE_DIR  "uuid/uuid.h" )
find_library( UUID_LIBRARIES uuid ) 

find_package_handle_standard_args( UUID DEFAULT_MSG UUID_LIBRARIES UUID_INCLUDE_DIR )

