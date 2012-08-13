project( burning-ffi )

add_library( burning_ffi SHARED object.cpp FFIObject.cpp )
install( TARGETS burning_ffi DESTINATION ${LIBRARY_INSTALL_PATH} )
install( FILES ffi_object.h FFIObject.hpp DESTINATION ${HEADER_INSTALL_PATH} )

find_package( UUID REQUIRED )

include_directories( ${UUID_INCLUDE_DIR} )

add_library( burning_ffi_internal SHARED uuid.c dynamic_string.cpp )
target_link_libraries( burning_ffi_internal ${UUID_LIBRARIES} )
install( TARGETS burning_ffi_internal DESTINATION ${LIBRARY_INSTALL_PATH} )

include_directories( ${CMAKE_CURRENT_SOURCE_DIR} )

add_library( ffi_test SHARED tests/simple.cpp )
target_link_libraries( ffi_test burning_ffi )

copy_library( ffi_test )



