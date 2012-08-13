project( burning )
cmake_minimum_required( VERSION 2.6 )

set( CMAKE_INSTALL_PREFIX /usr )

set( CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake )
set( BURNING_PATH ${CMAKE_CURRENT_SOURCE_DIR} )

include( ${BURNING_PATH}/cmake/filesystem.cmake )

set( LIBRARY_PATH ${CMAKE_CURRENT_SOURCE_DIR}/lib )

set( LIBRARY_INSTALL_PATH ${CMAKE_INSTALL_PREFIX}/lib )
set( HEADER_INSTALL_PATH ${CMAKE_INSTALL_PREFIX}/include )

add_subdirectory( ffi )