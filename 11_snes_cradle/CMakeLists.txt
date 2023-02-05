cmake_minimum_required( VERSION 3.21 FATAL_ERROR )

set( MEMORY_MAP_FILE ${CMAKE_CURRENT_SOURCE_DIR}/MemoryMap.cfg )
list( APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake" )

project( SNESCradle
         LANGUAGES CA65816 )

add_executable( ${PROJECT_NAME} "" )
add_subdirectory( assets )
add_subdirectory( src )
set_target_properties( ${PROJECT_NAME}
    PROPERTIES SUFFIX ".smc" )

# Check if the path to the memory map file is set
if( NOT DEFINED MEMORY_MAP_FILE )
  message( FATAL_ERROR "Path to memory map file not set!" )
endif( NOT DEFINED MEMORY_MAP_FILE )
