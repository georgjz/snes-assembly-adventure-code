# Find the ca65 assembler
find_program(
    CMAKE_CA65816_COMPILER
        NAMES "ca65"
        HINTS "${CMAKE_SOURCE_DIR}"
        DOC "ca65 assembler"
)

mark_as_advanced( CMAKE_CA65816_COMPILER )

set( CMAKE_CA65816_SOURCE_FILE_EXTENSIONS s;asm )
set( CMAKE_CA65816_OUTPUT_EXTENSION .o )
set( CMAKE_CA65816_COMPILER_ENV_VAR "FOO" )

# Configure variables set in this file for fast reload later on
configure_file( ${CMAKE_CURRENT_LIST_DIR}/CMakeCA65816Compiler.cmake.in
                ${CMAKE_PLATFORM_INFO_DIR}/CMakeCA65816Compiler.cmake )
