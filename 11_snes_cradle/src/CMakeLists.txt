cmake_minimum_required( VERSION 3.21 FATAL_ERROR )

target_compile_options( ${PROJECT_NAME}
    PRIVATE "SHELL:-I ${CMAKE_CURRENT_SOURCE_DIR}" )

add_subdirectory( common )
add_subdirectory( Game )
add_subdirectory( PPU )

target_sources( ${PROJECT_NAME}
    PRIVATE Main.s )
