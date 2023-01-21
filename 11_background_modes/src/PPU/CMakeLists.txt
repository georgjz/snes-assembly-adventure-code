cmake_minimum_required( VERSION 3.21 FATAL_ERROR )

target_compile_options( ${PROJECT_NAME}
    PRIVATE "SHELL:-I ${CMAKE_CURRENT_SOURCE_DIR}" )

target_sources( ${PROJECT_NAME}
    PRIVATE PPU.s )
