# How to build objects
set( CMAKE_CA65816_COMPILE_OBJECT
    "<CMAKE_CA65816_COMPILER> --cpu 65816 \
                              <FLAGS> \
                              -s \
                              -o <OBJECT> \
                              <SOURCE>"
)

# How to build executables
set( CMAKE_CA65816_LINK_EXECUTABLE
    "ld65 -C ${MEMORY_MAP_FILE} \
          <OBJECTS> \
          -o <TARGET>"
)

set( CMAKE_CA65816_INFORMATION_LOADED 1 )
