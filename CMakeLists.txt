# MojoBASIC; a modern reimplementation of QuickBASIC.
#
# Please see the file LICENSE.txt in the source's root directory.
#
#  This file written by Ryan C. Gordon.

cmake_minimum_required(VERSION 2.8)
project(MojoBASIC)

include_directories(.)

find_program(HG hg DOC "Path to hg command line app: http://www.selenic.com/mercurial/")
if(NOT HG)
    MESSAGE(STATUS "Mercurial (hg) not found. You can go on, but version info will be wrong.")
    SET(MOJOBASIC_VERSION -1)
    SET(MOJOBASIC_CHANGESET "???")
else()
    mark_as_advanced(HG)
    execute_process(
        COMMAND hg tip --template {rev}
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        RESULT_VARIABLE HGVERSION_RC
        OUTPUT_VARIABLE MOJOBASIC_VERSION
        ERROR_QUIET
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    execute_process(
        COMMAND hg tip --template hg-{rev}:{node|short}
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        RESULT_VARIABLE HGVERSION_RC
        OUTPUT_VARIABLE MOJOBASIC_CHANGESET
        ERROR_QUIET
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
endif()

write_file(
    "${CMAKE_CURRENT_SOURCE_DIR}/mojobasic_version.h"
    "/* This file was autogenerated. Do not edit! */\n"
    "#ifndef _INCL_MOJOBASIC_VERSION_H_\n"
    "#define _INCL_MOJOBASIC_VERSION_H_\n"
    "#define MOJOBASIC_VERSION ${MOJOBASIC_VERSION}\n"
    "#define MOJOBASIC_CHANGESET \"${MOJOBASIC_CHANGESET}\"\n"
    "#endif\n"
)

if(CMAKE_COMPILER_IS_GNUCC)
    ADD_DEFINITIONS(-Wall -ggdb3)
endif()

if(MSVC)
    add_definitions(-D_CRT_SECURE_NO_WARNINGS=1)
endif()

if(false)
find_program(LLVM_CONFIG llvm-config DOC "Path to llvm-config command line app: http://llvm.org/")
if(NOT LLVM_CONFIG)
    message(FATAL_ERROR "llvm-config not found. This project requires LLVM.")
else()
    mark_as_advanced(LLVM_CONFIG)
    execute_process(
        COMMAND ${LLVM_CONFIG} --cxxflags
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        RESULT_VARIABLE HGVERSION_RC
        OUTPUT_VARIABLE LLVM_CXXFLAGS
        ERROR_QUIET
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    execute_process(
        COMMAND ${LLVM_CONFIG} --ldflags
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        OUTPUT_VARIABLE LLVM_LDFLAGS
        ERROR_QUIET
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
    execute_process(
        COMMAND ${LLVM_CONFIG} --libs
        WORKING_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}"
        OUTPUT_VARIABLE LLVM_LIBS
        ERROR_QUIET
        OUTPUT_STRIP_TRAILING_WHITESPACE
    )
endif()
endif()

add_library(mojobasic STATIC
    mojobasic_common.cpp
    mojobasic_lexer.cpp
    mojobasic_parser.cpp
    mojobasic_preprocessor.cpp
    mojobasic_compiler.cpp
)

if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    SET(PLATFORM_LIBS "m;pthread;dl;ncurses")
endif()

if(APPLE)
    SET(PLATFORM_LIBS "curses")
endif()

set_source_files_properties(
    mojobasic_compiler.cpp
    PROPERTIES COMPILE_FLAGS "${LLVM_CXXFLAGS}"
)

find_program(RE2C re2c DOC "Path to re2c command line app: http://re2c.org/")
if(NOT RE2C)
    MESSAGE(STATUS "re2c missing. You can go on, but can't rebuild the lexer.")
else()
    mark_as_advanced(RE2C)
    add_custom_command(
        OUTPUT "${CMAKE_CURRENT_SOURCE_DIR}/mojobasic_lexer.cpp"
        DEPENDS mojobasic_lexer.re
        COMMAND "${RE2C}"
        ARGS -is --no-generation-date -o "${CMAKE_CURRENT_SOURCE_DIR}/mojobasic_lexer.cpp" "${CMAKE_CURRENT_SOURCE_DIR}/mojobasic_lexer.re"
    )
endif()

add_executable(testapp test.cpp)
target_link_libraries(testapp mojobasic ${LLVM_LIBS} ${PLATFORM_LIBS})
set_target_properties(testapp PROPERTIES LINK_FLAGS "${LLVM_LDFLAGS}")

if(false)
# Unit tests...
add_custom_target(
    tests
    COMMAND "${CMAKE_CURRENT_SOURCE_DIR}/test.sh"
    WORKING_DIRECTORY "${CMAKE_CURRENT_BINARY_DIR}"
    DEPENDS testapp
    COMMENT "Running tests..."
    VERBATIM
)
endif()

# End of CMakeLists.txt ...

