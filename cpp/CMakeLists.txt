cmake_minimum_required(VERSION 3.10)
set(CMAKE_C_COMPILER /usr/bin/clang)
set(CMAKE_CXX_COMPILER /usr/bin/clang++)

project (verify)

set(CMAKE_CXX_STANDARD 11)
set(cmake_export_compile_commands on)


if (NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release)
    set(CMAKE_B)
endif ()

include_directories(
    ${MY_SOURCE_DIR}/src
    ${MY_SOURCE_DIR}/include
)

add_executable (
    ${PROJECT_NAME} src/main.cpp
    include/candidates.hpp)
