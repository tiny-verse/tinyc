cmake_minimum_required(VERSION 3.5)

set(PROJECT_NAME "libtinyc")

project(${PROJECT_NAME})
include_directories(${CMAKE_CURRENT_SOURCE_DIR})
include_directories(${CMAKE_SOURCE_DIR}/common)
file(GLOB_RECURSE SRC "*.cpp" "*.h")
add_library(${PROJECT_NAME} ${SRC})

set(TINY_LIBRARIES "${TINY_LIBRARIES};${PROJECT_NAME}" PARENT_SCOPE)
