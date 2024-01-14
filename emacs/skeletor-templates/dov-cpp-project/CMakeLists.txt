cmake_minimum_required(VERSION 3.27)
project(hello-world LANGUAGES CXX)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_CURRENT_SOURCE_DIR}/cmake)

add_executable(__PROJECT-NAME__
  __PROJECT-NAME__.cpp)

find_package(fmt CONFIG REQUIRED)
target_link_libraries(__PROJECT-NAME__ PRIVATE
  fmt::fmt)
