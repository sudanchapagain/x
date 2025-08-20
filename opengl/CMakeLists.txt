cmake_minimum_required(VERSION 3.20)
project(ComputerGraphicsAssignments)

if(NOT DEFINED PROJECT_ID)
  if(DEFINED ENV{PROJECT_ID})
    set(PROJECT_ID $ENV{PROJECT_ID})
  else()
    message(FATAL_ERROR "set PROJECT_ID. `-DPROJECT_ID=01` or export PROJECT_ID=01")
  endif()
endif()

message(STATUS "building ${PROJECT_ID}")

file(GLOB PROJECT_FOLDERS "${PROJECT_ID}_*")

if(NOT PROJECT_FOLDERS)
  message(FATAL_ERROR "no folder found - ${PROJECT_ID}")
endif()

list(GET PROJECT_FOLDERS 0 PROJECT_FOLDER)
message(STATUS "project folder: ${PROJECT_FOLDER}")

set(SRC_FILE "${PROJECT_FOLDER}/src/main.cpp")

if(NOT EXISTS ${SRC_FILE})
  message(FATAL_ERROR "source file ${SRC_FILE} does not exist")
endif()

add_executable(Project${PROJECT_ID} ${SRC_FILE})

find_package(OpenGL REQUIRED)
find_package(GLUT REQUIRED)
target_link_libraries(Project${PROJECT_ID} PRIVATE OpenGL::GL GLUT::GLUT GLU)
