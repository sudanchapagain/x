cmake_minimum_required(VERSION 3.20)
project(load_balancer_cpp LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

find_package(CURL REQUIRED)

include(FetchContent)

FetchContent_Declare(
    httplib
    GIT_REPOSITORY https://github.com/yhirose/cpp-httplib.git
    GIT_TAG        master
)

FetchContent_MakeAvailable(httplib)

# server
add_executable(backend_server
    src/server.cpp
)

target_link_libraries(backend_server PRIVATE
    httplib
)

# load balancer
add_executable(load_balancer
    src/main.cpp
)

target_include_directories(load_balancer PRIVATE
    ${CURL_INCLUDE_DIRS}
)

target_link_libraries(load_balancer PRIVATE
    ${CURL_LIBRARIES}
    httplib
)

target_compile_definitions(load_balancer PRIVATE
    CPPHTTPLIB_OPENSSL_SUPPORT=0
)

