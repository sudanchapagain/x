#!/usr/bin/env sh

c++ src/main.cpp -o build/player `pkg-config --cflags --libs gstreamer-1.0`
