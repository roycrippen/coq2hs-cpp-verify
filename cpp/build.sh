#!/usr/bin/bash
if [ ! -d cmake-build-release ]
then
  mkdir cmake-build-release
fi
cd cmake-build-release

cmake ..
cmake --build .

cd ..
