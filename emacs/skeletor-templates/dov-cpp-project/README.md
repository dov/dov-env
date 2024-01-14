# __PROJECT-NAME__

## Summary

A minimal example of how to use vcpkg with cmake. This example shows linking to the fmt and the spdlog libraries

# Building

Use the following command to build:

```sh
mkdir build
cd build
cmake --preset ninja -DCMAKE_BUILD_TYPE=Debug ..
ninja
```

