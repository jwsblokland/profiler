# Profiler
The library profiler offers an easy API to add timers to an existing Fortran libary
or program. This is done via using a global or local timer object. See the API
documentation for detailed API documentation. This documentation can be generated
using Doxygen.

## Building and installation
For building the library profiler one needs a Fortran 2008 compliant compiler and 
CMake. The instructions for building and installing profiler can be found be below.

The first step of the building is by configuring it using CMake. This is done using the following
command
```
  $ FC=<fortran_compiler> cmake -DCMAKE_BUILD_TYPE=Release -DBUILD_DOC=ON -B <bld_dir> -S <src_dir>
```
Here, `<fortran_compiler>`, `<bld_dir>` and `<src_dir>` are the Fortran compiler, build directory and
the source directory, respectively. The `-DBUILD_DOC=ON` flag means that the user documentation
will be generated using Doxygen. If one wants to generate the internal user documentation, the
flag `-DBUILD_DOC_INTERNAL=ON` needs to be added to the CMake command.

The next step is actually building the code. This is also done using CMake via the command
```
  $ cmake --build <bld_dir> --config Release
```
On a Linux system it is also possible to make use of `make`.

Before the installation, one can run the tests by making use for CTest via the command
```
  $ ctest --test <bld_dir>
```

The final step is the installation of ptools. This is also done using CMake using the
command
```
  $ cmake --install <bld_dir> --prefix <install_dir>
```
Here, `<install_dir>` is the installation prefix directory.

## Limitations
Currently, the profiler library does not support either OpenMP or MPI natively. This does not mean, you can not
the library in an OpenMP and/or MPI enabled library or program. For example, if one wants to time certain OpenMP
loop you need to place the timer outside the loop such you time theloop as a whole instead of the individual
loop iterations.
