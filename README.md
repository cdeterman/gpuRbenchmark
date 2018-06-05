# gpuRbenchmark

The gpuR benchmarking package.  Herein is contained wrappers for benchmarking various operations that are executed on the GPU relative to base R.


Note, for Windows users, you may to execute the following prior to install in order for the build to complete successfully.  The package needs to link against the `float` package in order to have support for "float" values.  Unfortunately, the linking that can be done on Linux isn't supported on Windows and therefore requires your `PATH` to be updated.

```r
float_libs_dir_rel = system.file(path, package="float")
float_libs_dir = tools::file_path_as_absolute(float_libs_dir_rel)
dll.path <- normalizePath(float_libs_dir)
dll.path <- utils::shortPathName(dll.path)
dll.path <- gsub("\\\\", "/", dll.path)

Sys.setenv(PATH = paste(dll.path, Sys.getenv("PATH"), sep = ";"))
```

### Example GEMM benchmark

```r
library(gpuRbenchmark)
benchmark_gemm(type="float")
```