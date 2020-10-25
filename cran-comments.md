# Changes

This is a minor release. It incorporates a new functions that solve the latest errors found in CRAN checks.
It also includes modifications to get rid of package dependencies.

# Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 18.04.5 LTS, R 3.6.3
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R 4.0.3
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R 3.6.3
* OK: rhub, Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* FAIL: rhub, Ubuntu Linux 16.04 LTS, R-release, GCC
* FAIL: rhub, Fedora Linux, R-devel, clang, gfortran
* OK: travis-ci, x86_64-pc-linux-gnu (64-bit), Ubuntu 16.04.6 LTS, R 4.0.2

Failure in rhub test environments are due to missing software and package dependencies in those test
environments. The __rhub__ package maintainer is aware of this [issue](https://github.com/r-hub/rhub/issues/427).

# R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE in most test environments.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>'

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  FEBR (3:54, 12:63)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-10-24 as problems were not corrected
    in time.

  Including not using Suggests conditionally.
```

This NOTE can be ignored.

# Reverse dependencies

There are no reverse dependencies.
