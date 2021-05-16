# Changes

This is a minor release. It incorporates new functions that solve the latest errors found in CRAN
checks. It also includes modifications to get rid of package dependencies as well as code and 
documentation improvements. Finally, it defuncts a few obsolete functions.

# Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 20.04.2 LTS, R version 4.0.5 (2021-03-31)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 3.6.3 (2020-02-29)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.0.5 (2021-03-31)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.1.0 RC (2021-05-10 r80288)
* OK: rhub, Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* OK: rhub, Ubuntu Linux 20.04.1 LTS, R-release, GCC
* OK: rhub, Fedora Linux, R-devel, clang, gfortran

* OK: travis-ci, x86_64-pc-linux-gnu (64-bit), Ubuntu 16.04.6 LTS, R 4.0.2

# R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE in all test environments.

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’

New submission

Package was archived on CRAN

Possibly mis-spelled words in DESCRIPTION:
  FEBR (3:54, 15:63)

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-10-24 as problems were not corrected
    in time.

  Including not using Suggests conditionally.
```

This NOTE can be ignored.

# Reverse dependencies

There are no reverse dependencies.
