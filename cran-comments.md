# Changes

This is a minor release. It incorporates new functions that solve the latest errors found in CRAN
checks. It also includes modifications to get rid of package dependencies as well as has various
code and documentation improvements. Finally, it defuncts a few obsolete functions.

# Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 20.04.2 LTS, R version 4.0.5 (2021-03-31)
* OK: travis-ci, x86_64-pc-linux-gnu (64-bit), Ubuntu 16.04.6 LTS, R version 4.0.2 (2020-06-22)
* NOTE: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 3.6.3 (2020-02-29)
* NOTE: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.0.5 (2021-03-31)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.1.0 RC (2021-05-10 r80288)
* NOTE: rhub, Fedora Linux, R-devel, clang, gfortran
* NOTE: rhub, Ubuntu Linux 20.04.1 LTS, R-release, GCC
* PREPERROR: rhub, Windows Server 2008 R2 SP1, R-release, 32/64 bit

# R CMD check results

## PREPERRORs

The PREPERROR observed in rhub (Windows Server 2008 R2 SP1, R-release, 32/64 bit) was:

```
Error: Bioconductor does not yet build and check packages for R version 4.2; see
Execution halted
  https://bioconductor.org/install
```

This PREPERROR can be ignored as it pertains to the configuration of the remote test environment.

## ERRORs

There were no ERRORs.

## WARNINGs

There were no WARNINGs.

## NOTEs

There was one NOTE in most test environments:

```
Maintainer: 'Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>'

New submission

Package was archived on CRAN

CRAN repository db overrides:
  X-CRAN-Comment: Archived on 2020-10-24 as problems were not corrected
    in time.

  Including not using Suggests conditionally.
```

This NOTE can be ignored.

A few test environments issued the following note:

```
* checking examples ... NOTE
Examples with CPU (user + system) or elapsed time > 5s
        user system elapsed
febr2sf 0.54  0.041   5.121
```

This NOTE can be ignored.

# Reverse dependencies

There are no reverse dependencies.
