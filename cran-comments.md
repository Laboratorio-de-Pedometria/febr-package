# CRAN Comments

## Changes

This is a patch release. It includes modifications to get rid of warnings in tests under.
Debian Linux, R-devel, clang, ISO-8859-15 locale. Warnings were due to R code comments using
non-ASCII characters. Those comments were removed in this patch.

## Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 20.04.3 LTS, R version 4.1.2 (2021-11-01)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.0.5 (2021-03-31)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.1.2 (2021-11-01)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable) (2022-02-26 r81819 ucrt)
* OK: rhub, Fedora Linux, R-devel, clang, gfortran
* OK: rhub, Ubuntu Linux 20.04.1 LTS, R-release, GCC
* OK: rhub, Windows Server 2022, R-devel, 64 bit
* OK: rhub, Debian Linux, R-devel, clang, ISO-8859-15 locale

## R CMD check results

### ERRORs

There were no ERRORs.

### WARNINGs

There were no WARNINGs.

### NOTEs

There was one NOTE in most test environments:

```
Maintainer: 'Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>'
```

## Reverse dependencies

There are no reverse dependencies.
