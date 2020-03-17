# Changes

This is a minor release. It consists of a important modifications in order to get rid of dependencies on the 
following packages: __googlesheets__, __googlesheets4__, __sp__, and __xlsx__. This is done by using new 
internal functions. None od the changes affect the behavior os the package.

# Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 18.04.3 LTS, R 3.6.1
* OK: travis-ci, x86_64-pc-linux-gnu (64-bit), Ubuntu 14.04.5 LTS, R 3.6.2
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R 3.5.3
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R 3.6.3
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable)
* OK: rhub, Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* OK: rhub, Fedora Linux, R-devel, clang, gfortran
* FAIL: rhub, Ubuntu Linux 16.04 LTS, R-release, GCC

Failure in rhub test environments are due to missing software and package dependencies in those test
environments. The __rhub__ package maintainer is aware of this issue.

# R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE in most test environments.

```
0 errors | 0 warnings | 1 note 
checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’

New submission
```

This NOTE can be ignored.

There were two NOTEs in rhub test environments:

```
* checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’

Found the following (possibly) invalid URLs:
  URL: https://www.ibge.gov.br/geociencias/downloads-geociencias.html
    From: man/observation.Rd
    Status: 403
    Message: Forbidden

* checking Rd cross-references ... NOTE
Package unavailable to check Rd xrefs: ‘openxlsx’
```

These NOTEs can be ignored.

# Reverse dependencies

There are no reverse dependencies.
