# Changes

This is a minor release. It consists of a important modifications in order to get rid of dependencies on the 
following packages: __googlesheets__, __googlesheets4__, __sp__, and __xlsx__. This is done by using new 
internal functions. None od the changes affect the behavior os the package.

# Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 18.04.3 LTS, R 3.6.1
* OK: travis-ci, x86_64-pc-linux-gnu (64-bit), Ubuntu 14.04.5 LTS, R 3.6.2
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R 3.5.3

* OK: rhub, Windows Server 2008 R2 SP1, R-devel, 32/64 bit

* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R 3.6.2
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable)
* FAIL: rhub, Ubuntu Linux 16.04 LTS, R-release, GCC
* FAIL: rhub, Fedora Linux, R-devel, clang, gfortran



Failure in rhub test environments are due to missing software and package dependencies in those test
environments.

On Ubuntu Linux:

```

```

On Fedora Linux:

```
3255#> Warning messages:
3256#> 1: In i.p(...) : installation of package ‘rJava’ had non-zero exit status
3257#> 2: In i.p(...) :>
3258#> installation of package ‘xlsxjars’ had non-zero exit status
3259#> 3: In i.p(...) : installation of package ‘xlsx’ had non-zero exit status
3260#> 4: In i.p(...) :
3261#> installation of package ‘/tmp/RtmpvO7ETT/file11f614f864c/febr_1.0.2.9001.tar.gz’ had non-zero exit status
```

The __rhub__ package maintainer has been warned about this issue.

# R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE in most test environments.

```R
0 errors | 0 warnings | 1 note 
checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’

New submission
```

This NOTE can be ignored.

# Reverse dependencies

There are no reverse dependencies.
