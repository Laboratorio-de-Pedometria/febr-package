# Changes

This is a patch. It consists of a minor internal modification in all download functions as to cope with 
changes in data storage in the Free Brazilian Repository for Open Soil Data (___febr___) that will be
implemented in the near future.

When tested locally -- x86_64-pc-linux-gnu --, two examples needed more that five seconds to run during an 
`R CMD check`. However, when tested on travis-ci and win-builder, these examples where run in less than five 
seconds. The long time needed to run locally likely is due to the relatively poor Internet connection speed.

# Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 18.04.3 LTS, R 3.6.2
* OK: travis-ci, x86_64-pc-linux-gnu (64-bit), Ubuntu 14.04.5 LTS, R 3.6.2
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R 3.5.3
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R 3.6.2
* OK: rhub, Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* FAIL: rhub, Ubuntu Linux 16.04 LTS, R-release, GCC
* FAIL: rhub, Fedora Linux, R-devel, clang, gfortran


* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable)

Failure in rhub test environments are due to missing software and package dependencies in those test
environments.

On Ubuntu Linux:

```
5859#> Warning messages:
5860#> 1: In i.p(...) : installation of package ‘rgdal’ had non-zero exit status
5861#> 2: In i.p(...) : installation of package ‘xml2’ had non-zero exit status
5862#> 3: In i.p(...) :
5863#> installation of package ‘openssl’ had non-zero exit status
5864#> 4: In i.p(...) : installation of package ‘httr’ had non-zero exit status
5865#> 5: In i.p(...) :
5866#> installation of package ‘googlesheets’ had non-zero exit status
5867#> 6: In i.p(...) :
5868#> installation of package ‘/tmp/Rtmp5y4qjZ/file1323239050a/febr_1.0.2.9001.tar.gz’ had non-zero exit status
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
