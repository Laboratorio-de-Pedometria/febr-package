## Changes
This is a patch. It includes several bug fixes that fix the problems shown in https://cran.r-project.org/web/checks/check_results_febr.html.

When tested locally -- x86_64-pc-linux-gnu --, two examples needed more that five seconds to run during an 
`R CMD check`. However, when tested on travis-ci and win-builder, these examples where run in less than five 
seconds. The long time needed to run locally likely is due to the relatively poor Internet connection speed.

## Test environments
* OK: local x86_64-pc-linux-gnu (ubuntu 18.04), R 3.4.4
* OK: ubuntu 14.04 (on travis-ci), R 3.5.1
* OK: win-builder (devel and release)
* OK: Windows Server 2008 R2 SP1, R-devel, 32/64 bit (rhub)
* FAIL: Ubuntu Linux 16.04 LTS, R-release, GCC (rhub)
* FAIL: Fedora Linux, R-devel, clang, gfortran (rhub)

Failure in rhub test environments are due to missing software and package dependencies in those test
environments. I am not sure how to deal with these failures.

```
* installing *source* package ‘openssl’ ...
** package ‘openssl’ successfully unpacked and MD5 sums checked
Using PKG_CFLAGS=
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because openssl was not found. Try installing:
 * deb: libssl-dev (Debian, Ubuntu, etc)
 * rpm: openssl-devel (Fedora, CentOS, RHEL)
 * csw: libssl_dev (Solaris)
 * brew: openssl@1.1 (Mac OSX)
If openssl is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a openssl.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘openssl’
* removing ‘/home/docker/R/openssl’
```

```
ERROR: dependency ‘openssl’ is not available for package ‘httr’
* removing ‘/home/docker/R/httr’
```

```
checking for gdal-config... no
no
configure: error: gdal-config not found or not executable.
ERROR: configuration failed for package ‘rgdal’
* removing ‘/home/docker/R/rgdal’
```

```
------------------------- ANTICONF ERROR ---------------------------
Configuration failed because libxml-2.0 was not found. Try installing:
 * deb: libxml2-dev (Debian, Ubuntu, etc)
 * rpm: libxml2-devel (Fedora, CentOS, RHEL)
 * csw: libxml2_dev (Solaris)
If libxml-2.0 is already installed, check that 'pkg-config' is in your
PATH and PKG_CONFIG_PATH contains a libxml-2.0.pc file. If pkg-config
is unavailable you can set INCLUDE_DIR and LIB_DIR manually via:
R CMD INSTALL --configure-vars='INCLUDE_DIR=... LIB_DIR=...'
--------------------------------------------------------------------
ERROR: configuration failed for package ‘xml2’
* removing ‘/home/docker/R/xml2’
```

```
ERROR: dependencies ‘httr’, ‘xml2’ are not available for package ‘googlesheets’
* removing ‘/home/docker/R/googlesheets’
```

```
Installing package into ‘/home/docker/R’
(as ‘lib’ is unspecified)
ERROR: dependency ‘googlesheets’ is not available for package ‘febr’
* removing ‘/home/docker/R/febr’
Warning messages:
1: In i.p(...) :
  installation of package ‘openssl’ had non-zero exit status
2: In i.p(...) : installation of package ‘httr’ had non-zero exit status
3: In i.p(...) : installation of package ‘rgdal’ had non-zero exit status
4: In i.p(...) : installation of package ‘xml2’ had non-zero exit status
5: In i.p(...) :
  installation of package ‘googlesheets’ had non-zero exit status
6: In i.p(...) :
  installation of package ‘/tmp/RtmpWDlUFh/file1068507ab1/febr_1.0.1.tar.gz’ had non-zero exit status
```

## R CMD check results

```R
0 errors | 0 warnings | 1 note 
checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’

New submission
```

## Reverse dependencies

There are no reverse dependencies.
