# Changes

This is a patch. It includes several bug fixes that fix the problems shown in https://cran.r-project.org/web/checks/check_results_febr.html.

When tested locally -- x86_64-pc-linux-gnu --, two examples needed more that five seconds to run during an 
`R CMD check`. However, when tested on travis-ci and win-builder, these examples where run in less than five 
seconds. The long time needed to run locally likely is due to the relatively poor Internet connection speed.

# Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 18.04.01 LTS, R 3.6.0
* OK: travis-ci, x86_64-pc-linux-gnu (64-bit), Ubuntu 14.04.5 LTS, R 3.6.0
* OK: rhub, Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* FAIL: rhub, Ubuntu Linux 16.04 LTS, R-release, GCC
* FAIL: rhub, Fedora Linux, R-devel, clang, gfortran
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R 3.5.3
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R 3.6.0
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable)

Failure in rhub test environments are due to missing software and package dependencies in those test
environments.

On Ubuntu Linux:

```
5572#> Warning messages:
5573#> 1: In i.p(...) : installation of package ‘rgdal’ had non-zero exit status
5574#> 2: In i.p(...) : installation of package ‘xml2’ had non-zero exit status
5575#> 3: In i.p(...) :
5576#> installation of package ‘openssl’ had non-zero exit status
5577#> 4: In i.p(...) : installation of package ‘httr’ had non-zero exit status
5578#> 5: In i.p(...) :
5579#> installation of package ‘googlesheets’ had non-zero exit status
5580#> 6: In i.p(...) :
5581#> installation of package ‘/tmp/RtmpGI8dbA/file13b111cf369/febr_1.0.1.9003.tar.gz’ had non-zero exit status
```

On Fedora Linux:

```
3574#> Warning messages:
3575#> 1: In i.p(...) : installation of package ‘rJava’ had non-zero exit status
3576#> 2: In i.p(...) :
3577#> installation of package ‘xlsxjars’ had non-zero exit status
3578#> 3: In i.p(...) : installation of package ‘xlsx’ had non-zero exit status
3579#> 4: In i.p(...) :
3580#> installation of package ‘/tmp/RtmpN4SnNb/file11c4d39b687/febr_1.0.1.9003.tar.gz’ had non-zero exit status
```

The __rhub__ package maintainer has been warned about this issue.


# R CMD check results

There were no ERRORs or WARNINGs.

There was one NOTE in all platforms.

```R
0 errors | 0 warnings | 1 note 
checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’

New submission
```

This NOTE can be ignored.

# Reverse dependencies

There are no reverse dependencies.
