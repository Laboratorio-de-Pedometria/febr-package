## Changes
This is a patch. It includes several bug fixes that fix the problems shown in https://cran.r-project.org/web/checks/check_results_febr.html.

When tested locally -- x86_64-pc-linux-gnu --, two examples needed more that five seconds to run during an 
`R CMD check`. However, when tested on travis-ci and win-builder, these examples where run in less than five 
seconds. The long time needed to run locally likely is due to the relatively poor Internet connection speed.

## Test environments
* local x86_64-pc-linux-gnu (ubuntu 18.04), R 3.4.4
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit

## R CMD check results

```R
0 errors | 0 warnings | 1 note 
checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’

New submission
```

## Reverse dependencies

There are no reverse dependencies.
