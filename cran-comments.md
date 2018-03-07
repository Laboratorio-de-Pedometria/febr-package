## Changes
This is a new release. Most examples need more that 5 sec to run during a R CMD check. That is because the
functions are used to access a server and download data. The run time likely depends on the Internet 
connection. These examples have been tested using `devtools::run_examples()`. For submission, they were wrapped
in `\donttest{}`.

## Test environments
* local x86_64-pc-linux-gnu (ubuntu 16.04), R 3.4.3
* ubuntu 14.04 (on travis-ci), R 3.4.2
* win-builder (devel and release)

## R CMD check results
0 errors | 0 warnings | 1 note 
checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’

New submission

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
