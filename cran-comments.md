## Changes
This is a new release.

When tested locally -- x86_64-pc-linux-gnu --, most examples needed more that five seconds to run during an 
`R CMD check`. However, when tested on travis-ci and win-builder, these examples where run in less than five 
seconds. The long time needed to run locally likely is due to the relatively poor Internet connection speed. 
Moreover, the local `R CMD check` indicates a memory leak (which can be ignored):

```R
The error most likely occurred in:

> base::assign(".ptime", proc.time(), pos = "CheckExEnv")
> ### Name: observation
> ### Title: Get _observation_ table
> ### Aliases: observation
> 
> ### ** Examples
... 55 lines ...
==14993==    definitely lost: 0 bytes in 0 blocks
==14993==    indirectly lost: 0 bytes in 0 blocks
==14993==      possibly lost: 0 bytes in 0 blocks
==14993==    still reachable: 63,055,385 bytes in 31,835 blocks
==14993==         suppressed: 0 bytes in 0 blocks
==14993== Rerun with --leak-check=full to see details of leaked memory
==14993== 
==14993== For counts of detected and suppressed errors, rerun with: -v
==14993== Use --track-origins=yes to see where uninitialised values come from
==14993== ERROR SUMMARY: 4982 errors from 11 contexts (suppressed: 0 from 0)
** found \donttest examples: check also with --run-donttest
```

## Test environments
* local x86_64-pc-linux-gnu (ubuntu 16.04), R 3.4.3
* ubuntu 14.04 (on travis-ci), R 3.4.2
* win-builder (devel and release)

## R CMD check results

```R
0 errors | 0 warnings | 1 note 
checking CRAN incoming feasibility ... NOTE
Maintainer: ‘Alessandro Samuel-Rosa <alessandrosamuelrosa@gmail.com>’

New submission
```

## Reverse dependencies

This is a new release, so there are no reverse dependencies.
