# CRAN Comments

## Changes

This is a minor release. It incorporates new functions that solve the errors found in the CRAN
check results on 2020-10-24. At that time, the package was archived as the following problems
(including not using Suggests conditionally) were not corrected in time:

```
Check Details

Version: 1.1.0
Check: examples
Result: ERROR
    Running examples in 'febr-Ex.R' failed
    The error most likely occurred in:
    
    > base::assign(".ptime", proc.time(), pos = "CheckExEnv")
    > ### Name: febr2spdf
    > ### Title: Create an sf object
    > ### Aliases: febr2spdf febr2sf
    >
    > ### ** Examples
    >
    > res <- observation(dataset = "ctb0003", variable = "taxon",
    + progress = FALSE, verbose = FALSE)
    Error in `[.data.frame`(tmp, , c("coord_x", "coord_y")) :
     undefined columns selected
    Calls: observation -> apply -> [ -> [.data.frame
    Execution halted
Flavors: r-devel-linux-x86_64-debian-clang, r-devel-linux-x86_64-debian-gcc, r-patched-linux-x86_64, r-release-linux-x86_64

Version: 1.1.0
Check: Rd cross-references
Result: NOTE
    Undeclared package ‘openxlsx’ in Rd xrefs
Flavor: r-devel-linux-x86_64-fedora-clang

Version: 1.1.0
Check: examples
Result: ERROR
    Running examples in ‘febr-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: febr2spdf
    > ### Title: Create an sf object
    > ### Aliases: febr2spdf febr2sf
    >
    > ### ** Examples
    >
    > res <- observation(dataset = "ctb0003", variable = "taxon",
    + progress = FALSE, verbose = FALSE)
    Error in `[.data.frame`(tmp, , c("coord_x", "coord_y")) :
     undefined columns selected
    Calls: observation -> apply -> [ -> [.data.frame
    Execution halted
Flavors: r-devel-linux-x86_64-fedora-clang, r-devel-linux-x86_64-fedora-gcc, r-devel-windows-ix86+x86_64, r-patched-solaris-x86, r-release-windows-ix86+x86_64, r-oldrel-windows-ix86+x86_64 
```

This minor release also includes modifications to get rid of package dependencies as well as has
various code and documentation improvements. Finally, it defuncts a few obsolete functions.

## Test environments

* OK: local, x86_64-pc-linux-gnu (64-bit), Ubuntu 20.04.3 LTS, R version 4.1.2 (2021-11-01)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.0.5 (2021-03-31)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R version 4.1.2 (2021-11-01)
* OK: winbuilder, x86_64-w64-mingw32 (64-bit), Windows, R Under development (unstable) (2022-02-16 r81750 ucrt)
* OK: rhub, Fedora Linux, R-devel, clang, gfortran
* OK: rhub, Ubuntu Linux 20.04.1 LTS, R-release, GCC
* OK: rhub, Windows Server 2022, R-devel, 64 bit

## R CMD check results

### ERRORs

There were no ERRORs.

### WARNINGs

There were no WARNINGs.

### NOTEs

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

This NOTE can be ignored. This release incorporates new functions that solve the errors found in the
CRAN check results on 2020-10-24.

## Reverse dependencies

There are no reverse dependencies.
