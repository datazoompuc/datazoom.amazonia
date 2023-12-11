# New submission



# Previous CRAN comments

Since our previous submission, we have added many new functions and changed the way most pre-existing functions work. We have change http to https.

## Test environments
- R-hub windows-x86_64-devel (r-devel)

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

GitHub Actions checks on Windows, MacOS, and Linux also return no errors, warnings, or notes.

## R CMD check results
> On windows-x86_64-devel (r-devel), ubuntu-gcc-release (r-release), fedora-clang-devel (r-devel)
  checking CRAN incoming feasibility ... NOTE
  Maintainer: 'Francisco de Lima Cavalcanti <francisco.lima.cavalcanti@gmail.com>'
  
  New submission
  
  Package was archived on CRAN
  
  CRAN repository db overrides:
    X-CRAN-Comment: Archived on 2022-01-23 for policy violation.
  
    Maintainer is a not a single person. Also, reauires flaky packages
      sidrdr and geobr.
      
  * Maintainer has been changed and package is no longer dependent on the (now archived) package 'geobr'.   

> On windows-x86_64-devel (r-devel)
  checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'
    
  * Seems to be a common note, but it doesn't appear on other checks besides `devtools::check_win_devel()` and `rhub::check_for_cran()`.
  * GitHub Actions checks and local `devtools::check()` display no errors, warnings, or notes.
