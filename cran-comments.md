## Resubmission
This is a resubmission. In this version I have:

* Changed the version number from '0.0.0' to '0.1.0' 

* Omitted the redundant 'R' in 'R package'.

* Included a reference
Note that 'R CMD check --as-cran' sees the DOI as invalid (due to a '>' character being part of the DOI). However, I think it is correct.

## Test environments
* local Fedora 25 install, R 3.4.1
* Ubuntu 14.04, R 3.4.1 (on Travis-CI)
* Windows, R (unstable and 3.4.1) (using win-builder, devel and release)

## R CMD check results (Fedora and Ubuntu)
0 errors | 0 warnings | 0 notes

## R CMD check results (Windows R-release)
0 errors | 0 warnings | 1 note

## R CMD check results (Windows R-devel)
0 errors | 0 warnings | 2 notes

Specific note on Windows R-devel:
"Examples with CPU or elapsed time > 10s"

This isn't too unusual since I am running some examples with 9999 bootstrap resamples and the function 'mclapply' in the 'parallel' package is not supported on Windows. Therefore, these 9999 resamples are running on a single core.  
