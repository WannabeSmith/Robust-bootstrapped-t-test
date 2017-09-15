## Resubmission
This is a resubmission. In this version I have:

* Changed the version number from '0.0.0' to '0.1.0'

* Omitted the redundant 'R' in 'R package'.

* Included a reference

Note that 'R CMD check --as-cran' sees the DOI as invalid (due to a '>' character being part of the DOI). The DOI is as follows:

10.1002/(SICI)1097-0258(19991030)18:20<2749::AID-SIM195>3.0.CO;2-C

## Test environments
* local Fedora 25 install, R 3.4.1
* Ubuntu 14.04, R 3.4.1 (on Travis-CI)
* Windows, R (unstable and 3.4.1) (using win-builder, devel and release)

## R CMD check results (Fedora and Ubuntu)
0 errors | 0 warnings | 0 notes

## R CMD check results (Windows R-release)
0 errors | 0 warnings | 1 note

## R CMD check results (Windows R-devel)
0 errors | 0 warnings | 1 note
