## Test environments
* local Fedora 25 install, R 3.4.1
* Ubuntu 14.04, R 3.4.1 (on Travis-CI)
* Windows, R (unstable and 3.4.1) (using win-builder, devel and release)

## R CMD check results (Fedora and Ubuntu)
0 errors | 0 warnings | 0 notes

## R CMD check results (Windows R-release)
0 errors | 0 warnings | 1 note

## R CMD check results (Windows R-devel)
1 error | 0 warnings | 1 note

Specific error message for arch i386 on Windows R-devel:
"DLL 'parallel' not found: maybe not installed for this architecture?"

This package depends on the 'parallel' package but it seems to not be installed on the test machine (or DLLs not available). The package was working, however, on September 12, 2017 around 10 PM (UTC+01:00) and no changes have been made since then. 
