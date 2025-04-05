## R CMD check results

0 errors | 0 warnings | 1 note

* New submission

## Test environments

* Local R installation: R 4.4.3
* R-CMD-check GitHub Action: 
  - macos-latest (release)
  - window-latest (release)
  - ubuntu-latest (devel)
  - ubuntu-latest (release)
  - ubuntu-latest (oldrel-1)
* win-builder: 
  - using R version 4.5.0 RC (2025-04-04 r88112 ucrt) 
  - using platform: x86_64-w64-mingw32

## Additional comments

* Although `penguins` is not available in **datasets** until R 4.5.0, **basepenguins** itself does not require R >= 4.5.0, since it doesn't run any scripts that use the **datasets** versions of `penguins` and `penguins_raw`.
