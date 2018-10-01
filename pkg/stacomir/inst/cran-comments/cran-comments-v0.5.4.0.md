This updates the existing stacomiR package on CRAN.

# Notes

Latest message from CRAN states that the package is no longer building. The packages
 ‘gWidgetsRGtk2’ ‘RGtk2’ are no longer available on r-release-osx-x86_64 and the packages
  ‘stacomirtools’ ‘RODBC’ are not available on  r-oldrel-osx-x86_64. There is nothing I can do about that.
  
There is also a fix to an error : the maintainer of Hmisc Frank E Harrell has asked me to update several of my functions namely `roundPOSIXt` and `truncPOSIXt`. This is done in this release.

In addition this version adds :

* the removal of calls to the database named "test", and use of dplyr for internal calculations. 
* the update of tests
* the developpement of a vignette to present the package

#Testing Environments

My Windows machine.
Win Builder -- current and development.
R-forge.


