# mpm
(animal) Move Persistence Model

[![Project Status: WIP – Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/latest/wip.svg)](http://www.repostatus.org/#wip)
[![Build Status](https://travis-ci.org/ianjonsen/mpm.svg?branch=master)](https://travis-ci.org/ianjonsen/mpm)

This is a development package that fits a random walk with time-varying move persistence (autocorrelation) to single or multiple individual animal tracks. Move persistence (gamma_t) is a latent variable that is estimated as a simple random walk through time. The models are fitted using maximum likelihood estimation via the `TMB` ([Template Model Builder](https://github.com/kaskr/adcomp)) package and using C++. Whether the input data are for a single or multiple individuals, the model is fit to each individual track separately.

## Installation 
Currently, for testing and evaluation purposes only. You will need C++ compile tools. 

On PC's running Windows, ensure you have installed [Rtools](https://cran.r-project.org/bin/windows/Rtools/) 

On Mac's, ensure you have installed [Xcode](https://developer.apple.com/xcode/) and Xcode developer tools. If installation is needed, make sure you start Xcode after install to ensure final setup of developer tools is completed. Both Xcode and Xcode developer tools can be installed from the [Mac App Store](https://itunes.apple.com/au/app/xcode/id497799835?mt=12)

### From GitHub
`mpm` development version is available via:
```
devtools::install_github("ianjonsen/mpm")
```
