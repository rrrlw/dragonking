# dragonking: An R package implementing statistical tool for identification of dragon kings

[![Travis-CI Build Status](https://travis-ci.org/rrrlw/dragonking.svg?branch=master)](https://travis-ci.org/rrrlw/dragonking)
[![](http://www.r-pkg.org/badges/version/dragonking)](https://CRAN.R-project.org/package=dragonking)

## Purpose

The dragonking R package implements published statistical tests and test statistics for identification of dragon kings.

## Installation

To install the dragonking R package, run the following code in R:

```r
devtools::install_github("rrrlw/dragonking")
```

Note that you need the devtools package to install dragonking from GitHub (at least until it is accepted into CRAN).
To install devtools and dragonking, run the following code in R:

```r
# install and load devtools
install.packages("devtools")
library("devtools")

# install dragonking
install_github("rrrlw/dragonking")
```

## Example use

The following code demonstrates the use of dragonking with a mock dataset.
For examples of more detailed and varied use, please see the documentation and available vignettes.

```r
# load dragonking package
library("dragonking")

# create mock dataset with no dragon kings (DKs)
set.none <- rexp(100)

# create mock dataset with 5 DKs
set.dk <- c(rexp(95),
            15.1, 14.9, 15.0, 14.95, 15.09) # DKs

# run DK test on dataset without DKs (we are looking for 2 DKs)
results.none <- dk_test(set.none, r = 2)
print(results.none["Test Statistic"]) # retrieve test stat
print(results.none["p-value"])        # retrieve p-value

# run DK test on dataset with DKs (we are looking for 3 DKs)
dk_test(set.dk, r = 3)                # print out test stat and p-value
```

Please report any bugs, comments, suggestions, etc. regarding dragonking on the [GitHub issues page](https://github.com/rrrlw/dragonking/issues).
