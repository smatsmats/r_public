r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

# for sure these
install.packages("aws.s3")
install.packages("zoo")
install.packages("scales")

#install.packages("tidyverse")   # maybe we don't need the whole -verse
# todyverse things
install.packages("dplyr")
install.packages("lubridate")
install.packages("tidyr")
install.packages("ggplot2")
install.packages("stringr")

# for github actions tests
install.packages("testthat")

# mapping things
install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")

# map transformations
# for ubuntu: sudo apt-get install libgeos++-dev libgeos-3.8.0 libgeos-c1v5 libgeos-dev
install.packages("rgeos")
# for ubuntu: sudo apt-get install libgdal26 libgdal-dev
install.packages("rgdal")
install.packages("maptools")
install.packages("mapproj")
install.packages("gpclib")

# this needs to be explicitly updated
install.packages("Rcpp")
