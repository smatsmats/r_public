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

#for testing
install.packages("testthat")

# mapping things
install.packages("ggmap")
install.packages("maps")
install.packages("mapdata")
install.packages("usmap")

# map transformations
install.packages("rgeos")
# for ubuntu: apt-get install libgdal26
# for ubuntu: sudo apt-get install libgdal-dev
install.packages("rgdal")
install.packages("maptools")
