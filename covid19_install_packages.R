r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

# for sure these
library("aws.s3")
library("zoo")
library("scales")

#library("tidyverse")   # maybe we don't need the whole -verse
# todyverse things
library("dplyr")
library("lubridate")
library("tidyr")
library("ggplot2")
library("stringr")
