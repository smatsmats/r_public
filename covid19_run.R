library("aws.s3")
library("zoo")
library("scales")

#library("tidyverse")   # maybe we don't need the whole -verse
# tidyverse things
library("dplyr")
library("lubridate")
library("tidyr")
library("ggplot2")
library("stringr")

# mapping packages
library("ggmap")
library("maps")
library("mapdata")

library("remotes")
install_github("smatsmats/stevecovid19")

# tmp goes in tmp, everything is tmp
setwd("/tmp")

# don't push to amazon if we don't have the environment vars
if (Sys.getenv("AWS_DEFAULT_REGION") == "") {
  cat("No AWS creds in environment\n")
  cat("turning off AWS pushes\n")
  PUSH_TO_AMAZON <- FALSE
} else {
  if (Sys.getenv("BUCKET") == "") {
    cat("Must set environment var BUCKET\n")
    quit()
  }
  else {
    bucket <- Sys.getenv("BUCKET")
  }
  PUSH_TO_AMAZON <- TRUE
}

# Some flags
USE_JHU_POPS <- TRUE     # don't use populations directly from census
ENABLE_RED_BLUE <- FALSE
USA_ALL <- TRUE
USE_GGPLOT <- TRUE       # versus base graphs
VERBOSE <- FALSE
KEEP_FILES <- FALSE      # don't remove files after being pushed

cat("Code loaded\n")

ret <- stevecovid19::onetime()
cat("OneTime loaded\n")

ret <- stevecovid19::vax_data()
cat("Vax Data loaded\n")

ret <- stevecovid19::newday()
cat("Newday loaded\n")

population <- stevecovid19::get_population()
cat("Population loaded\n")

ret <- stevecovid19::doit()

ret <- stevecovid19::prep_wide_data()
ret <- stevecovid19::make_maps()

warnings()
