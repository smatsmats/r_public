source("./covid19_functions.R")

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
KEEP_FILES <- TRUE      # don't remove files after being pushed

cat("Code loaded\n")

ret <- onetime()
cat("OneTime loaded\n")

ret <- vax_data()
cat("Vax Data loaded\n")

ret <- newday()
cat("Newday loaded\n")

population <- get_population()
cat("Population loaded\n")

#ret <- doit()

ret <- prep_wide_data()
ret <- make_maps()

warnings()
