source("./covid19_functions.R")
source("./covid19_mygraphs.R")

library("yaml")

mode <- 'test'

c <- read_yaml("/Users/willey/r_public/covid19_config.yml")

if ( mode == 'test' ) {
  config <- c$test
} else if ( mode == 'prod' ) {
  config <- c$prod
}

# tmp goes in tmp, everything is tmp
setwd(config$tmpdir)

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
USE_JHU_POPS <- config$use_jhu_pops     # don't use populations directly from census
ENABLE_RED_BLUE <- config$enable_red_blue
USA_ALL <- config$usa_all
USE_GGPLOT <- config$use_ggplot       # versus base graphs
VERBOSE <- config$verbose
KEEP_FILES <- config$keep_files      # don't remove files after being pushed

cat("Code loaded\n")

ret <- onetime()
cat("OneTime loaded\n")

ret <- get_vax_data()
cat("Vax Data loaded\n")

ret <- newday()
cat("Newday loaded\n")

population <- get_population()
cat("Population loaded\n")

ret <- mygraphs()

ret <- prep_wide_data()
ret <- make_map_bases()
ret <- make_maps()

warnings()


