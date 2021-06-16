source("covid19_functions.R")

# tmp goes in tmp, everything is tmp
setwd("/tmp")

# Some flags
USE_JHU_POPS <- TRUE     # don't use populations directly from census
ENABLE_RED_BLUE <- FALSE
USA_ALL <- TRUE
USE_GGPLOT <- TRUE       # versus base graphs
PUSH_TO_AMAZON <- TRUE
VERBOSE <- FALSE
KEEP_FILES <- FALSE      # don't remove files after being pushed

cat("Code loaded\n")

onetime(version = version)
cat("OneTime loaded\n")

vax_data()
cat("Vax Data loaded\n")

newday(version = version)
cat("Newday loaded\n")

population <- get_population()
cat("Population loaded\n")

prod(version = version)

prep_wide_data()
ret <- make_maps()

warnings()
