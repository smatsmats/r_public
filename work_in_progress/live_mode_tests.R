# this probably won't work like this
# these were all of the inline tests that were written along with all of the associated functions
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
ENABLE_RED_BLUE <- FALSE
USA_ALL <- TRUE
VERBOSE <- FALSE
KEEP_FILES <- TRUE      # don't remove files after being pushed

cat("Code loaded\n")

ret <- onetime()
cat("OneTime loaded\n")

ret <- get_vax_data()
cat("Vax Data loaded\n")

ret <- newday()
cat("Newday loaded\n")

population <- get_population()
cat("Population loaded\n")


  print(paste("Washington", "Island", get_redblue("Washington", "Island")))
  print(paste(
    "Washington",
    "Columbia",
    get_redblue("Washington", "Columbia")
  ))
  print(paste(
    "Washington",
    "Garfield",
    get_redblue("Washington", "Garfield")
  ))
  print(paste("Alaska", "bumfuck", get_redblue("Alaska", "bumfuck")))
  print(paste(
    "Louisiana",
    "Terrebonne",
    get_redblue("Louisiana", "Terrebonne")
  ))
  print(paste(
    "District of Columbia",
    "dc",
    get_redblue("District of Columbia", "dc")
  ))

  b_ci_cases <-
    get_admin2("Maryland", "Baltimore City")
  make_plot(b_ci_cases, "bongo", "bingo")
  ic_cases <- get_admin2("Washington", "Island")

  make_plot(
    loc_txt = "Washington",
    "Island",
    df = ic_cases,
    daily_cases = TRUE,
    file_base = NULL
  )
  kc_cases <- get_admin2("Washington", "King")
  make_plot(
    loc_txt = "Washington",
    "King",
    df = kc_cases,
    daily_cases = TRUE,
    file_base = NULL
  )
  cc_cases <- get_admin2("Washington", "Columbia")
  ac_cases <- get_admin2("Washington", "Adams")
  wa_cases <- get_admin2("Washington", "Total")

  gc_cases <- get_admin2("Washington", "Garfield")
  tc_cases <- get_admin2("Louisiana", "Terrebonne")
  junk_new <- get_admin2("Virginia", "Lunenburg")

  wa_cases <<- get_admin1("Washington")
  make_plot(wa_cases, "bongo", cases = TRUE)
  dp_cases <<- get_admin1("Diamond Princess")
  ca_bc_cases <<- get_admin1("British Columbia", admin0 = "Canada")
  make_plot(ca_bc_cases, "bongo", daily_cases = TRUE)
  dc_cases <<- get_admin1("District of Columbia")
  write.csv(wa_cases, "wa_cases.csv")
  pr_cases <- get_admin1("Puerto Rico")
  a_cases <- get_admin2(state = "Washington", county = "Adams")
  wa_east_west(file_base = "waeastwest")

