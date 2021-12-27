# for sure these
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

# for map transformations
library("rgeos")
library("rgdal")
library("maptools")
library("gpclib")
gpclibPermit()

# use tigris for pullng cb shapfiles
library("tigris")
options(tigris_use_cache = TRUE)

# design decisions
# - don't combine plotting and making df"s
# - except where it makes sense, i.e. build all states
#
#

# Some flags
ENABLE_RED_BLUE <- FALSE
USA_ALL <- TRUE
VERBOSE <- TRUE
KEEP_FILES <- FALSE      # don't remove files after being pushed

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

# constants
plot_start_date <- "2020/3/1"  # not the earliest case in WA but ...
plot_end_date <-
  format(Sys.Date(), "%Y/%m/%d") # gets reset in newday function
cumulative_c19_cases_txt <- "Cumulative COVID-19 Cases"
daily_c19_cases_txt <- "Daily COVID-19 Cases"
fourteen_day_avrg_txt <- "14day Average"
fourteen_day_sum_txt <- "14day Sum"
redblue_txt <- "Red / Blue"
hundy_txt <- "per 100,000"
main_daily_cases_hundy_txt <- paste(daily_c19_cases_txt, hundy_txt)
main_daily_cases_hundy_14d_avrg_txt <-
  paste(daily_c19_cases_txt, hundy_txt, fourteen_day_avrg_txt)
main_daily_cases_hundy_14d_sum_txt <-
  paste(daily_c19_cases_txt, hundy_txt, fourteen_day_sum_txt)
main_14day_trend_txt <-
  "14day Trend (of cases per 100,000, 14day average)"
main_cases_hundy_txt <- paste(cumulative_c19_cases_txt, hundy_txt)
ylab_cases_txt <- "Cases"
ylab_daily_cases_txt <- "Daily Cases"
ylab_cases_hundy_txt <- "Cases / 100,000 Population"
ylab_daily_cases_hundy_txt <- "Daily Cases / 100,000 Population"
plot_file_width <- (480 * 2)
plot_file_height <- (310 * 2)

file_to_bucket <- function(file, unlink_after = TRUE) {
  if (PUSH_TO_AMAZON) {
    file <- str_replace_all(file, " ", "_")
    put_object(
      file = file,
      bucket = bucket,
      multipart = FALSE,
      acl = "public-read",
      headers = list(),
      verbose = TRUE,
      show_progress = FALSE
    )
  }
  if (unlink_after & !KEEP_FILES) {
    unlink(file)
  }

  return(0)
}

# reads in population data and does some formating
get_population <- function() {
  uid_iso_fips_lookup <-
    read.csv(
      "https://github.com/CSSEGISandData/COVID-19/blob/master/csse_covid_19_data/UID_ISO_FIPS_LookUp_Table.csv?raw=true"
    )
  uid_iso_fips_lookup[uid_iso_fips_lookup$Province_State %in% "Guam", ]$Combined_Key <- "Guam, Guam, US"
  # we don't get disaggregated covid19 data for usvi so treat all islands the same
  uid_iso_fips_lookup[uid_iso_fips_lookup$Province_State %in% "Virgin Islands", ]$Combined_Key <- "Virgin Islands, Virgin Islands, US"

  uid_iso_fips_lookup <- mash_combined_key(uid_iso_fips_lookup)
  population <- uid_iso_fips_lookup

  # put into global view
  uid_iso_fips_lookup <<- uid_iso_fips_lookup

  return(population)
}

pop_format <- function(pop) {
  return(format(pop * 100000, big.mark = ","))
}

state_pop_txt <- function(s, df) {
  return(paste(s, " State (pop=", pop_format(df$pop[1]), ")", sep = ""))
}

onetime <- function() {
  # some datasets
  # 2016 presidential election results, by county
  if (ENABLE_RED_BLUE) {
    prez_2016 <<-
      read.csv(
        "https://raw.githubusercontent.com/mkearney/presidential_election_county_results_2016/master/pres.elect16.results.dec9.csv"
      )
    prez_2020 <<-
      read.csv(
        "https://github.com/kjhealy/us_elections_2020_csv/raw/master/results_current.csv"
      )
  }

  # info on wa counties
  wa_counties <<-
    read.csv(
      "https://docs.google.com/uc?id=19OOGc3UmvN77oqPP9JeRKFbGSzuxzxRQ&export=download"
    )

  return(0)
}

newday <- function() {
  # reset end date
  plot_end_date <<- format(Sys.Date(), "%Y/%m/%d")

  # comes in wide
  usa_confirmed <<-
    read.csv(
      "https://github.com/CSSEGISandData//COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv?raw=true"
    )
  global_confirmed <<-
    read.csv(
      "https://github.com/CSSEGISandData//COVID-19/blob/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv?raw=true"
    )

  # clean-up global-confirmed
  global_confirmed[c("Lat", "Long")] <- NULL
  # make long
  global_confirmed_t <<- pivot_longer(
    global_confirmed,
    cols = starts_with("X"),
    names_to = "dates",
    values_to = "cases",
    values_drop_na = FALSE
  )
  global_confirmed_t$dates <<-
    as.Date(global_confirmed_t$dates,  format = "X%m.%d.%y")
  global_confirmed_t$country <<-
    tolower(global_confirmed_t$Country.Region)
  country_t <<-
    global_confirmed_t %>% 
    group_by(Country.Region, dates) %>%
    summarise(cases = sum(cases), .groups = 'drop')
  
  uc <- usa_confirmed

  # remove soem junk
  uc[c(
    "UID",
    "iso2",
    "iso3",
    "code3",
    "FIPS",
    "Country_Region",
    "Lat",
    "Long_",
    "Combined_Key"
  )] <- NULL
  usa_confirmed_t <<-
    pivot_longer(
      uc,
      cols = starts_with("X"),
      names_to = "dates",
      values_to = "cases",
      values_drop_na = FALSE
    )
  usa_confirmed_t$dates <<-
    as.Date(substr(usa_confirmed_t$dates, 2, 20),
            format = "%m.%d.%y")
  usa_confirmed_t$state_ <<- tolower(usa_confirmed_t$Province_State)
  usa_states <<-
    usa_confirmed_t %>% 
    group_by(Province_State, dates) %>%
    summarise(cases = sum(cases), .groups = 'drop')

  # pivot back wide to get the nice wide version
  us_states_wide_raw <<- pivot_wider(
    usa_states,
    id_cols = Province_State,
    names_from = dates,
    values_from = cases
  )

  return(0)
}

get_vax_data <- function() {
  vax_global_wide_raw <<-
    read.csv(
      "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/global_data/time_series_covid19_vaccine_doses_admin_global.csv"
    )
  vax_us_wide_raw <<-
    read.csv(
      "https://raw.githubusercontent.com/govex/COVID-19/master/data_tables/vaccine_data/us_data/time_series/time_series_covid19_vaccine_doses_admin_US.csv"
    )
  vax_global_wide <- mash_combined_key(vax_global_wide_raw)
  vax_us_wide <- mash_combined_key(vax_us_wide_raw)
  latest_g <- dim(vax_global_wide_raw)[2] - 1
  latest_u <- dim(vax_us_wide_raw)[2] - 1

  vax_global_wide <<-
    summarize_vax_wide_data(vax_global_wide, latest_g)
  vax_us_wide <<- summarize_vax_wide_data(vax_us_wide, latest_u)

  write.csv(vax_us_wide, "vax_us_wide.csv")

  return(0)

}

get_pop <- function(country = "US",
                    admin1 = "",
                    admin2 = "") {
  # legacy
  if (is.null(admin2)) {
    admin2 <- ""
  }
  # legacy
  if (admin2 == "Total") {
    admin2 <- ""
  }
  if (is.null(country)) {
    country <- "US"
  }
  if (is.null(admin1)) {
    admin1 <- ""
  }

  if (VERBOSE) {
    cat(
      "get_pop: going to get pop for: country:",
      country,
      "| admin1 / state:",
      admin1,
      "| admin2 / county:",
      admin2,
      "\n"
    )
  }

  row <- subset(
    uid_iso_fips_lookup,
    grepl(country, Country_Region, ignore.case = TRUE) &
      grepl(admin1, Province_State, ignore.case = TRUE) &
      grepl(admin2, Admin2, ignore.case = TRUE)
  )

  hundy = row$Population[1] / 100000.0

  if (is.na(hundy)) {
    hundy <- 0
  }

  return(hundy)
}


# cleans-up some goofy county names - needed this with US Census pops and
# election results
get_full_county_name <- function(state = "not alaska",  county) {
  if (state == "Alaska") {
    full_county_name <- county
  } else if (state == "District of Columbia") {
    full_county_name <- "District of Columbia"
  } else if (state == "Guam") {
    full_county_name <- "Guam"
  } else if (state == "Virgin Islands") {
    full_county_name <- "Virgin Islands"
  } else if (state == "Louisiana") {
    full_county_name = paste(county, "Parish")
  } else {
    mystate <- state
    trans <-
      subset(county_transformations,
             state == mystate & county_in == county)
    if (nrow(trans) == 1) {
      full_county_name <- trans$county_out
    } else {
      full_county_name = paste(county, "County")
    }
  }
  return(full_county_name)
}

# "place" class
# admin1 e.g. us state, cnd province
# admin2 e.g. us county
place <- function(admin_level,
                  country = "US",
                  admin1,
                  admin2) {

  ok_levels <- c("country", "admin1", "admin2")
  if (!admin_level %in% ok_levels) {
    stop("need a correct administrative level")
  } 
  obj <- list(country = country,
              admin1 = admin1,
              admin2 = admin2,
              level = admin_level, 
              txt = txt, 
              df = df)
  attr(obj, "class") <- "place"
  return(obj)
}



# old function where we only returned winning prez candidate
get_2016_prez <- function(state, county) {
  mycounty <- county
  if (state == "Alaska") {
    hill_trump <- subset(prez_2016, state.name == state)$lead[1]
  } else if (state == "District of Columbia") {
    hill_trump <- subset(prez_2016, county == county)$lead[1]
  } else {
    hill_trump <-
      subset(prez_2016, state.name == state &
               county == mycounty)$lead[1]
  }
  if (is.na(hill_trump)) {
    return("unkown")
  } else {
    return(hill_trump)
  }
}

get_redblue2016 <- function(state, county) {
  red_cand <- "Donald Trump"
  blue_cand <- "Hillary Clinton"
  if (state == "Louisiana") {
    mycounty <- paste(county, "Parish")
  } else {
    mycounty <- get_full_county_name(state, county)
  }

  if (state == "District of Columbia") {
    red_pct_t <- subset(prez_2016, county == state &
                          cand == red_cand)$pct[1]
    blue_pct_t <- subset(prez_2016, county == state &
                           cand == blue_cand)$pct[1]

  } else if (state == "Alaska" || state == "District of Columbia") {
    red_pct_t <- subset(prez_2016, state.name == state &
                          cand == red_cand)$pct[1]
    blue_pct_t <- subset(prez_2016, state.name == state &
                           cand == blue_cand)$pct[1]

  } else {
    red_pct_t <- subset(prez_2016,
                        state.name == state &
                          county == mycounty &
                          cand == red_cand)$pct[1]
    blue_pct_t <- subset(prez_2016,
                         state.name == state &
                           county == mycounty &
                           cand == blue_cand)$pct[1]
  }
  #  print(paste(red_pct_t, blue_pct_t, ","))
  red_pct <- red_pct_t / (red_pct_t + blue_pct_t)
  blue_pct <- blue_pct_t / (red_pct_t + blue_pct_t)
  return(c(red_pct, blue_pct))
}

get_redblue <-  function(state, county) {
  get_redblue2016(state, county)
}

# takes any data frame (should be an object)
# and makes a number of plots
# this should be a method to places class
make_plot <- function(df,
                      loc_txt,
                      main_txt = NULL,
                      # plot types:
                      cases_per_hundy = TRUE,
                      cases = TRUE,
                      daily_cases = FALSE,
                      file_base = NULL) {
  # maybe bail
  if (is.null(df)) {
    return(NULL)
  }

  # bail if we have no population
  if (df[1, ]$pop == 0) {
    return(NULL)
  }

  # maybe override the global cumulative_c19_cases_txt
  if (!is.null(main_txt)) {
    cumulative_c19_cases_txt = main_txt
  }

  if (cases_per_hundy) {
    if (!is.null(file_base)) {
      f <- paste(file_base, "_cases_per_hundy", ".jpg", sep = "")
      jpeg(filename = f,
           width = plot_file_width,
           height = plot_file_height)
    }

    p <- ggplot(data = df, aes(x = dates, y = cases_per_hundy)) +
      geom_line(colour = "purple", na.rm = FALSE) +
      labs(
        title = paste(loc_txt, cumulative_c19_cases_txt, hundy_txt),
        subtitle = paste("created", format(Sys.time(), "%m/%d/%Y %H:%M:%S")),
        x = "Dates",
        y = ylab_cases_hundy_txt
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        #            panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)
      )
    print(p)

    if (!is.null(file_base)) {
      dev.off()
    }

  }

  if (cases) {
    if (!is.null(file_base)) {
      f <- paste(file_base, "_cases", ".jpg", sep = "")
      jpeg(filename = f,
           width = plot_file_width,
           height = plot_file_height)
    }

    p <- ggplot(data = df, aes(x = dates, y = cases)) +
      geom_line(colour = "purple", na.rm = FALSE) +
      labs(
        title = paste(loc_txt, cumulative_c19_cases_txt),
        subtitle = paste("created", format(Sys.time(), "%m/%d/%Y %H:%M:%S")),
        x = "Dates",
        y = ylab_cases_txt
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        #            panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5)
      )

    print(p)

    if (!is.null(file_base)) {
      dev.off()
    }
  } # if cases

  if (daily_cases) {
    if (!is.null(file_base)) {
      f <- paste(file_base, "_daily_cases", ".jpg", sep = "")
      jpeg(filename = f,
           width = plot_file_width,
           height = plot_file_height)
    }

    p <- ggplot(data = df, aes(dates)) +
      geom_line(
        aes(y = daily_cases_per_hundy, colour = "Daily"),
        size = 0.3,
        na.rm = FALSE
      ) +
      scale_color_manual(values = c("14 Day Average / Sum" = "red",
                                    "Daily" = "mediumpurple1")) +
      labs(
        title = paste(loc_txt, main_daily_cases_hundy_txt),
        subtitle = paste("created", format(Sys.time(), "%m/%d/%Y %H:%M:%S"))
      ) +
      scale_x_date(name = "Dates") +
      scale_y_continuous(
        name =  ylab_daily_cases_hundy_txt,
        limits = c(0, max(df$daily_cases_per_hundy)),
        sec.axis = sec_axis(trans =  ~ . * 14, name = "14 Day Sum / 100,000 Population")
      ) +
      theme_bw() +
      theme(
        panel.grid.minor = element_blank(),
        #            panel.grid.major = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        plot.caption = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = c(0.35, 0.87),
        legend.background = element_rect(
          linetype = "solid",
          size = 0.2,
          colour = "black"
        )
      ) +
      geom_line(aes(y = daily_cases_per_hundy_avrg14d,
                    colour = "14 Day Average / Sum"),
                na.rm = FALSE)

    print(p)

    if (!is.null(file_base)) {
      dev.off()
    }
  } # if daily cases

  # just return something not NULL
  return(p)
}

# flesh out data frame with summary columns
# build the data so it can be graphed or dumped out in a csv
build_cols <- function(df, pop) {
  if (pop > 0) {
    df$pop <- pop
    df$cases_per_hundy <- df$cases / pop
    # ifelse(df$cases_per_hundy < 0, 0, df$cases_per_hundy)
  } else {
    df$pop <- 0
    df$cases_per_hundy <- 0
  }

  # get the daily deltas
  df$daily_cases <- ave(
    df$cases,
    FUN = function(x)
      c(0, diff(x))
  )
  df$daily_cases_per_hundy <- df$daily_cases / pop
  #make na"s zero (first daily starts as an NA)
  #  df$daily_cases[is.na(df$daily_cases)] <- 0

  # rolling averages
  df$daily_cases_avrg7d <-
    zoo::rollmean(df$daily_cases,
                  k = 7,
                  fill = NA,
                  align = "right")
  df$daily_cases_avrg7d[is.na(df$daily_cases_avrg7d)] <- 0
  df$daily_cases_avrg14d <-
    zoo::rollmean(df$daily_cases,
                  k = 14,
                  fill = NA,
                  align = "right")
  df$daily_cases_avrg14d[is.na(df$daily_cases_avrg14d)] <- 0

  df$daily_cases_sum7d <-
    zoo::rollsum(df$daily_cases,
                 k = 7,
                 fill = NA,
                 align = "right")
  df$daily_cases_sum7d[is.na(df$daily_cases_avrg7d)] <- 0
  df$daily_cases_sum14d <-
    zoo::rollsum(df$daily_cases,
                 k = 14,
                 fill = NA,
                 align = "right")
  df$daily_cases_sum14d[is.na(df$daily_cases_avrg14d)] <- 0

  if (pop > 0) {
    df$daily_cases_per_hundy_avrg7d <- df$daily_cases_avrg7d / pop
    df$daily_cases_per_hundy_avrg14d <- df$daily_cases_avrg14d / pop
    df$daily_cases_per_hundy_sum7d <- df$daily_cases_sum7d / pop
    df$daily_cases_per_hundy_sum14d <- df$daily_cases_sum14d / pop
    #ifelse(df$cases_per_hundy < 0, 0, df$cases_per_hundy)
  } else {
    df$daily_cases_per_hundy_avrg7d <- 0
    df$daily_cases_per_hundy_avrg14d <- 0
    df$daily_cases_per_hundy_sum7d <- 0
    df$daily_cases_per_hundy_sum14d <- 0
  }

  if (ENABLE_RED_BLUE) {
    red_blue_pcts <- get_redblue(state, county)

    df$red_cases <- df$cases * red_blue_pcts[1]
    df$red_daily_cases <-
      df$daily_cases * red_blue_pcts[1]

    df$red_daily_cases_avrg7d <-
      df$daily_cases_avrg7d * red_blue_pcts[1]
    df$red_daily_cases_avrg14d <-
      df$daily_cases_avrg14d * red_blue_pcts[1]
    df$red_daily_cases_per_hundy_avrg7d <-
      df$daily_cases_per_hundy_avrg7d * red_blue_pcts[1]
    df$red_daily_cases_per_hundy_avrg14d <-
      df$daily_cases_per_hundy_avrg14d * red_blue_pcts[1]

    df$red_pop <- df$pop * red_blue_pcts[1]
    df$red_cases_per_hundy <- df$cases_per_hundy * red_blue_pcts[1]
    ifelse(df$red_cases_per_hundy < 0, 0, df$red_cases_per_hundy)

    df$blue_cases <- df$cases * red_blue_pcts[2]
    df$blue_daily_cases <-
      df$daily_cases * red_blue_pcts[2]

    df$blue_daily_cases_avrg7d <-
      df$daily_cases_avrg7d * red_blue_pcts[1]
    df$blue_daily_cases_avrg14d <-
      df$daily_cases_avrg14d * red_blue_pcts[1]
    df$blue_daily_cases_per_hundy_avrg7d <-
      df$daily_cases_per_hundy_avrg7d * red_blue_pcts[1]
    df$blue_daily_cases_per_hundy_avrg14d <-
      df$daily_cases_per_hundy_avrg14d * red_blue_pcts[1]

    df$blue_pop <- df$pop * red_blue_pcts[2]
    df$blue_cases_per_hundy <- df$cases_per_hundy * red_blue_pcts[2]
    ifelse(df$blue_cases_per_hundy < 0, 0, df$blue_cases_per_hundy)
  } #enable red blue

  return(df)
}

# selects a county
# county = admin level2
# for now this probably doesn't work for non US divisions
get_admin2 <- function(state_in, 
                       county_in,
                       country_in = "US") {
  cat("building admin2: country:", 
      country_in,
      "| admin1 / state:",
      state_in,
      "| admin2 / county:",
      county_in,
      "\n")

  if (county_in == "Total") {
    county_cases_t <- as.data.frame(subset(usa_states,
                                           Province_State == state_in))
  }
  else {
    # convert into a data frame instead of a tuple.  tuple has big performance impacts down the road
    county_cases_t <-
      as.data.frame(subset(usa_confirmed_t, Admin2 == county_in &
                             Province_State == state_in))
  }

  pop <- get_pop(admin1 = state_in, 
                 admin2 = county_in, 
                 country = country_in)

  df <- build_cols(county_cases_t, pop)

  return(df)

}

write_csv_file <- function(df, file_base) {
  write.csv(df, paste(file_base, ".csv", sep = ""))
}

# maybe rip this all out?
make_redblue_plot <- function(df,
                              loc_txt,
                              main_txt = NULL,
                              cases_per_hundy = TRUE,
                              cases = TRUE,
                              file_base = NULL) {
  # maybe bail
  if (is.null(df)) {
    return(NULL)
  }

  # maybe override the global cumulative_c19_cases_txt
  if (!is.null(main_txt)) {
    cumulative_c19_cases_txt = main_txt
  }

  max_red <- 0
  max_blue <- 0
  try(max_red <- max(df$red_cases), silent = TRUE)
  try(max_blue <- max(df$blue_cases), silent = TRUE)
  max_y <- ifelse(max_red > max_blue, max_red, max_blue)

  max_red_cases_per_hundy <- 0
  max_blue_cases_per_hundy <- 0
  try(max_red_cases_per_hundy <-
        max(df$red_cases_per_hundy, 1),
      silent = TRUE)
  try(max_blue_cases_per_hundy <-
        max(df$blue_cases_per_hundy, 1),
      silent = TRUE)
  max_y_cases_per_hundy <-
    ifelse(
      max_red_cases_per_hundy > max_blue_cases_per_hundy,
      max_red_cases_per_hundy,
      max_blue_cases_per_hundy
    )
  #  print(max(df$red_per_hund, 1))
  #  print(paste(max_red_cases_per_hundy, max_blue_cases_per_hundy,max_y_cases_per_hundy, sep=", "))
  if (cases_per_hundy) {
    if (!is.null(file_base)) {
      f <- paste(file_base, "_cases_per_hundy", ".jpg", sep = "")
      jpeg(filename = f,
           width = plot_file_width,
           height = plot_file_height)
    }
    plot(
      df$dates,
      df$red_cases_per_hundy,
      main = paste(loc_txt, redblue_txt, cumulative_c19_cases_txt, hundy_txt),
      ylab = ylab_cases_hundy_txt,
      xlab = "Dates",
      type = "l",
      col = "red",
      ylim = c(0, max_y_cases_per_hundy),
      xlim = as.Date(c(plot_start_date, plot_end_date))
    )
    if (max_blue_cases_per_hundy > 0) {
      lines(df$dates, df$blue_cases_per_hundy, col = "blue")
    }
    if (!is.null(file_base)) {
      dev.off()
    }

  }

  if (cases) {
    if (!is.null(file_base)) {
      f <- paste(file_base, "_cases", ".jpg", sep = "")
      jpeg(filename = f,
           width = plot_file_width,
           height = plot_file_height)
      #      print("on 2")
    }
    plot(
      df$dates,
      df$red_cases,
      main = paste(loc_txt, redblue_txt, cumulative_c19_cases_txt),
      ylab = ylab_cases_txt,
      xlab = "Dates",
      type = "l",
      col = "red",
      ylim = c(0, max_y),
      xlim = as.Date(c(plot_start_date, plot_end_date))
    )
    if (max_blue > 0) {
      lines(df$dates, df$blue_cases, col = "blue")
    }
    if (!is.null(file_base)) {
      dev.off()
      #      print("off 2")
    }

  }

}

# use this to combine places
# as an example we aggreagate east and western Washington St
# again, it should be a class method
# 
# This is a little table of data in class object and how they're
# combined
# 
# var                           source          aggregate function
#                           
# pop                           get_pop         addition
# cases                         something       addition
# daily_cases                   something       addition
# cases_per_hundy               division        division
# daily_cases_per_hundy         division        division
# daily_cases_avgr7             rollmean        rollmean
# daily_cases_avgr7             rollmean        rollmean
# daily_cases_per_hundy_avrg7   division        division
# daily_cases_per_hundy_avrg14  division        division
#
aggregate_dfs <- function(in_df, new_df) {

  for (r in 1:nrow(in_df)) {
    in_df[r, "pop"] <- in_df[r, "pop"] + new_df[r, "pop"]

    in_df[r, "cases"] <- in_df[r, "cases"] + new_df[r, "cases"]
    in_df[r, "cases_per_hundy"] <-
      in_df[r, "cases"] / in_df[r, "pop"]
    in_df[r, "daily_cases"] <- in_df[r, "daily_cases"] +
      new_df[r, "daily_cases"]
    in_df[r, "daily_cases_per_hundy"] <-
      in_df[r, "daily_cases"] / in_df[r, "pop"]

    if (r < 7) {
      in_df[r, "daily_cases_avrg7d"] <- 0
      in_df[r, "daily_cases_per_hundy_avrg7d"] <- 0
      in_df[r, "daily_cases_sum7d"] <- 0
      in_df[r, "daily_cases_per_hundy_sum7d"] <- 0
    }
    else {
      in_df[r, "daily_cases_avrg7d"] <-
        zoo::rollmean(in_df[(r - 6):r, "daily_cases"],
                      k = 7,
                      fill = NA,
                      align = "left")[1]
      in_df[r, "daily_cases_per_hundy_avrg7d"] <-
        in_df[r, "daily_cases_avrg7d"] / in_df[r, "pop"]
      in_df[r, "daily_cases_sum7d"] <-
        zoo::rollsum(in_df[(r - 6):r, "daily_cases"],
                     k = 7,
                     fill = NA,
                     align = "left")[1]
      in_df[r, "daily_cases_per_hundy_sum7d"] <-
        in_df[r, "daily_cases_sum7d"] / in_df[r, "pop"]
    }
    if (r < 14) {
      in_df[r, "daily_cases_avrg14d"] <- 0
      in_df[r, "daily_cases_per_hundy_avrg14d"] <- 0
      in_df[r, "daily_cases_sum14d"] <- 0
      in_df[r, "daily_cases_per_hundy_sum14d"] <- 0
    }
    else {
      in_df[r, "daily_cases_avrg14d"] <-
        zoo::rollmean(in_df[(r - 13):r, "daily_cases"],
                      k = 14,
                      fill = NA,
                      align = "left")[1]
      in_df[r, "daily_cases_per_hundy_avrg14d"] <-
        in_df[r, "daily_cases_avrg14d"] / in_df[r, "pop"]
      in_df[r, "daily_cases_sum14d"] <-
        zoo::rollsum(in_df[(r - 13):r, "daily_cases"],
                     k = 14,
                     fill = NA,
                     align = "left")[1]
      in_df[r, "daily_cases_per_hundy_sum14d"] <-
        in_df[r, "daily_cases_sum14d"] / in_df[r, "pop"]
    }

    if (ENABLE_RED_BLUE) {
      in_df[r, "red_cases"] <-
        in_df[r, "red_cases"] + new_df[r, "red_cases"]
      #      print(r)
      if (r < 8) {
        in_df[r, "red_daily_cases_avrg7d"] <- 0
        in_df[r, "red_daily_cases_per_hundy_avrg7d"] <- 0
      }
      else {
      }
      if (r < 15) {
        in_df[r, "red_daily_cases_avrg14d"] <- 0
        in_df[r, "red_daily_cases_per_hundy_avrg14d"] <- 0
      }
      else {
        #        in_df[r, "red_daily_cases_avrg14d"] <- zoo::rollmean(in_df[(r-13):r, "daily_cases"], k = 14, fill = NA, align="left")[0]
        #        in_df[r, "red_daily_cases_per_hundy_avrg14d"] <- in_df[r, "red_daily_cases_avrg14d"] / in_df[r, "pop"]
      }

      in_df[r, "red_daily_cases_per_hundy_avrg7d"] <-
        in_df[r, "red_daily_cases_per_hundy_avrg7d"] +
        new_df[r, "red_daily_cases_per_hundy_avrg7d"]
      in_df[r, "red_daily_cases_per_hundy_avrg14d"] <-
        in_df[r, "red_daily_cases_per_hundy_avrg14d"] +
        new_df[r, "red_daily_cases_per_hundy_avrg14d"]


      in_df[r, "red_pop"] <-
        in_df[r, "red_pop"] + new_df[r, "red_pop"]
      in_df[r, "red_cases_per_hundy"] <-
        in_df[r, "red_cases_per_hundy"] +
        new_df[r, "red_cases_per_hundy"]
      in_df[r, "blue_cases"] <-
        in_df[r, "blue_cases"] + new_df[r, "blue_cases"]

      in_df[r, "blue_daily_cases_avrg7d"] <-
        in_df[r, "blue_daily_cases_avrg7d"] +
        new_df[r, "blue_daily_cases_avrg7d"]
      in_df[r, "blue_daily_cases_avrg14d"] <-
        in_df[r, "blue_daily_cases_avrg14d"] +
        new_df[r, "blue_daily_cases_avrg14d"]
      in_df[r, "blue_daily_cases_per_hundy_avrg7d"] <-
        in_df[r, "blue_daily_cases_per_hundy_avrg7d"] +
        new_df[r, "blue_daily_cases_per_hundy_avrg7d"]
      in_df[r, "blue_daily_cases_per_hundy_avrg14d"] <-
        in_df[r, "blue_daily_cases_per_hundy_avrg14d"] +
        new_df[r, "blue_daily_cases_per_hundy_avrg14d"]

      in_df[r, "blue_pop"] <-
        in_df[r, "blue_pop"] + new_df[r, "blue_pop"]
      in_df[r, "blue_cases_per_hundy"] <-
        in_df[r, "blue_cases_per_hundy"] +
        new_df[r, "blue_cases_per_hundy"]
    }
  } # enable red blue

  return(in_df)

}


get_admin1 <- function(admin1_in,
                       country_in = "US") {
  cat("building admin1: country:", 
      country_in,
      "| admin1 / state:",
      admin1_in,
      "\n")

  if (country_in == "US") {
    state_cases_t <-
      as.data.frame(subset(
        usa_states,
        grepl(admin1_in, Province_State, ignore.case = TRUE)
      ))
  }
  else  {
    state_cases_t <- as.data.frame(subset(
      global_confirmed_t,
      grepl(country_in,
            Country.Region,
            ignore.case = TRUE) &
        grepl(admin1_in,
              Province.State,
              ignore.case = TRUE)
    ))
  }

  pop <- get_pop(admin1 = admin1_in, country = country_in)

  df <- build_cols(state_cases_t, pop)

  return(df)

}

get_country <- function(country_in) {
  cat("in get_country:", country_in)

  # convert into a data frame instead of a tuple.
  # tuple has big performance impacts down the road
  country_cases_t <- as.data.frame(subset(
    country_t,
    grepl(country_in, Country.Region, ignore.case = TRUE)
  ))

  pop <- get_pop(country = country_in)

  df <- build_cols(country_cases_t, pop)

  return(df)

}

make_county_string <- function(state, county) {
  s <- tolower(paste(state, county))
  s <- str_replace_all(tolower(s), " ", "_")
  return(s)
}

build_all_counties <- function(state = "Washington",
                               ref_county = "King",
                               keep_dfs = FALSE,
                               write_dfs = FALSE,
                               plot_ref_and = FALSE,
                               plot_daily_cases = FALSE,
                               plot_state_cases_per_hundy = FALSE) {
  if (plot_ref_and) {
    ref_cases <- get_admin2(state = state, county = ref_county)
    ref_maxy = max(ref_cases$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
    ref_txt <-
      paste(ref_county, " County, ", state, " (pop=", pop_format(ref_cases$pop[1]), ")", sep = "")
  }

  state_df <-
    as.data.frame(subset(usa_confirmed, Province_State == state))

  counties <- unique(sort(state_df$Admin2))

  for (county in counties) {
    if (str_detect(county, "Out of ") |
        county == "" |
        county == "unkown" |
        county == "Unassigned") {
      next
    }

    if (VERBOSE) {
      cat(county, " County, ", state, " state")
    }
    new_df <- get_admin2(state = state, county = county)

    if (is.null(new_df)) {
      next
    }

    s_c = make_county_string(state, county)
    file_base <- s_c
    if (write_dfs) {
      filename <- paste(file_base, "csv", sep = ".")
      write.csv(new_df, filename)
      file_to_bucket(filename)
    }

    if (keep_dfs) {
      st_string <- s_c
      new_df_name <- paste(s_c, "_df", sep = "")
      assign(new_df_name, new_df, envir = .GlobalEnv)

      # make the text name for graphs
      txt_arg <- paste(s_c, "_txt", sep = "")
      txt_value <- paste(county,
                         " County, ",
                         state,
                         " State (pop=",
                         pop_format(new_df$pop[1]),
                         ")",
                         sep = "")
      assign(txt_arg, txt_value, envir = .GlobalEnv)
    }

    # really no point if there isn"t anyone there
    if (new_df[1, ]$pop == 0) {
      next
    }

    if (plot_daily_cases) {
      ret <- make_plot(
        df = new_df,
        loc_txt = paste(county, " County, ", state),
        daily_cases = TRUE,
        file_base = file_base
      )
      if (!is.null(ret)) {
        filename <- paste(file_base, "daily_cases.jpg", sep = "_")
        file_to_bucket(filename)
      }
    }

    if (plot_state_cases_per_hundy) {
      ret <- make_plot(
        new_df,
        loc_txt = paste(county, " County, ", state),
        cases_per_hundy = TRUE,
        file_base = file_base
      )
      if (!is.null(ret)) {
        filename <- paste(file_base, "cases_per_hundy.jpg", sep = "_")
        file_to_bucket(filename)
      }
    }

    if (plot_ref_and) {
      # multiple counties 14 day
      filename <- paste(ref_county, "_and_", county, ".jpg", sep = "")
      filename <- tolower(filename)
      filename <- str_replace_all(filename, " ", "_")

      jpeg(filename = filename,
           width = plot_file_width,
           height = plot_file_height)

      new_df$ref_cases <- ref_cases$cases
      new_df$ref_daily_cases <- ref_cases$daily_cases
      new_df$ref_daily_cases_per_hundy_avrg14d <-
        ref_cases$daily_cases_per_hundy_avrg14d
      s_txt <- txt_value

      # figure the maxy for ref_and plot
      new_df_maxy <- max(new_df$daily_cases_per_hundy_avrg14d)
      maxy <- ifelse(new_df_maxy > ref_maxy, new_df_maxy, ref_maxy)

      p <- ggplot(data = new_df, aes(dates)) +
        geom_line(aes(y = daily_cases_per_hundy_avrg14d,
                      colour = s_txt)) +
        geom_line(aes(y = ref_daily_cases_per_hundy_avrg14d,
                      colour = ref_txt)) +
        scale_color_manual(values = c("black", "darkgreen")) +
        ylim(0, maxy) +
        labs(
          title = "Daily Cases per 100,000, 14day Average",
          subtitle = paste("created", format(Sys.time(), "%m/%d/%Y %H:%M:%S")),
          x = "Dates",
          y = ylab_daily_cases_hundy_txt
        ) +
        theme_bw() +
        theme(
          panel.grid.minor = element_blank(),
          #            panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = c(0.35, 0.87),
          legend.background = element_rect(
            linetype = "solid",
            size = 0.2,
            colour = "black"
          )
        )

      print(p)

      dev.off()
      file_to_bucket(filename)
    }

  } # for all counties

}

make_state_string <- function(state) {
  s <- tolower(state)
  s <- str_replace_all(tolower(s), " ", "_")
  return(s)
}

build_all_states <- function(combined = TRUE,
                             keep_dfs = FALSE,
                             write_dfs = FALSE,
                             plot_wa_and = FALSE,
                             plot_daily_cases = FALSE,
                             plot_state_cases_per_hundy = FALSE) {
  if (exists("usa_df")) {
    remove(usa_df, envir = .GlobalEnv)
  }

  if (plot_wa_and) {
    wa_cases <- get_admin1("Washington")
    max_wa_y = max(wa_cases$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
    wa_s_txt <-
      paste("Washington (pop=", pop_format(wa_cases$pop[1]), ")", sep = "")
  }

  states <- unique(sort(usa_confirmed$Province_State))

  for (state in states) {
    if (VERBOSE) {
      print(paste("state is", state))
    }
    new_df <- get_admin1(state)

    if (is.null(new_df)) {
      next
    }

    file_base <- str_replace_all(tolower(state), " ", "_")
    if (write_dfs) {
      filename <- paste(file_base, "csv", sep = ".")
      write.csv(new_df, filename)
      file_to_bucket(filename)
    }

    if (keep_dfs) {
      st_string <- make_state_string(state)
      new_df_name <- paste(st_string, "_df", sep = "")
      assign(new_df_name, new_df, envir = .GlobalEnv)

      # make the text name for graphs
      txt_arg <- paste(st_string, "_s_txt", sep = "")
      txt_value <- paste(state,
                         " State (pop=",
                         pop_format(new_df$pop[1]),
                         ")",
                         sep = "")
      assign(txt_arg, txt_value, envir = .GlobalEnv)
    }

    if (combined) {
      if (exists("usa_df")) {
        usa_df <- aggregate_dfs(usa_df, new_df)
      }
      else {
        usa_df <- new_df
      }
    }

    # really no point if there isn"t anyone there
    if (new_df[1, ]$pop == 0) {
      next
    }

    if (plot_daily_cases) {
      ret <- make_plot(
        df = new_df,
        loc_txt = state,
        daily_cases = TRUE,
        file_base = file_base
      )
      if (!is.null(ret)) {
        filename <- paste(file_base, "daily_cases.jpg", sep = "_")
        file_to_bucket(filename)
      }
    }

    if (plot_state_cases_per_hundy) {
      ret <- make_plot(
        new_df,
        loc_txt = state,
        cases_per_hundy = TRUE,
        file_base = file_base
      )
      if (!is.null(ret)) {
        filename <- paste(file_base, "cases_per_hundy.jpg", sep = "_")
        file_to_bucket(filename)
      }
    }

    if (plot_wa_and) {
      # multiple counties 14 day
      filename <- paste("wa_and_", tolower(state), ".jpg", sep = "")
      filename <- str_replace_all(filename, " ", "_")

      jpeg(filename = filename,
           width = plot_file_width,
           height = plot_file_height)

      new_df$wa_cases <- wa_cases$cases
      new_df$wa_daily_cases <- wa_cases$daily_cases
      new_df$wa_daily_cases_per_hundy_avrg14d <-
        wa_cases$daily_cases_per_hundy_avrg14d
      s_txt <-
        paste(state, " (pop=", pop_format(new_df$pop[1]), ")", sep = "")

      p <- ggplot(data = new_df, aes(dates)) +
        geom_line(aes(y = daily_cases_per_hundy_avrg14d,
                      colour = s_txt)) +
        geom_line(aes(y = wa_daily_cases_per_hundy_avrg14d,
                      colour = wa_s_txt)) +
        scale_color_manual(values = c("black", "darkgreen")) +
        ylim(0, max(new_df$daily_cases_per_hundy_avrg14d)) +
        labs(
          title = "Daily Cases per 100,000, 14day Average",
          subtitle = paste("created", format(Sys.time(), "%m/%d/%Y %H:%M:%S")),
          x = "Dates",
          y = ylab_daily_cases_hundy_txt
        ) +
        theme_bw() +
        theme(
          panel.grid.minor = element_blank(),
          #            panel.grid.major = element_blank(),
          panel.background = element_blank(),
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5),
          plot.caption = element_text(hjust = 0.5),
          legend.title = element_blank(),
          legend.position = c(0.35, 0.87),
          legend.background = element_rect(
            linetype = "solid",
            size = 0.2,
            colour = "black"
          )
        )

      print(p)

      dev.off()
      file_to_bucket(filename)
    }

  } # for all states

  return(usa_df)

}

wa_east_west <- function(plot_casesned = FALSE,
                         plot_casesned_cases = FALSE,
                         file_base = NULL) {
  state = "Washington"
  loc_txt = "Eastern / Western Washington"

  cases <-
    as.data.frame(subset(usa_confirmed, Province_State == state))

  counties <- unique(sort(cases$Admin2))

  for (county in counties) {
    if (str_detect(county, "Out of ") |
        county == "" |
        county == "unkown" |
        county == "Unassigned") {
      next
    }

    # get the county dataframe    
    df_name <- tolower(paste(state, county, "df", sep = "_"))
    df_name <- str_replace_all(df_name, "[ ]", "_")
    df <- eval(parse(text = df_name))

    if (wa_counties[which(wa_counties$county == county),]$eastwest == "eastern") {
      cat("aggregating", county, "east\n")
      if (exists("combined_east_df")) {
        combined_east_df <- aggregate_dfs(combined_east_df, df)
      }
      else {
        combined_east_df <- df
      }
    } else {
      cat("aggregating", county, "west\n")
      if (exists("combined_west_df")) {
        combined_west_df <- aggregate_dfs(combined_west_df, df)
      }
      else {
        combined_west_df <- df
      }
    }
  }

  if (!is.null(file_base)) {
    f <- paste(file_base, "_cases_per_hundy", ".jpg", sep = "")
    jpeg(filename = f,
         width = plot_file_width,
         height = plot_file_height)
  }

  plot(
    combined_east_df$dates,
    combined_east_df$cases_per_hundy,
    main = paste(loc_txt, cumulative_c19_cases_txt, hundy_txt),
    ylab = ylab_cases_hundy_txt,
    xlab = "Dates",
    type = "l",
    col = "gold",
    xlim = as.Date(c(plot_start_date, plot_end_date))
  )
  mtext(paste("created", format(Sys.time(), "%m/%d/%Y %H:%M:%S")), side = 3)
  legend(
    "topleft",
    legend = c("Eastern", "Western"),
    col = c("gold", "green"),
    lty = 1
  )
  lines(combined_west_df$dates,
        combined_west_df$cases_per_hundy,
        col = "green")
  if (!is.null(file_base)) {
    dev.off()
  }

  filename = "east_west_daily.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)

  maxy = max(combined_east_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
  east_txt <-
    paste("East of the Cascades, WA (pop=",
          pop_format(combined_east_df$pop[1]),
          ")",
          sep = "")
  west_txt <-
    paste("West of the Cascades, WA (pop=",
          pop_format(combined_west_df$pop[1]),
          ")",
          sep = "")

  ew_df <- data.frame(
    dates = combined_east_df$dates,
    east = combined_east_df$daily_cases_per_hundy_avrg14d,
    west = combined_west_df$daily_cases_per_hundy_avrg14d
  )
  p <- ggplot(data = ew_df, aes(dates)) +
    geom_line(aes(y = west, colour = west_txt)) +
    geom_line(aes(y = east, colour = east_txt)) +
    scale_color_manual(values = c("gold", "green")) +
    labs(
      title = main_daily_cases_hundy_14d_avrg_txt,
      subtitle = paste("created", format(Sys.time(), "%m/%d/%Y %H:%M:%S")),
      x = "Dates",
      y = ylab_daily_cases_hundy_txt
    ) +
    scale_y_continuous(
      limits = c(0, maxy),
      sec.axis = sec_axis(trans =  ~ . * 14, name =
                            "14 Day Sum / 100,000 Population")
    ) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.background = element_blank(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.87),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )

  print(p)
  dev.off()
  file_to_bucket(filename)

}

summarize_vax_wide_data <- function(df, latest_col) {
  df$latest <- df[, latest_col]
  df$vax_pct <- df$latest / df$Population
  df$week2ago <- df[, (latest_col - 14)]
  df$trend <-  df$latest - df$week2ago

  return(df)
}

summarize_wide_data <- function(df, latest_col) {
  df$latest <- df[, latest_col]
  # difference over 14days
  df$diff14 <- df$latest - df[, (latest_col - 14)]

  df$avrg14 <- df$diff14 / 14
  df$latest_per_hundy <- df$latest / df$Population * 100000
  df$avrg14_per_hundy <- df$avrg14 / df$Population * 100000

  df$week2ago <- df[, (latest_col - 14)]

  # change over 14days (two weeks ago)
  df$week2ago_diff14 <-
    df[, (latest_col - 14)] - df[, (latest_col - 28)]
  df$week2ago_avrg14 <- df$week2ago_diff14 / 14
  df$week2ago_per_hundy <- df$week2ago / df$Population * 100000
  df$week2ago_avrg14_per_hundy <-
    df$week2ago_avrg14 / df$Population * 100000
  df$trend <-  df$avrg14_per_hundy - df$week2ago_avrg14_per_hundy

  return(df)
}

# makes a consistent clean key for matching
mash_combined_key <- function(df) {
  # silly AK hacks
  df <-
    df %>% mutate(
      Combined_Key = replace(
        Combined_Key,
        Combined_Key == "Yakutat plus Hoonah-Angoon, Alaska, US",
        "Yakutat, Alaska, US"
      )
    ) %>% mutate(
      Combined_Key = replace(
        Combined_Key,
        Combined_Key == "Bristol Bay plus Lake and Peninsula, Alaska, US",
        "Lake and Peninsula, Alaska, US"
      )
    )

  df$combinedkeylc <- str_to_lower(df$Combined_Key)
  df$combinedkeylc <- str_replace_all(df$combinedkeylc, "[.]", "")
  df$combinedkeylc <- str_replace_all(df$combinedkeylc, "[ ]", "")
  # since we take out single quotes in when we convert from UTF-8 we need
  # to take out all single quotes for comparing keys
  df$combinedkeylc <- str_replace_all(df$combinedkeylc, "[']", "")
  return(df)
}

# makes a consistent clean key for matching
mash_province_state_key <- function(df) {
  df$provincestatelc <- str_to_lower(df$Province_State)
  df$provincestatelc <- str_replace_all(df$provincestatelc, "[.]", "")
  df$provincestatelc <- str_replace_all(df$provincestatelc, "[ ]", "")
  return(df)
}

prep_wide_data <- function() {
  us_counties_wide <- usa_confirmed
  us_counties_wide[us_counties_wide$Province_State %in% "Guam",]$Combined_Key <-
    "Guam, Guam, US"
  us_counties_wide[us_counties_wide$Province_State %in% "Virgin Islands",]$Combined_Key <-
    "Virgin Islands, Virgin Islands, US"

  # county data
  # before we add any columns get the last date column
  # then subtract one for the prior day since "today" might not be fully reported.
  latest <- dim(us_counties_wide)[2] - 1
  us_counties_wide <- mash_combined_key(us_counties_wide)

  us_counties_wide <- merge(us_counties_wide,
                            population[, c("Population", "Combined_Key", "combinedkeylc")],
                            by = "combinedkeylc",
                            all.x = TRUE)

  us_counties_wide <- summarize_wide_data(us_counties_wide, latest)

  # put in global environment
  us_counties_wide <<- us_counties_wide

  write.csv(us_counties_wide, file = "us_counties_covid19_cases.csv")

  # states
  us_states_wide <- us_states_wide_raw
  latest <- dim(us_states_wide)[2] - 1
  # get the pops for just us states
  uid_iso_fips_lookup_states <-
    filter(uid_iso_fips_lookup, Admin2 == "" &
             Country_Region == "US")
  us_states_wide <- merge(us_states_wide,
                          uid_iso_fips_lookup_states[, c("Population", "Province_State")],
                          by = "Province_State")

  us_states_wide <- mash_province_state_key(us_states_wide)
  us_states_wide <<- summarize_wide_data(us_states_wide, latest)
}

make_a_map_from_base <- function(df,
                                 key = NULL,
                                 var,
                                 base,
                                 title,
                                 midpoint = "mean",
                                 lowpoint = NULL,
                                 trans = NULL,
                                 border1_color = NULL,
                                 border1_df = NULL,
                                 border2_color = NULL,
                                 border2_df = NULL,
                                 caption = NULL,
                                 add_insert_boxes = FALSE,
                                 filebase = NULL) {
  # prepare to drop the axes and ticks but leave the guides and legends
  # We can't just throw down a theme_nothing()!
  ditch_the_axes <- theme(
    axis.text = element_blank(),
    axis.line = element_blank(),
    axis.ticks = element_blank(),
    panel.border = element_blank(),
    panel.grid = element_blank(),
    axis.title = element_blank()
  )

  # if we get a key then can make a df with only the key and values
  # to make available on the webpage
  if (!is.null(key)) {
    df4export <- unique(df[, c(key, var)])
    if (!is.null(filebase)) {
      filename <- paste(filebase, "csv", sep = ".")
      write.csv(df4export, filename)
      file_to_bucket(filename)
    }
  }

  meanv <- mean(df[, var], na.rm = TRUE)
  mean_txt <- paste("Mean =", round(meanv, digits = 1))
  med <- median(df[, var], na.rm = TRUE)

  iqr <- IQR(df[, var], na.rm = TRUE)
  if (is.null(lowpoint)) {
    data_range <- c(med - iqr * 1.5, iqr * 1.5 + med)
  }
  else {
    data_range <- c(lowpoint, iqr * 1.5 + med)
  }
  if (VERBOSE) {
    print(paste("iqr", iqr, "med", med, "range", data_range))
  }

  if (!is.null(filebase)) {
    filename <- paste(filebase, "jpg", sep = ".")
    jpeg(filename = filename,
         width = plot_file_width,
         height = plot_file_height)
  }

  mymap <- base +
    geom_polygon(data = df, aes(fill = get(var))) +
    theme_bw() +
    ditch_the_axes +
    labs(title = title,
        subtitle = paste("created", format(Sys.time(), "%m/%d/%Y %H:%M:%S"))) +
    theme(
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5)
    )
  if (is.null(trans)) {
    if (midpoint == "mean") {
      mymap <- mymap +
        scale_fill_gradient2(
          midpoint = meanv,
          low = "blue",
          mid = "white",
          high = "red",
          name = paste(mean_txt, "\nCenter at mean"),
          limits = data_range,
          oob = scales::squish
        )
    }
    else {
      mymap <- mymap +
        scale_fill_gradient2(
          midpoint = midpoint,
          low = "blue",
          mid = "white",
          high = "red",
          name = paste(mean_txt, "\nCenter at 0 (no change)"),
          limits = data_range,
          oob = scales::squish
        )
    }
  }
  else {
    if (trans == "log10") {
      mymap <- mymap +
        scale_fill_gradient(
          breaks = c(2, 4, 10, 100, 1000, 10000),
          low = "white",
          high = "red",
          space = "Lab",
          na.value = "pink",
          name = mean_txt,
          trans = "log10"
        )
    }
  }
  if (!is.null(border1_df)) {
    if (is.null(border1_color)) {
      border1_color <- "black"
    }
    mymap <- mymap +
      geom_polygon(data = border1_df,
                   color = border1_color,
                   fill = NA)
  }
  if (!is.null(border2_df)) {
    if (is.null(border2_color)) {
      border2_color <- "black"
    }
    mymap <- mymap +
      geom_polygon(data = border2_df,
                   color = border2_color,
                   fill = NA)
  }
  if (!is.null(caption)) {
    mymap <- mymap +
      labs(caption = caption)
  }
  if ( add_insert_boxes ) {
    mymap <- insert_boxes(mymap)
  }
  print(mymap)
  if (!is.null(filebase)) {
    dev.off()
    file_to_bucket(filename)
  }
}

transform_state <- function(object, rot, scale, shift){
  #    print(max(apply(bbox(object), 1, diff)) / scale)
  #    print(shift)
  #    print(coordinates(object))
  object %>% elide(rotate = rot) %>%
    elide(scale = max(apply(bbox(object), 1, diff)) / scale) %>%
    elide(shift = shift)
}

# manually construct insert boxes instead of map bboxes so they 
# look nicer.
insert_boxes <- function(p) {
  lat <- c(38,38,33,33,33,33)
  long <- c(-73,-68,-68,-73,-73,-73)
  name <- c("dc","dc","dc","dc","dc","dc")
  label <- c("", "DC", "", "", "", "")
  dc_box <- tibble(lat, long, name, label)

  lat <- c(33,33,28.5,28.5,28.5,28.5)
  long <- c(-75,-66,-66,-75,-75,-75)
  name <- c("pr","pr","pr","pr","pr","pr")
  label <- c("", "PR", "", "", "", "")
  pr_box <- tibble(lat, long, name, label)

  lat <- c(28.5,28.5,23.5,23.5,23.5,23.5)
  long <- c(-73,-68,-68,-73,-73,-73)
  name <- c("usvi","usvi","usvi","usvi","usvi","usvi")
  label <- c("", "US VI", "", "", "", "")
  usvi_box <- tibble(lat, long, name, label)

  lat <- c(33,33,29,29,29,29)
  long <- c(-125,-120,-120,-125,-125,-125)
  name <- c("guam","guam","guam","guam","guam","guam")
  label <- c("", "Guam", "", "", "", "")
  guam_box <- tibble(lat, long, name, label)

  lat <- c(31,31,22,22,29,29)
  long <- c(-120,-112,-112,-125, -125,-120)
  name <- c("ak","ak","ak","ak","ak","ak")
  label <- c("", "AK", "", "", "", "")
  ak_box <- tibble(lat, long, name, label)

  lat <- c(30,25,22,22,22,22)
  long <- c(-112,-99,-99,-112,-112,-112)
  name <- c("hi","hi","hi","hi","hi","hi")
  label <- c("", "HI", "", "", "", "")
  hi_box <- tibble(lat, long, name, label)

  boxes <- dc_box %>%
    rbind(pr_box) %>%
    rbind(guam_box) %>%
    rbind(hi_box) %>%
    rbind(ak_box) %>%
    rbind(usvi_box)

  insert_boxes <- p +
    geom_polygon(data = boxes,
                 color = "black",
                 aes(x = long, y = lat,
                      group = factor(name)),
                 fill = NA) +
    geom_text(data = boxes,
              aes(label = label,
                  x = long,
                  y = lat,
                  group = factor(name)),
              nudge_x = -1.5,
              nudge_y = -.5) +
    coord_fixed(1.3)
  return(insert_boxes)

  print(insert_boxes)
}


# a lot of manual tweaking to move non-conus states and territories
# into insert positions
# here 'cb' is census bureau
load_cb_shapefile <- function(loc,
                              layer,
                              doing_state = TRUE,
                              include_nmi = FALSE,
                              include_as = FALSE) {
  sf_in <-
    readOGR(dsn = loc,
            layer = layer,
            verbose = FALSE) %>% spTransform(CRS("+init=epsg:2163"))

  # mess with USVI name and make a combined key
  if (doing_state) {
    sf_in@data[sf_in$NAME == "United States Virgin Islands",]$NAME <-
      'Virgin Islands'
    sf_in$STATE_NAME <- sf_in$NAME
    sf_in$Combined_Key <- tolower(sf_in$NAME)
  }
  else {
    sf_in@data[sf_in$STATE_NAME == "United States Virgin Islands",]$STATE_NAME <-
      'Virgin Islands'
    sf_in@data[sf_in$STATE_NAME == "Virgin Islands",]$NAME <-
      'Virgin Islands'
    # cb files are UTF-8, other data sources are not
    sf_in@data$NAME <-
      iconv(sf_in@data$NAME, "UTF-8", "ASCII//TRANSLIT")
    sf_in@data$NAME <-
      gsub("'", "" , sf_in@data$NAME, ignore.case = TRUE)
    sf_in@data$NAME <-
      gsub("~", "" , sf_in@data$NAME, ignore.case = TRUE)
    sf_in@data$NAME <-
      gsub('"', "" , sf_in@data$NAME, ignore.case = TRUE)

    sf_in$Combined_Key <- paste(str_to_title(sf_in$NAME),
                                ", ",
                                str_to_title(sf_in$STATE_NAME),
                                ", US",
                                sep = "")
  }

  alaska <- sf_in[sf_in$STATE_NAME == "Alaska",] %>%
    transform_state(-35, 2.5, c(-2400000,-2100000))
  proj4string(alaska) <- proj4string(sf_in)

  hawaii <- sf_in[sf_in$STATE_NAME == "Hawaii",] %>%
    transform_state(-35, .75, c(-1000000, -2373000))
  proj4string(hawaii) <- proj4string(sf_in)

  dc <- sf_in[sf_in$STATE_NAME == "District of Columbia",] %>%
    transform_state(0, .1, c(2525000, -700000))
  proj4string(dc) <- proj4string(sf_in)

  pr <- sf_in[sf_in$STATE_NAME == "Puerto Rico",] %>%
    transform_state(0, .5, c(2500000, -1250000))
  proj4string(pr) <- proj4string(sf_in)

  guam <- sf_in[sf_in$STATE_NAME == "Guam",] %>%
    transform_state(0, .25, c(-2200000, -1400000))
  proj4string(guam) <- proj4string(sf_in)

  usvi <- sf_in[sf_in$STATE_NAME == "Virgin Islands",] %>%
    transform_state(0, .25, c(2800000, -1800000))
  proj4string(usvi) <- proj4string(sf_in)


  if (include_as) {
    as <- sf_in[sf_in$STATE_NAME == "American Samoa",] %>%
      transform_state(0, 1, c(5500000, 2200000))
    proj4string(as) <- proj4string(sf_in)
  }

  if (include_nmi) {
    nmi <-
      sf_in[sf_in$STATE_NAME == "Commonwealth of the Northern Mariana Islands",] %>%
      transform_state(0, .5, c(3000000, -300000))
    proj4string(nmi) <- proj4string(sf_in)
  }

  bound_df <-
    sf_in[!sf_in$STATE_NAME %in% c(
      "Alaska",
      "Hawaii",
      "Guam",
      "Commonwealth of the Northern Mariana Islands",
      "Virgin Islands",
      "Puerto Rico",
      "American Samoa"
    ),] %>%
    rbind(alaska) %>%
    rbind(hawaii) %>%
    rbind(dc) %>%
    rbind(pr) %>%
    rbind(usvi) %>%
    rbind(guam)

  if (include_nmi) {
    bound_df <- bound_df %>% rbind(nmi)
  }
  if (include_as) {
    bound_df <- bound_df %>% rbind(as)
  }

  polys <- spTransform(bound_df, CRS("+init=epsg:4326"))
  fortified <-
    fortify(polys, region = "Combined_Key") %>% 
    mutate(id = tolower(id))

  return(fortified)

}

test_thing <- function() {
  mys <- get_county_polygons()
  #  mys <- load_county_shapefile(shp_loc, layer)
  thing <-
    ggplot(data = mys,
           mapping = aes(
             x = long,
             y = lat,
             group = factor(group)
           )) +
    geom_polygon(color = "black") +
    coord_fixed(1.3)
  thing <- insert_boxes(thing)


  jpeg(filename = "test.jpg",
       width = plot_file_width,
       height = plot_file_height)
  print(thing)
  dev.off()
}

dl_unzip <- function(zip_url) {
  zip <- tempfile()
  download.file(zip_url, zip)
  unzipped_dir <- tempdir()
  unzip(zip, overwrite = TRUE, exdir = unzipped_dir)
  if (!KEEP_FILES) {
    unlink(zip)
  }
  return(unzipped_dir)
}

get_states_polygons <- function() {
  # from https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
  # 1 : 500,000 (national)  https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_all_500k.zip
  # 1 : 5,000,000 (national)   "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_all_5m.zip"
  # https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_5m.zip
  # 1 : 20,000,000 (national)    https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_all_20m.zip

  zip_url <-
    "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_state_5m.zip"
  unzipped_dir <- dl_unzip(zip_url)

  loc <- file.path(unzipped_dir, "cb_2020_us_state_5m.shp")
  print(paste("Shapefile:", loc))

  layer <- "cb_2020_us_state_5m"
  states50new <- load_cb_shapefile(loc, layer, doing_state = TRUE)
  if (!KEEP_FILES) {
    unlink(unzipped_dir)
  }
  return(states50new)

}

get_county_polygons <- function() {
  # from https://www.census.gov/geographies/mapping-files/time-series/geo/cartographic-boundary.html
  # 1 : 5,000,000 (national)   https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_5m.zip

  zip_url <-
    "https://www2.census.gov/geo/tiger/GENZ2020/shp/cb_2020_us_county_5m.zip"
  unzipped_dir <- dl_unzip(zip_url)

  loc <- file.path(unzipped_dir, "cb_2020_us_county_5m.shp")
  print(paste("Shapefile:", loc))

  layer <- "cb_2020_us_county_5m"
  counties50new <-
    load_cb_shapefile(loc, layer, doing_state = FALSE)
  if (!KEEP_FILES) {
    unlink(unzipped_dir)
  }
  return(counties50new)
}

# make base maps
# we get maps both from mapdata and census bureau shapefiles
make_map_bases <- function() {

  # first get usa outline, make global
  usa_mapd <<- map_data("usa")

  # states outlines
  # states_mapd is just CONUS and states50 is 50 states + inserts
  states_mapd <- map_data("state")
  states50 <<- get_states_polygons()

  # county outlines
  # counties50 is all 50 states + inserts
  counties50 <- get_county_polygons()
  counties50$Combined_Key <- counties50$id
  counties50 <- mash_combined_key(counties50)

  # frankly the simple maps look better when blown-up
  # so we have both
  counties_mapd <- map_data("county")
  # always having to 'fix' dc
  counties_mapd[counties_mapd$region %in% "district of columbia",]$subregion <- "district of columbia"
  # make a combined key that matches our data
  counties_mapd$Combined_Key <- paste(
      str_to_title(counties_mapd$subregion),
      ", ",
      str_to_title(counties_mapd$region),
      ", US",
      sep = ""
    )
  counties_mapd <- mash_combined_key(counties_mapd)

  # add Province_State to make merging easier
  # we keep this around for the borders
  states_mapd$Province_State = str_to_title(states_mapd$region)
  states_mapd <- mash_province_state_key(states_mapd)
  # this fails:
  states_mapd_merged <-
    inner_join(states_mapd, us_states_wide, by = "provincestatelc")
  states_mapd <<- states_mapd

  # add Province_State to make merging easier
  states50$Province_State = str_to_title(states50$id)
  states50 <- mash_province_state_key(states50)
  states50_merged <<-
    inner_join(states50, us_states_wide, by = "provincestatelc")

  counties50_merged <-
    inner_join(counties50, us_counties_wide, by = "combinedkeylc")

  counties_mapd_merged <-
    inner_join(counties_mapd, us_counties_wide, by = "combinedkeylc")

  wa_counties_mapd_merged <<-
    subset(counties_mapd_merged, Province_State == "Washington")
  mddcva_counties_mapd_merged <<-
    subset(counties_mapd_merged, 
           Province_State == "Maryland" | 
             Province_State == "Virginia" | 
             Province_State == "District of Columbia")
  
  vax_states_merged <<-
    inner_join(states_mapd, vax_us_wide, by = "Province_State")
  # use key = "Province_State"

  wa_df <<- subset(states_mapd, region == "washington")
  wa_base <<-
    ggplot(data = wa_df,
           mapping = aes(x = long, y = lat, group = group)) +
    coord_fixed(1.3) +
    geom_polygon(color = "black", fill = "gray")

  mddcva_df <<- subset(states_mapd, 
                    region == "maryland" | 
                      region == "district of columbia" |
                      region == "virginia")
  mddcva_base <<-
    ggplot(data = mddcva_df,
           mapping = aes(x = long, y = lat, group = group)) +
    coord_fixed(1.3) +
    geom_polygon(color = "black", fill = "gray")
  
  states50_base <<-
    ggplot(data = states50,
           mapping = aes(
             x = long,
             y = lat,
             group = factor(group)
           )) +
    geom_polygon(color = "white") +
    coord_fixed(1.3)

  # us county maps
  counties50_base <<-
    ggplot(data = counties50_merged,
           mapping = aes(
             x = long,
             y = lat,
             group = factor(group)
           )) +
    geom_polygon(color = "black") +
    coord_fixed(1.3)

  # grey out states that aren't sending-up data
  # Nebraska puts all of there cases in 'Unassigned'
  # Maryland got hacked
  for (s in config$bad_data_states) {
    counties50_merged[counties50_merged$Province_State 
                      %in% s, ]$avrg14_per_hundy <- NA
    counties50_merged[counties50_merged$Province_State 
                      %in% s, ]$trend <- NA
    
  }
  
  counties50_merged <<- counties50_merged
  
  return()
}

make_maps <- function() {

  make_a_map_from_base(
    df = wa_counties_mapd_merged,
    key = "Combined_Key.x",
    var = "avrg14_per_hundy",
    base = wa_base,
    lowpoint = 0,
    border1_color = "grey",
    border1_df = wa_counties_mapd_merged,
    border2_df = wa_df,
    title = paste("Washington",
                  main_daily_cases_hundy_14d_avrg_txt),
    filebase = "map_wa_14avrg"
  )
  make_a_map_from_base(
    df = wa_counties_mapd_merged,
    var = "trend",
    key = "Combined_Key.x",
    midpoint = 0,
    border1_color = "grey",
    border1_df = wa_counties_mapd_merged,
    border2_df = wa_df,
    base = wa_base,
    title = paste("Washington", main_14day_trend_txt),
    filebase = "map_wa_trend"
  )

  make_a_map_from_base(
    df = mddcva_counties_mapd_merged,
    key = "Combined_Key.x",
    var = "avrg14_per_hundy",
    base = mddcva_base,
    lowpoint = 0,
    border1_color = "grey",
    border1_df = mddcva_counties_mapd_merged,
    border2_df = mddcva_df,
    title = paste("Maryland, DC, & Virginia",
                  main_daily_cases_hundy_14d_avrg_txt),
    filebase = "map_mddcva_14avrg"
  )
  make_a_map_from_base(
    df = mddcva_counties_mapd_merged,
    var = "trend",
    key = "Combined_Key.x",
    midpoint = 0,
    border1_color = "grey",
    border1_df = mddcva_counties_mapd_merged,
    border2_df = mddcva_df,
    base = mddcva_base,
    title = paste("Maryland, DC, Virginia", main_14day_trend_txt),
    filebase = "map_mddcva_trend"
  )
  
  make_a_map_from_base(
    df = states50_merged,
    var = "avrg14_per_hundy",
    key = "provincestatelc",
    lowpoint = 0,
    base = states50_base,
    border1_color = "grey",
    border1_df = states50,
    border2_df = usa_mapd,
    title = paste("US", main_daily_cases_hundy_14d_avrg_txt, "States"),
    add_insert_boxes = TRUE,
    filebase = "map_usa_14avrg"
  )

  make_a_map_from_base(
    df = states50_merged,
    var = "trend",
    key = "provincestatelc",
    midpoint = 0,
    base = states50_base,
    border1_color = "grey",
    border1_df = states50,
    border2_df = usa_mapd,
    title = paste("US", main_14day_trend_txt, "States"),
    add_insert_boxes = TRUE,
    filebase = "map_usa_trend"
  )

  make_a_map_from_base(
    df = vax_states_merged,
    var = "vax_pct",
    key = "Province_State",
    lowpoint = 0,
    base = states50_base,
    border1_color = "grey",
    border1_df = states50,
    border2_df = usa_mapd,
    add_insert_boxes = TRUE,
    title = paste("US", main_daily_cases_hundy_14d_avrg_txt, "States"),
    filebase = "vax1"
  )

  make_a_map_from_base(
    df = counties50_merged,
    var = "avrg14_per_hundy",
    key = "Combined_Key.x",
    lowpoint = 0,
    base = counties50_base,
    title = paste("US", main_daily_cases_hundy_14d_avrg_txt, "Counties"),
    #    trans = "log10",
    border1_color = "grey",
    border1_df = states_mapd,
    add_insert_boxes = TRUE,
    border2_df = usa_mapd,
    caption = "(black or grey represends missing data)",
    filebase = "map_usa_14avrg_c"
  )
  make_a_map_from_base(
    df = counties50_merged,
    var = "trend",
    key = "Combined_Key.x",
    midpoint = 0,
    base = counties50_base,
    title = paste("US", main_14day_trend_txt, "Counties"),
    border1_color = "grey",
    border1_df = states_mapd,
    border2_df = usa_mapd,
    add_insert_boxes = TRUE,
    caption = "(black or grey represends missing data)",
    filebase = "map_usa_trend_c"
  )

  return()
}
