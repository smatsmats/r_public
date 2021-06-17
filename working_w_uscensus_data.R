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


# tmp goes in tmp, everything is tmp
setwd("/tmp")



# reads in population file date and does some formating
get_population <- function() {
    # population data from census bureau
    population <-
      read.csv(
        "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/counties/totals/co-est2019-alldata.csv"
      )

    st_pops <-
      read.csv(
        "https://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-alldata.csv"
      )

    for (r in 1:nrow(population)) {
      if (population[r, "STNAME"] == "Alaska") {
        c <- population[r, "CTYNAME"]
        c_out <- c
        c_out <- str_remove(c_out, " Area")
        c_out <- str_remove(c_out, " Borough")
        c_out <- str_remove(c_out, " City")
        c_out <- str_remove(c_out, " Municipality")
        c_out <- str_remove(c_out, " and")
        c_out <- str_remove(c_out, " Census")
        if (c_out == "Lake Peninsula") {
          c_out <- "Lake and Peninsula"
        }
        population[r, "CTYNAME"] <- c_out
      } else if (population[r, "STNAME"] == "New Mexico") {
        c <- population[r, "CTYNAME"]
        c_out <- c
        if (c == "Do\xf1a Ana County") {
          c_out <- "Dona Ana County"
        }
        population[r, "CTYNAME"] <- c_out
      }
    }
    population$state_ <- tolower(population$STNAME)
    population$ctyname_ <- tolower(population$CTYNAME)

  return(population)
}


onetime <- function() {
    county_transformations <<-
      read.csv(
        "https://docs.google.com/uc?id=15pWPcFsrx-MhzXdzqFtVOGhr2swooRvE&export=download"
      )

  return(0)
}

# cleans-up some goofy county names - maybe only need this with US Census pops
get_full_county_name <- function(state = "not alaska",  county) {
  if (state == "Alaska") {
    full_county_name <- county
  } else if (state == "District of Columbia") {
    full_county_name <- "District of Columbia"
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

# returns population in 100,000"s
get_pop_uscensus <- function(state, county = "Total") {
  #  if ( state == "District Of Columbia" ) {
  #    state <- "District of Columbia"
  #  }
  
  state <- tolower(state)
  
  # if request is for the whole state the county = state and county # == 0
  if (county == "Total") {
    print(paste("going to get pop for: ", state, sep = ""))
    row <- subset(population, state_ == state &
                    ctyname_ == state &
                    COUNTY == 0)
  }
  else {
    pop_county_name = tolower(get_full_county_name(tolower(state),
                                                   tolower(county)))
    print(paste(
      "going to get pop for: ",
      county,
      " (",
      pop_county_name,
      ") ",
      state,
      sep = ""
    ))
    row <- subset(population,
                  state_ == state &
                    ctyname_ == pop_county_name &
                    COUNTY > 0)
  }
  
  if (nrow(row) != 1) {
    print(paste("FAIL: can't get pop for", county, state))
    return(0)
  }
  pop <- row$POPESTIMATE2019
  hundy <- as.double(round(pop / 100000, 3))
  return(hundy)
}


get_pop <- function(state = NULL,
                    county = NULL,
                    country = NULL,
                    province_state = NULL,
                    admin2 = NULL) {
  if (!is.null(province_state)) {
    state <- province_state
  }
  if (!is.null(admin2)) {
    county <- admin2
  }
  admin2 <- county
  province_state <- state
    if (is.null(county)) {
      county <- "Total"
    }
    return(get_pop_uscensus(state, county))
}



