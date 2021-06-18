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

# design decisions
# - don't combine plotting and making df"s
# - except where it makes sense, i.e. build all states
#
#

# tmp goes in tmp, everything is tmp
setwd("/tmp")

# Some flags
ENABLE_RED_BLUE <- FALSE
USA_ALL <- TRUE
USE_GGPLOT <- TRUE       # versus base graphs
VERBOSE <- FALSE
KEEP_FILES <- FALSE      # don't remove files after being pushed
live_mode <- FALSE


make_plot_base <- function(df,
                           loc_txt,
                           main_txt = NULL,
                           cases_per_hundy = TRUE,
                           cases = TRUE,
                           daily_cases = FALSE,
                           file_base = NULL) {
  # maybe bail
  if (is.null(df)) {
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
    
    plot(
      df$dates,
      df$cases_per_hundy,
      main = paste(loc_txt, cumulative_c19_cases_txt, hundy_txt),
      ylab = ylab_cases_hundy_txt,
      xlab = "Dates",
      type = "l",
      col = "purple",
      xlim = as.Date(c(plot_start_date, plot_end_date))
    )
    mtext(paste("created", format(Sys.Date(), "%m/%d/%Y")), side = 3)
    
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
    
    plot(
      df$dates,
      df$cases,
      main = paste(loc_txt, cumulative_c19_cases_txt),
      ylab = ylab_cases_txt,
      xlab = "Dates",
      type = "l",
      col = "purple",
      xlim = as.Date(c(plot_start_date, plot_end_date))
    )
    mtext(paste("created", format(Sys.Date(), "%m/%d/%Y")), side = 3)
    
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
    
    maxy = max(df$daily_cases)
    plot(
      df$dates,
      df$daily_cases,
      main = paste(loc_txt, daily_c19_cases_txt),
      ylab = ylab_daily_cases_txt,
      ylim = c(0, maxy),
      xlab = "Dates",
      type = "l",
      col = "mediumpurple1",
      xlim = as.Date(c(plot_start_date, plot_end_date))
    )
    lines(df$dates, df$daily_cases_avrg14d, col = "red")
    legend(
      "topleft",
      legend = c("Daily", "14 Day Average"),
      col = c("mediumpurple1", "red"),
      lty = 1
    )
    mtext(paste("created", format(Sys.Date(), "%m/%d/%Y")), side = 3)
    
    
    if (!is.null(file_base)) {
      dev.off()
    }
  } # if daily cases
}


make_plot <- function(df,
                      loc_txt,
                      main_txt = NULL,
                      cases_per_hundy = FALSE,
                      cases = FALSE,
                      daily_cases = FALSE,
                      file_base = NULL) {
  if (USE_GGPLOT) {
    return(
      make_plot_gg(
        df = df,
        loc_txt = loc_txt,
        main_txt = main_txt,
        cases_per_hundy = cases_per_hundy,
        cases = cases,
        daily_cases = daily_cases,
        file_base = file_base
      )
    )
  }
  else {
    return(
      make_plot_base(
        df = df,
        loc_txt = loc_txt,
        main_txt = main_txt,
        cases_per_hundy = cases_per_hundy,
        cases = cases,
        daily_cases = daily_cases,
        file_base = file_base
      )
    )
  }
}

multi_make_plot <- function(df,
                            multi_cats,
                            loc_txt = "loc_txt",
                            main_txt = NULL,
                            cases_per_hundy = TRUE,
                            cases = TRUE,
                            daily_cases = FALSE,
                            file_base = NULL) {
  p <- ggplot(data = df, aes(dates)) +
    scale_color_manual(values = c("14 Day Average" = "red",
                                  "Daily" = "mediumpurple1")) +
    #   ylim(0,max(df$daily_cases)) +
    labs(
      title = paste(loc_txt, daily_c19_cases_txt),
      subtitle = paste("created", format(Sys.Date(), "%m/%d/%Y")),
      x = "Dates",
      y = ylab_daily_cases_txt
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
  for (cat in multi_cats) {
    p <- p + geom_line(aes(y = cat, colour = "Daily"), size = 0.3)
  }
  
  
  #    geom_line(aes(y = daily_cases_avrg14d, colour="14 Day Average"))
  
  print(p)
  
  if (!is.null(file_base)) {
    dev.off()
  }
  
} # multi_make_plot


if (live_mode) {
  ic_cases <- get_admin2("Washington",
                         county = "Island")
  make_plot(
    df = ic_cases,
    loc_txt = "bongo",
    cases_per_hundy = TRUE,
    cases = TRUE,
    daily_cases = TRUE
  )
  
}

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
get_admin2 <- function(state, county) {
  cat("in get_admin2(", county, ")")
  
  if (county == "Total") {
    county_cases_t <- as.data.frame(subset(usa_states,
                                           Province_State == state))
  }
  else {
    # convert into a data frame instead of a tuple.  tuple has big performance impacts down the road
    county_cases_t <-
      as.data.frame(subset(usa_confirmed_t, Admin2 == county &
                             Province_State == state))
  }
  
  pop <- get_pop(state, county, country = "US")
  
  df <- build_cols(county_cases_t, pop)
  
  return(df)
  
}


if (live_mode) {
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
  
}

write_csv_file <- function(df, file_base) {
  write.csv(df, paste(file_base, ".csv", sep = ""))
}

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


# var                           source          aggregate function
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
  #    print(paste("in aggregate_dfs", in_df[nrow(in_df),"cases"], new_df[nrow(in_df),"cases"]))
  #    print(paste("in aggregate_dfs", in_df[1,"pop"], new_df[1,"pop"]))
  
  for (r in 1:nrow(in_df)) {
    #      print(paste("in:", in_df[r, "pop"], "new:", new_df[r, "pop"]))
    in_df[r, "pop"] <- in_df[r, "pop"] + new_df[r, "pop"]
    #      print(paste("combined:", in_df[r, "pop"]))
    
    in_df[r, "cases"] <- in_df[r, "cases"] + new_df[r, "cases"]
    in_df[r, "cases_per_hundy"] <-
      in_df[r, "cases"] / in_df[r, "pop"]
    in_df[r, "daily_cases"] <- in_df[r, "daily_cases"] +
      new_df[r, "daily_cases"]
    in_df[r, "daily_cases_per_hundy"] <-
      in_df[r, "daily_cases"] / in_df[r, "pop"]
    
    #      print(paste("where the fun starts", r))
    if (r < 7) {
      in_df[r, "daily_cases_avrg7d"] <- 0
      in_df[r, "daily_cases_per_hundy_avrg7d"] <- 0
      in_df[r, "daily_cases_sum7d"] <- 0
      in_df[r, "daily_cases_per_hundy_sum7d"] <- 0
    }
    else {
      #        print(in_df[(r-6):r, "daily_cases"])
      #        print(zoo::rollmean(in_df[(r-6):r, "daily_cases"], k = 7, fill = NA, align="left"))
      #        print(zoo::rollmean(in_df[(r-6):r, "daily_cases"], k = 7, fill = NA, align="left")[0])
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
    
    
    #      in_df[r, "daily_cases_avrg7d"] <- in_df[r, "daily_cases_avrg7d"] +
    #        new_df[r, "daily_cases_avrg7d"]
    #      in_df[r, "daily_cases_avrg14d"] <- in_df[r, "daily_cases_avrg14d"] +
    #        new_df[r, "daily_cases_avrg14d"]
    #      in_df[r, "daily_cases_per_hundy_avrg7d"] <- in_df[r, "daily_cases_per_hundy_avrg7d"] +
    #        new_df[r, "daily_cases_per_hundy_avrg7d"]
    #      in_df[r, "daily_cases_per_hundy_avrg14d"] <- in_df[r, "daily_cases_per_hundy_avrg14d"] +
    #        new_df[r, "daily_cases_per_hundy_avrg14d"]
    
    
    #      print("kinda done")
    
    if (ENABLE_RED_BLUE) {
      in_df[r, "red_cases"] <-
        in_df[r, "red_cases"] + new_df[r, "red_cases"]
      #      print(r)
      if (r < 8) {
        in_df[r, "red_daily_cases_avrg7d"] <- 0
        in_df[r, "red_daily_cases_per_hundy_avrg7d"] <- 0
      }
      else {
        #        in_df[r, "red_daily_cases_avrg7d"] <- zoo::rollmean(in_df[(r-6):r, "daily_cases"], k = 7, fill = NA, align="left")[0]
        #        in_df[r, "red_daily_cases_per_hundy_avrg7d"] <- in_df[r, "red_daily_cases_avrg7d"] / in_df[r, "pop"]
      }
      if (r < 15) {
        in_df[r, "red_daily_cases_avrg14d"] <- 0
        in_df[r, "red_daily_cases_per_hundy_avrg14d"] <- 0
      }
      else {
        #        in_df[r, "red_daily_cases_avrg14d"] <- zoo::rollmean(in_df[(r-13):r, "daily_cases"], k = 14, fill = NA, align="left")[0]
        #        in_df[r, "red_daily_cases_per_hundy_avrg14d"] <- in_df[r, "red_daily_cases_avrg14d"] / in_df[r, "pop"]
      }
      
      #      in_df[r, "red_daily_cases_avrg14d"] <- in_df[r, "red_daily_cases_avrg14d"] +
      #        new_df[r, "red_daily_cases_avrg14d"]
      
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

if (live_mode) {
  
}


get_admin1 <- function(admin1,
                       admin0 = "US") {
  cat("in get_admin1, state:",
      admin1,
      "country:",
      admin0,
      "\n")
  
  
  if (admin0 == "US") {
    state_cases_t <-
      as.data.frame(subset(
        usa_states,
        grepl(admin1, Province_State, ignore.case = TRUE)
      ))
  }
  else  {
    country <-
      admin0  #  ARRRG I don't understand why this is needed!!!!
    state_cases_t <- as.data.frame(subset(
      global_confirmed_t,
      grepl(country,
            Country.Region,
            ignore.case = TRUE) &
        grepl(admin1,
              Province.State,
              ignore.case = TRUE)
    ))
  }
  
  
  pop <- get_pop(admin1, country = admin0)
  
  df <- build_cols(state_cases_t, pop)
  
  return(df)
  
}


if (live_mode) {
  wa_cases <<- get_admin1("Washington")
  make_plot(wa_cases, "bongo", cases = TRUE)
  dp_cases <<- get_admin1("Diamond Princess")
  ca_bc_cases <<- get_admin1("British Columbia", admin0 = "Canada")
  make_plot(ca_bc_cases, "bongo", daily_cases = TRUE)
  dc_cases <<- get_admin1("District of Columbia")
  write.csv(wa_cases, "wa_cases.csv")
  pr_cases <- get_admin1("Puerto Rico")
}

get_admin0 <- function(country_in) {
  cat("in get_admin0:", country_in)
  
  # convert into a data frame instead of a tuple.
  # tuple has big performance impacts down the road
  country_cases_t <- as.data.frame(subset(
    admin0_t,
    grepl(country_in, Country.Region, ignore.case = TRUE)
  ))
  
  pop <- get_pop(country = country_in)
  
  df <- build_cols(country_cases_t, pop)
  
  return(df)
  
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
    if (new_df[1,]$pop == 0) {
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
      
        max_new_y = max(new_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
        maxy <- if (max_wa_y > max_new_y) {
          max_wa_y
        } else {
          max_new_y
        }
        s_txt <-
          paste(state, " (pop=", pop_format(new_df$pop[1]), ")", sep = "")
        
        plot(
          new_df$dates,
          new_df$daily_cases_per_hundy_avrg14d,
          main = "Daily Cases per 100,000, 14day Average",
          ylab = ylab_daily_cases_hundy_txt,
          ylim = c(0, maxy),
          xlab = "Dates",
          type = "l",
          col = "black",
          xlim = as.Date(c(plot_start_date, plot_end_date))
        )
        lines(washington_df$dates,
              washington_df$daily_cases_per_hundy_avrg14d,
              col = "darkgreen")
        legend(
          "topleft",
          legend = c(s_txt, wa_s_txt),
          col = c("black", "darkgreen"),
          lty = 1
        )
        mtext(paste("created", format(Sys.Date(), "%m/%d/%Y")), side = 3)
      
      dev.off()
      file_to_bucket(filename)
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
    
    if (wa_counties[which(wa_counties$county == county), ]$eastwest == "eastern") {
      cat("east\n")
      east_df <- get_admin2(state = state, county = county)
      if (exists("combined_east_df")) {
        combined_east_df <- aggregate_dfs(combined_east_df, east_df)
      }
      else {
        combined_east_df <- east_df
      }
    } else {
      cat("west\n")
      west_df <- get_admin2(state = state, county = county)
      if (exists("combined_west_df")) {
        combined_west_df <- aggregate_dfs(combined_west_df, west_df)
      }
      else {
        combined_west_df <- west_df
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
  mtext(paste("created", format(Sys.Date(), "%m/%d/%Y")), side = 3)
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
  
  
}


    plot(
      kc_cases$dates,
      kc_cases$cases_per_hundy,
      main = paste(cumulative_c19_cases_txt, hundy_txt),
      ylab = ylab_cases_hundy_txt,
      xlab = "Dates",
      type = "l",
      col = "green",
      lwd = 2,
      xlim = as.Date(c(plot_start_date, plot_end_date)),
      ylim = c(0, max(b_ci_cases$cases_per_hundy, na.rm = TRUE))
    )
    legend(
      "topleft",
      legend = c(
        kc_txt,
        yak_txt,
        ic_txt,
        kit_txt,
        b_co_txt,
        b_ci_txt,
        all_txt,
        mad_txt,
        montana_s_txt,
        sji_txt,
        jeff_txt
      ),
      col = c(
        "green",
        "Grey",
        "blue",
        "yellow",
        "red",
        "red",
        "darkred",
        "lightblue",
        "orange",
        "purple",
        "black"
      ),
      lty = 1:2,
      cex = 0.8
    )
    legend(
      "top",
      legend = "WA Combined",
      col = "darkgreen",
      lty = 5,
      lwd = 2
    )
    lines(
      washington_df$dates,
      washington_df$cases_per_hundy,
      col = "darkgreen",
      lty = 5,
      lwd = 2
    )
    mtext(paste("created", format(Sys.Date(), "%m/%d/%Y")), side = 3)
    lines(
      ic_cases$dates,
      ic_cases$cases_per_hundy,
      col = "blue",
      lty = 1,
      lwd = 2
    )
    lines(kit_cases$dates,
          kit_cases$cases_per_hundy,
          col = "yellow",
          lty = 2)
    lines(yak_cases$dates,
          yak_cases$cases_per_hundy,
          col = "grey",
          lty = 2)
    lines(b_co_cases$dates,
          b_co_cases$cases_per_hundy,
          col = "red",
          lty = 1)
    lines(b_ci_cases$dates,
          b_ci_cases$cases_per_hundy,
          col = "red",
          lty = 2)
    lines(all_cases$dates,
          all_cases$cases_per_hundy,
          col = "darkred",
          lty = 1)
    lines(mad_cases$dates,
          mad_cases$cases_per_hundy,
          col = "lightblue",
          lty = 2)
    lines(montana_df$dates,
          montana_df$cases_per_hundy,
          col = "orange",
          lty = 1)
    lines(sji_cases$dates,
          sji_cases$cases_per_hundy,
          col = "purple",
          lty = 2)
    lines(jeff_cases$dates,
          jeff_cases$cases_per_hundy,
          col = "black",
          lty = 1)
    # re-add king to make sure it"s on "top"
    lines(
      kc_cases$dates,
      kc_cases$cases_per_hundy,
      col = "green",
      lty = 1,
      lwd = 2
    )
    
  
  
  ##############################################################################
  filename = "uw_v_wsu.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  
    plot(
      wh_cases$dates,
      wh_cases$cases_per_hundy,
      main = paste(cumulative_c19_cases_txt, hundy_txt),
      ylab = ylab_cases_hundy_txt,
      xlab = "Dates",
      type = "l",
      col = "darkred",
      xlim = as.Date(c(plot_start_date, plot_end_date)),
      ylim = c(0, max(wh_cases$cases_per_hundy))
    )
    legend(
      "topleft",
      legend = c(wh_txt, kc_txt),
      col = c("darkred", "purple"),
      lty = 1,
      cex = 2
    )
    lines(kc_cases$dates, kc_cases$cases_per_hundy, col = "purple")
    mtext(paste("created", format(Sys.Date(), "%m/%d/%Y")), side = 3)
  } # USE_GGPLOT
  
  dev.off()
  file_to_bucket(filename)
  
  
  ##############################################################################
  filename = "is_king_balto.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  maxy = max(b_co_cases$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
  
    plot(
      kc_cases$dates,
      kc_cases$daily_cases_per_hundy_avrg14d,
      main = "Daily Cases per 100,000, 14day Average",
      ylab = ylab_daily_cases_hundy_txt,
      ylim = c(0, maxy),
      xlab = "Dates",
      type = "l",
      col = "green",
      xlim = as.Date(c(plot_start_date, plot_end_date))
    )
    lines(ic_cases$dates,
          ic_cases$daily_cases_per_hundy_avrg14d,
          col = "blue")
    lines(b_co_cases$dates,
          b_co_cases$daily_cases_per_hundy_avrg14d,
          col = "red")
    legend(
      "topleft",
      legend = c(kc_txt, ic_txt, b_co_txt),
      col = c("green", "blue", "red"),
      lty = 1
    )
    mtext(paste("created", format(Sys.Date(), "%m/%d/%Y")), side = 3)
  
  dev.off()
  file_to_bucket(filename)
  
  ##############################################################################
  filename <- "is_king_wa.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  maxy <-
    max(washington_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
  
    plot(
      kc_cases$dates,
      kc_cases$daily_cases_per_hundy_avrg14d,
      main = "Daily Cases per 100,000, 14day Average",
      ylab = ylab_daily_cases_hundy_txt,
      ylim = c(0, maxy),
      xlab = "Dates",
      type = "l",
      col = "green",
      xlim = as.Date(c(plot_start_date, plot_end_date))
    )
    lines(ic_cases$dates,
          ic_cases$daily_cases_per_hundy_avrg14d,
          col = "blue")
    lines(washington_df$dates,
          washington_df$daily_cases_per_hundy_avrg14d,
          col = "darkgreen")
    legend(
      "topleft",
      legend = c(ic_txt, kc_txt, washington_s_txt),
      col = c("green", "blue", "darkgreen"),
      lty = 1
    )
    mtext(paste("created", format(Sys.Date(), "%m/%d/%Y")), side = 3)
  
  dev.off()
  file_to_bucket(filename)
  
  
  ##############################################################################
  # MISC graphs
  # multiple counties 14 day
  filename = "misc.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  
  maxy = max(montana_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)
  
    plot(
      wa_cases$dates,
      wa_cases$daily_cases_per_hundy_avrg14d,
      # plot(kc_cases$dates, wa_cases$daily_cases_per_hundy_avrg14d,
      main = "Daily Cases per 100,000, 14day Average",
      ylab = ylab_daily_cases_hundy_txt,
      ylim = c(0, maxy),
      xlab = "Dates",
      type = "l",
      col = "darkgreen",
      xlim = as.Date(c(plot_start_date, plot_end_date))
    )
    
    lines(oregon_df$dates,
          oregon_df$daily_cases_per_hundy_avrg14d,
          col = "lightgreen")
    lines(montana_df$dates,
          montana_df$daily_cases_per_hundy_avrg14d,
          col = "orange")
    lines(california_df$dates,
          california_df$daily_cases_per_hundy_avrg14d,
          col = "brown")
    legend(
      "topleft",
      legend = c(
        "Pennsylvania",
        "Maryland",
        "Virginia",
        washington_s_txt,
        oregon_s_txt,
        montana_s_txt,
        california_s_txt
      ),
      col = c(
        "darkblue",
        "red",
        "gold",
        "darkgreen",
        "lightgreen",
        "orange",
        "brown"
      ),
      lty = 1
    )
    mtext(paste("created", format(Sys.Date(), "%m/%d/%Y")), side = 3)
    
  dev.off()
  file_to_bucket(filename)
  
