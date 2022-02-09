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

# use tigris for pullng cb shapfiles
library("tigris")
options(tigris_use_cache = TRUE)

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

mygraphs <- function() {
  # build all washington counties
  build_all_counties(
    state = "Washington",
    keep_dfs = TRUE,
    write_dfs = TRUE,
    plot_ref_and = TRUE,
    ref_county = "King",
    plot_state_cases_per_hundy = TRUE,
    plot_daily_cases = TRUE
  )

  if (USA_ALL) {
    usa_cases <- build_all_states(
      combined = TRUE,
      keep_dfs = TRUE,
      write_dfs = TRUE,
      plot_state_cases_per_hundy = TRUE,
      plot_wa_and = TRUE,
      plot_daily_cases = TRUE
    )
    make_plot(
      usa_cases,
      loc_txt = "US",
      cases_per_hundy = TRUE,
      daily_cases = TRUE,
      file_base = "US"
    )
    file_to_bucket("USA_cases_per_hundy.jpg")
    file_to_bucket("USA_daily_cases.jpg")
  }

  b_co_cases <-
    get_admin2(state = "Maryland", county = "Baltimore")
  b_ci_cases <-
    get_admin2(state = "Maryland", county = "Baltimore City")
  mad_cases <- get_admin2(state = "Virginia", county="Madison")
  pennsylvania_allegheny_df <-
    get_admin2(state = "Pennsylvania", county="Allegheny")

  ca_bc_cases <-
    get_admin1(country = "Canada", admin1 = "British Columbia")
  ca_bc_txt <-
    paste("British Columbia (pop=",
          pop_format(ca_bc_cases$pop[1]),
          ")",
          sep = "")
  ca_on_cases <- get_admin1(country = "Canada", admin1 = "Ontario")

  b_co_txt <-
    paste("Baltimore County, MD (pop=",
          pop_format(b_co_cases$pop[1]),
          ")",
          sep = "")
  b_ci_txt <-
    paste("Baltimore City, MD (pop=",
          pop_format(b_ci_cases$pop[1]),
          ")",
          sep = "")
  all_txt <-
    paste("Allegheny County, PA (pop=",
          pop_format(pennsylvania_allegheny_df$pop[1]),
          ")",
          sep = "")
  mad_txt <-
    paste("Madison County, VA (pop=",
          pop_format(mad_cases$pop[1]),
          ")",
          sep = "")
  if (USA_ALL) {
    usa_txt <-
      paste("US (pop=", pop_format(usa_cases$pop[1]), ")", sep = "")
  }
  #  india_txt <-
  #    paste("India (pop=", pop_format(india$pop[1]), ")", sep = "")

  ##############################################################################
  # manually combine into data frames to use for graphs
  # fix this

  temp_df_cases <- data.frame(
    dates = washington_df$dates,
    wa = washington_df$cases_per_hundy,
    ic = washington_island_df$cases_per_hundy,
    kc = washington_king_df$cases_per_hundy,
    yak = washington_yakima_df$cases_per_hundy,
    all = pennsylvania_allegheny_df$cases_per_hundy,
    mad = mad_cases$cases_per_hundy,
    sji = washington_san_juan_df$cases_per_hundy,
    sno = washington_snohomish_df$cases_per_hundy,
    ska = washington_skagit_df$cases_per_hundy,
    jeff = washington_jefferson_df$cases_per_hundy,
    kit = washington_kitsap_df$cases_per_hundy,
    b_co = b_co_cases$cases_per_hundy,
    b_ci = b_ci_cases$cases_per_hundy,
    usa = usa_cases$cases_per_hundy
  )
  temp_df_avrg <- data.frame(
    dates = washington_df$dates,
    or = oregon_df$daily_cases_per_hundy_avrg14d,
    mi = michigan_df$daily_cases_per_hundy_avrg14d,
    wa = washington_df$daily_cases_per_hundy_avrg14d,
    washington = washington_df$daily_cases_per_hundy_avrg14d,
    ic = washington_island_df$daily_cases_per_hundy_avrg14d,
    kc = washington_king_df$daily_cases_per_hundy_avrg14d,
    ca = california_df$daily_cases_per_hundy_avrg14d,
    md = maryland_df$daily_cases_per_hundy_avrg14d,
    id = idaho_df$daily_cases_per_hundy_avrg14d,
    ca_bc = ca_bc_cases$daily_cases_per_hundy_avrg14d,
    b_co = b_co_cases$daily_cases_per_hundy_avrg14d,
    b_ci = b_ci_cases$daily_cases_per_hundy_avrg14d,
    all = pennsylvania_allegheny_df$daily_cases_per_hundy_avrg14d,
    mad = mad_cases$daily_cases_per_hundy_avrg14d,
    sji = washington_san_juan_df$daily_cases_per_hundy_avrg14d,
    sno = washington_snohomish_df$daily_cases_per_hundy_avrg14d,
    ska = washington_skagit_df$daily_cases_per_hundy_avrg14d,
    jeff = washington_jefferson_df$daily_cases_per_hundy_avrg14d,
    kit = washington_kitsap_df$daily_cases_per_hundy_avrg14d,
    usa = usa_cases$daily_cases_per_hundy_avrg14d,
    nebraska = nebraska_df$daily_cases_per_hundy_avrg14d,
    south_dakota = south_dakota_df$daily_cases_per_hundy_avrg14d,
    montana = montana_df$daily_cases_per_hundy_avrg14d,
    wyoming = wyoming_df$daily_cases_per_hundy_avrg14d,
    colorado = colorado_df$daily_cases_per_hundy_avrg14d,
    kansas = kansas_df$daily_cases_per_hundy_avrg14d,
    missouri = missouri_df$daily_cases_per_hundy_avrg14d,
    iowa = iowa_df$daily_cases_per_hundy_avrg14d,
    florida = florida_df$daily_cases_per_hundy_avrg14d,
    louisiana = louisiana_df$daily_cases_per_hundy_avrg14d,
    alabama = alabama_df$daily_cases_per_hundy_avrg14d,
    arkansas = arkansas_df$daily_cases_per_hundy_avrg14d,
    georgia = georgia_df$daily_cases_per_hundy_avrg14d,
    texas = texas_df$daily_cases_per_hundy_avrg14d,
    wh = washington_whitman_df$daily_cases_per_hundy_avrg14d
  )
  temp_df_sum <- data.frame(
    dates = washington_king_df$dates,
    kc = washington_king_df$daily_cases_per_hundy_sum14d,
    ic = washington_island_df$daily_cases_per_hundy_sum14d,
    wa = washington_df$daily_cases_per_hundy_sum14d,
    ska = washington_skagit_df$daily_cases_per_hundy_sum14d,
    kit = washington_kitsap_df$daily_cases_per_hundy_sum14d,
    sno = washington_snohomish_df$daily_cases_per_hundy_sum14d,
    b_co = b_co_cases$daily_cases_per_hundy_sum14d
  )

  ##############################################################################
  filename = "my_perhundy_select.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)

  linetypes <- c(
    "wa" = "solid",
    "mt" =  "solid",
    "ic" = "solid",
    "kc" = "solid",
    "yak" = "solid",
    "all" = "solid",
    "mad" = "solid",
    "sji" = "solid",
    "jeff" = "solid",
    "kit" = "solid",
    "usa" = "dashed",
    "b_co" = "solid",
    "b_ci" = "solid"
  )

  p <-
    ggplot(data = temp_df_cases, aes(dates, linetypes = "linetypes")) +
    geom_line(aes(y = ic, colour = washington_island_txt), linetype = "solid") +
    geom_line(aes(y = kc, colour = washington_king_txt), linetype = "solid") +
    geom_line(aes(y = wa, colour = washington_s_txt), linetype = "solid") +
    geom_line(aes(y = b_co, colour = b_co_txt), linetype = "solid") +
    geom_line(aes(y = b_ci, colour = b_ci_txt), linetype = "solid") +
    geom_line(aes(y = kit, colour = washington_kitsap_txt), linetype = "solid") +
    geom_line(aes(y = jeff, colour = washington_jefferson_txt), linetype = "solid") +
    geom_line(aes(y = sji, colour = washington_san_juan_txt), linetype = "solid") +
    geom_line(aes(y = ska, colour = washington_skagit_txt), linetype = "solid") +
    geom_line(aes(y = sno, colour = washington_snohomish_txt), linetype = "solid") +
#    geom_line(aes(y = yak, colour = yak_txt), linetype = "solid") +
    geom_line(aes(y = all, colour = all_txt), linetype = "solid") +
    geom_line(aes(y = mad, colour = mad_txt), linetype = "solid") +
    geom_line(aes(y = usa, colour = usa_txt), linetype = "dashed") +
    scale_color_manual(
      values = c(
        "black",
        "darkred",
        "red",
        "blue",
        "pink",
        "green",
        "grey",
        "lightblue",
        "purple",
#        "turquoise",
        "gold",
        "chocolate3",
        "red",
        "darkgreen"
#        "yellow"
      )
    ) +
    #    scale_linetype_manual( values = c("solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "solid", "dashed", "solid", "solid")) +
    #      scale_linetype_manual( values = linetypes ) +
    ylim(0, max(usa_cases$cases_per_hundy, na.rm = TRUE)) +
    labs(title = main_cases_hundy_txt,
         subtitle = paste("created", 
                          format(Sys.time(), 
                                 "%m/%d/%Y %H:%M:%S")),
         x = "Dates",
         y = ylab_cases_hundy_txt) +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      #            panel.grid.major = element_blank(),
      panel.background = element_blank(),
      panel.border = element_blank(),
      axis.line = element_line(),
      plot.title = element_text(hjust = 0.5),
      plot.subtitle = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0.5),
      legend.title = element_blank(),
      legend.position = c(0.35, 0.80),
      legend.background = element_rect(
        linetype = "solid",
        size = 0.2,
        colour = "black"
      )
    )
  print(p)

  dev.off()
  file_to_bucket(filename)

  ##############################################################################
  # apple cup
  # daily rates
  filename = "apple_cup_daily.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)

  maxy = max(washington_whitman_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)

  p <- ggplot(data = temp_df_avrg, aes(dates)) +
    geom_line(aes(y = kc, colour = washington_king_txt)) +
    geom_line(aes(y = wh, colour = washington_whitman_txt)) +
    scale_color_manual(values = c("purple", "darkred")) +
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

  ##############################################################################
  filename = "uw_v_wsu.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)

  apple_cup <- data.frame(washington_whitman_df$dates,
                          washington_whitman_df$cases_per_hundy,
                          washington_king_df$cases_per_hundy)
  p <- ggplot(data = apple_cup, aes(washington_whitman_df.dates)) +
    geom_line(aes(y = washington_whitman_df.cases_per_hundy,
                  colour = washington_whitman_txt)) +
    geom_line(aes(y = washington_king_df.cases_per_hundy,
                  colour = washington_king_txt)) +
    scale_color_manual(values = c("purple", "darkred")) +
    ylim(0, max(apple_cup$washington_whitman_df.cases_per_hundy)) +
    labs(
      title = paste(cumulative_c19_cases_txt, hundy_txt),
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

  ##############################################################################
  wa_east_west(
    plot_casesned = TRUE,
    plot_casesned_cases = FALSE,
    file_base = "wa_east_west"
  )
  file_to_bucket(file = "wa_east_west_cases_per_hundy.jpg")

  ##############################################################################

  # 14 day moving plots
  make_plot(
    loc_txt = washington_island_txt,
    df = washington_island_df,
    daily_cases = TRUE,
    file_base = "island_wa"
  )
  file_to_bucket(file = "island_wa_daily_cases.jpg")
  make_plot(
    loc_txt = washington_king_txt,
    df = washington_king_df,
    daily_cases = TRUE,
    file_base = "king_wa"
  )
  file_to_bucket(file = "king_wa_daily_cases.jpg")
  make_plot(
    loc_txt = b_co_txt,
    df = b_co_cases,
    daily_cases = TRUE,
    file_base = "balto_co_md"
  )
  file_to_bucket(file = "balto_co_md_daily_cases.jpg")

  ##############################################################################
  filename = "is_king_balto.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  maxy = max(b_co_cases$daily_cases_per_hundy_avrg14d, na.rm = TRUE)

  p <- ggplot(data = temp_df_avrg, aes(dates)) +
    geom_line(aes(y = kc, colour = washington_king_txt)) +
    geom_line(aes(y = ic, colour = washington_island_txt)) +
    geom_line(aes(y = b_co, colour = b_co_txt)) +
    scale_color_manual(values = c("red", "blue", "green")) +
    ylim(0, maxy) +
    labs(
      title = main_daily_cases_hundy_14d_avrg_txt,
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

  ##############################################################################
  filename <- "is_king_wa.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)
  maxy <-
    max(washington_df$daily_cases_per_hundy_avrg14d, 
        washington_island_df$daily_cases_per_hundy_avrg14d,
        washington_king_df$daily_cases_per_hundy_avrg14d,
        na.rm = TRUE)

  cat("max y ", maxy, "\n")
  p <- ggplot(data = temp_df_avrg, aes(dates)) +
    geom_line(aes(y = wa, colour = washington_s_txt)) +
    geom_line(aes(y = ic, colour = washington_island_txt)) +
    geom_line(aes(y = kc, colour = washington_king_txt)) +
    scale_color_manual(values = c("blue", "green", "darkgreen")) +
    ylim(0, maxy) +
    labs(
      title = main_daily_cases_hundy_14d_avrg_txt,
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

  ##############################################################################
  filename = "is_king_wa_sum.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)

  maxy = max(washington_skagit_df$daily_cases_per_hundy_sum14d,
             washington_snohomish_df$daily_cases_per_hundy_sum14d,
             washington_island_df$daily_cases_per_hundy_sum14d,
             washington_kitsap_df$daily_cases_per_hundy_sum14d,
             washington_king_df$daily_cases_per_hundy_sum14d,
             na.rm = TRUE)
  cat("max y ", maxy, "\n")
  today <- Sys.Date()
  start_graph <- today - months(2)

  p <- ggplot(data = temp_df_sum, aes(dates)) +
    geom_line(aes(y = sno, colour = washington_snohomish_txt)) +
    geom_line(aes(y = ic, colour = washington_island_txt)) +
    geom_line(aes(y = ska, colour = washington_skagit_txt)) +
    geom_line(aes(y = kc, colour = washington_king_txt)) +
    geom_line(aes(y = kit, colour = washington_kitsap_txt)) +
    geom_vline(xintercept = as.Date(c("2021/04/24", "2021/04/09")), linetype =
                 "dashed") +
    geom_hline(yintercept = 200, linetype = "dashed") +
    # geom_hline(yintercept = c(200, 150)) +
    scale_color_manual(values = c("blue", "green", "yellow", "purple", "pink")) +
    ylim(0, maxy) +
    scale_x_date(
      date_breaks = "3 week",
      labels = date_format("%b-%d-%Y"),
      limits = c(start_graph, today),
    ) +
    labs(
      title = main_daily_cases_hundy_14d_sum_txt,
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

  ##############################################################################
  # MISC graphs
  # THE SOUTH
  filename = "thesouth.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)

  bigger = ifelse(louisiana_df$daily_cases_per_hundy_avrg14d > florida_df$daily_cases_per_hundy_avrg14d,
                  louisiana_df$daily_cases_per_hundy_avrg14d, florida_df$daily_cases_per_hundy_avrg14d)
  maxy = max(bigger, na.rm = TRUE)

  p <- ggplot(data = temp_df_avrg, aes(dates)) +
    geom_line(aes(y = florida, colour = florida_s_txt)) +
    geom_line(aes(y = louisiana, colour = louisiana_s_txt)) +
    geom_line(aes(y = alabama, colour = alabama_s_txt)) +
    geom_line(aes(y = arkansas, colour = arkansas_s_txt)) +
    geom_line(aes(y = georgia, colour = georgia_s_txt)) +
    geom_line(aes(y = texas, colour = texas_s_txt)) +
    geom_line(aes(y = washington, colour = washington_s_txt)) +
#    scale_color_manual(values = c("black", "orange", "lightgreen", "red", "darkgreen")) +
#    scale_linetype_manual(values = c("solid", "solid", "solid", "dashed", "solid")) +
    labs(
      title = paste("The South", main_daily_cases_hundy_14d_avrg_txt),
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

  ##############################################################################
  # MISC graphs
  # multiple counties 14 day
  filename = "misc.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)

  maxy = max(montana_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)

  p <- ggplot(data = temp_df_avrg, aes(dates)) +
    geom_line(aes(y = or, colour = oregon_s_txt)) +
    geom_line(aes(y = wa, colour = washington_s_txt)) +
    geom_line(aes(y = montana, colour = montana_s_txt)) +
    geom_line(aes(y = mi, colour = michigan_s_txt)) +
    #      geom_line(aes(y = india, colour=india_txt)) +
    geom_line(aes(y = usa, colour = usa_txt), linetype = "dashed") +
    scale_color_manual(values = c("black", "orange", "lightgreen", "red", "darkgreen")) +
    scale_linetype_manual(values = c("solid", "solid", "solid", "dashed", "solid")) +
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


  ##############################################################################
  # MISC2222222222222222222222
  # multiple counties 14 day
  filename = "misc2a.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)

  maxy = max(california_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)

  p <- ggplot(data = temp_df_avrg, aes(dates)) +
    geom_line(aes(y = or, colour = oregon_s_txt)) +
    geom_line(aes(y = wa, colour = washington_s_txt)) +
    geom_line(aes(y = ca_bc, colour = ca_bc_txt)) +
    geom_line(aes(y = id, colour = idaho_s_txt)) +
    geom_line(aes(y = ca, colour = california_s_txt)) +
    scale_color_manual(values = c("lightblue", "pink", "brown", "lightgreen", "darkgreen")) +
    ylim(0, maxy) +
    labs(
      title = main_daily_cases_hundy_14d_avrg_txt,
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

  ##############################################################################
  # MISC2222222222222222222222bbbb
  # multiple counties 14 day
  filename = "misc2.jpg"
  jpeg(filename = filename,
       width = plot_file_width,
       height = plot_file_height)

  maxy = max(south_dakota_df$daily_cases_per_hundy_avrg14d, na.rm = TRUE)

  p <- ggplot(data = temp_df_avrg, aes(dates)) +
    geom_line(aes(y = nebraska, colour = nebraska_s_txt)) +
    geom_line(aes(y = south_dakota, colour = south_dakota_s_txt)) +
    geom_line(aes(y = wyoming, colour = wyoming_s_txt)) +
    geom_line(aes(y = colorado, colour = colorado_s_txt)) +
    #      geom_line(aes(y = kansas, colour = kansas_s_txt)) +
    geom_line(aes(y = missouri, colour = missouri_s_txt)) +
    geom_line(aes(y = iowa, colour = iowa_s_txt)) +
    geom_line(aes(y = washington, colour = washington_s_txt)) +
    #     scale_color_manual(values = c("lightblue", "pink", "brown", "lightgreen", "darkgreen", "black", "red")) +
    ylim(0, maxy) +
    labs(
      title = paste("Really Nebraska?", main_daily_cases_hundy_14d_avrg_txt),
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

}  #mygraphs
