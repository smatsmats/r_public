KEEP_FILES <- TRUE
key = NULL
midpoint = "mean"
lowpoint = NULL
trans = NULL
border1_color = NULL
border1_df = NULL
border2_color = NULL
border2_df = NULL
caption = NULL
filebase = NULL

df = states_merged_usm
var = "avrg14_per_hundy"
key = "full"
lowpoint = 0
base = states_base_usm
border1_color = "grey"
border1_df = states_usm
border2_df = usa_usm
title = paste("USA", main_daily_cases_hundy_14d_avrg_txt, "States")
filebase = "mymap1.jpg"


usmap::usmap_crs()@projargs


