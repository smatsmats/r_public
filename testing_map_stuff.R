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



download.file("https://dl.dropbox.com/s/wl0z5rpygtowqbf/states_21basic.zip?dl=1", 
              "usmapdata.zip", 
              method = "curl")
# This is a mirror of http://www.arcgis.com/home/item.html?
# id=f7f805eb65eb4ab787a0a3e1116ca7e5
unzip("usmapdata.zip")

require(rgdal)
all_states <- readOGR("states_21basic/", "states")

require(ggplot2); require(maptools); require(rgeos); require(mapproj);
all_states <- fortify(all_states, region = "STATE_NAME")



ak <- map_data('state','alaska'
               )
xstates <- map_data("state")

transform_state <- function(object, rot, scale, shift){
  print(max(apply(bbox(object), 1, diff)) / scale)
  print(shift)
  print(coordinates(object))
  object %>% elide(rotate = rot) %>%
    elide(scale = max(apply(bbox(object), 1, diff)) / scale) %>%
    elide(shift = shift)
}


loc <- "/Users/willey/Desktop/states_21basic"
map <- readOGR(dsn="/Users/willey/Desktop/states_21basic", layer="states")

fifty_states_sp <- readOGR(dsn = loc, layer = "states", verbose = FALSE) %>% spTransform(CRS("+init=epsg:2163"))

alaska <- fifty_states_sp[fifty_states_sp$STATE_NAME == "Alaska", ] %>%
  transform_state(-35, 2.5, c(-2400000, -2100000))
proj4string(alaska) <- proj4string(fifty_states_sp)

hawaii <- fifty_states_sp[fifty_states_sp$STATE_NAME == "Hawaii", ] %>%
  transform_state(-35, .75, c(-1170000,-2363000))
proj4string(hawaii) <- proj4string(fifty_states_sp)

dc <- fifty_states_sp[fifty_states_sp$STATE_NAME == "District of Columbia", ] %>%
  transform_state(0, .1, c(2600000,-700000))
proj4string(dc) <- proj4string(fifty_states_sp)


fifty_states <-
  fifty_states_sp[!fifty_states_sp$STATE_NAME %in% c("Alaska","Hawaii"), ] %>%
  rbind(alaska) %>%
  rbind(hawaii) %>%
  rbind(dc) %>%
  
  spTransform(CRS("+init=epsg:4326")) %>%
  fortify(region = "STATE_NAME") %>%
  mutate(id = tolower(id))

base50 <-
  ggplot(data = fifty_states,
         mapping = aes(
           x = long,
           y = lat,
           group = factor(group)
         )) +
  geom_polygon(color = "black") +
  coord_fixed(1.3)

print(base50)

library("gpclib")
gpclibPermit()

