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
#  print(shift)
#  print(coordinates(object))
  object %>% elide(rotate = rot) %>%
    elide(scale = max(apply(bbox(object), 1, diff)) / scale) %>%
    elide(shift = shift)
}

load_state_shapefile <- function(loc, layer) {

  sf_in <- readOGR(dsn = loc, layer = layer, verbose = FALSE) %>% spTransform(CRS("+init=epsg:2163"))
  
#  data <- sf_in$NAME
#  data
  
  alaska <- sf_in[sf_in$NAME == "Alaska", ] %>%
    transform_state(-35, 2.5, c(-2400000, -2100000))
  proj4string(alaska) <- proj4string(sf_in)
  
  hawaii <- sf_in[sf_in$NAME == "Hawaii", ] %>%
    transform_state(-35, .75, c(-1170000,-2363000))
  proj4string(hawaii) <- proj4string(sf_in)
  
  dc <- sf_in[sf_in$NAME == "District of Columbia", ] %>%
    transform_state(0, .1, c(2600000,-700000))
  proj4string(dc) <- proj4string(sf_in)

  pr <- sf_in[sf_in$NAME == "Puerto Rico", ] %>%
    transform_state(0, .5, c(2500000,-1300000))
  proj4string(pr) <- proj4string(sf_in)
  
  guam <- sf_in[sf_in$NAME == "Guam", ] %>%
    transform_state(0, .5, c(-2200000,-1300000))
  proj4string(guam) <- proj4string(sf_in)
  
  as <- sf_in[sf_in$NAME == "American Samoa", ] %>%
    transform_state(0, 1, c(5500000,2200000))
  proj4string(as) <- proj4string(sf_in)
  
  usvi <- sf_in[sf_in$NAME == "United States Virgin Islands", ] %>%
    transform_state(0, .5, c(2800000,-1800000))
  proj4string(usvi) <- proj4string(sf_in)
  
  nmi <- sf_in[sf_in$NAME == "Commonwealth of the Northern Mariana Islands", ] %>%
    transform_state(0, .5, c(3000000,-300000))
  proj4string(nmi) <- proj4string(sf_in)
  
  thing_almost <-
    sf_in[!sf_in$NAME %in% c("Alaska","Hawaii", "Guam", "Commonwealth of the Northern Mariana Islands", "United States Virgin Islands", "Puerto Rico", "American Samoa"), ] %>%
    rbind(alaska) %>%
    rbind(hawaii) %>%
    rbind(dc) %>%
    rbind(pr) %>%
    rbind(as) %>%
    rbind(nmi) %>%
    rbind(usvi) %>%
    rbind(guam) %>%
    spTransform(CRS("+init=epsg:4326")) %>%
    fortify(region = "NAME") %>%
    mutate(id = tolower(id))
  
  thing <-
    ggplot(data = thing_almost,
           mapping = aes(
             x = long,
             y = lat,
             group = factor(group)
           )) +
    geom_polygon(color = "black") +
    coord_fixed(1.3)
  
  print(thing)
}


loc <- "/Users/willey/Desktop/cb_2020_us_all_5m/cb_2020_us_state_5m/cb_2020_us_state_5m.shp"
layer <- "cb_2020_us_state_5m"
load_state_shapefile(loc, layer())


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