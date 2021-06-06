library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(stringr)


usa <- map_data("usa")
ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) +
  coord_fixed(1.3)

states <- map_data("state")
states_base <- ggplot(data = states) +
  geom_polygon(aes(x = long, y = lat, fill = region, group = group), color = "white") +
  coord_fixed(1.3) +
  guides(fill=FALSE)  # do this to leave off the color legend
states_base
states_base <- ggplot(data = states, mapping = aes(x = long, y = lat, group = factor(group))) +
  geom_polygon(aes(x = long, y = lat, fill = "grey"), color = "white") +
  coord_fixed(1.3)
states_base

# add Province_State to make merging easier
states$Province_State = str_to_title(states$region)
states_merged <- inner_join(states, us_states_wide, by = "Province_State")


west_coast <- subset(states, region %in% c("california", "oregon", "washington"))
ggplot(data = west_coast) +
  geom_polygon(aes(x = long, y = lat, group = group), fill = "palegreen", color = "black") +
  coord_fixed(1.3)


wa_df <- subset(states, region == "washington")
wa_base <- ggplot(data = wa_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")
wa_base + theme_nothing()

counties <- map_data("county")
wa_county <- subset(counties, region == "washington")
wa_base + theme_nothing() +
  geom_polygon(data = wa_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top




# make a combined key that matches our data
wa_county$Combined_Key <- paste(str_to_title(wa_county$subregion),
                                ", ",
                                str_to_title(wa_county$region),
                                ", US",
                                sep="")



wacopa <- inner_join(wa_county, us_counties_wide, by = "Combined_Key")


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

elbow_room1 <- wa_base +
  geom_polygon(data = wacopa, aes(fill = avrg14_per_hundy), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

elbow_room1
elbow_room1 + scale_fill_gradient(trans = "log10")
eb2 <- elbow_room1 +
  scale_fill_gradientn(colours = rev(rainbow(7)),
                       breaks = c(0, 2, 4, 10, 100),
                       trans = "log10")
eb2

trend1 <- wa_base +
  geom_polygon(data = wacopa, aes(fill = trend), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

trend1
#trend1 + scale_fill_gradient(trans = "log10")
mid <- mean(wacopa$trend)
trend2 <- trend1 +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white",
                       high = "red", space = "Lab" )
trend2


wa_base <- ggplot(data = wa_df, mapping = aes(x = long, y = lat, group = group)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black", fill = "gray")
wa_base + theme_nothing()
wa_base + theme_nothing() +
  geom_polygon(data = wa_county, fill = NA, color = "white") +
  geom_polygon(color = "black", fill = NA)  # get the state border back on top

states_base <- ggplot(data = states, mapping = aes(x = long, y = lat, group = factor(group))) +
  geom_polygon(color = "white") +
  coord_fixed(1.3)
states_base
states_14day_avgr <- states_base +
  geom_polygon(data = states_merged, aes(fill = avrg14_per_hundy), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes
states_14day_avgr
mid <- mean(states_merged$avrg14_per_hund)
states_14day_avgr1 <- states_14day_avgr +
  scale_fill_gradient2(midpoint = mid, low = "blue", mid = "white",
                       high = "red", space = "Lab" )
states_14day_avgr1

states_trend1 <- states_base +
  geom_polygon(data = states_merged, aes(fill = trend), color = "white") +
  geom_polygon(color = "black", fill = NA) +
  theme_bw() +
  ditch_the_axes

states_trend1
states_trend2 <- states_trend1 +
  scale_fill_gradient2(midpoint = 0, low = "blue", mid = "white",
                       high = "red", space = "Lab" )
states_trend2

