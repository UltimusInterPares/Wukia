# ### Testing Maps
# library(sf)
# 
# # Read in the file
# Greece <- sf::st_read("stanford-bw768dd5470-shapefile/bw768dd5470.shp")
# class(Greece)
# attr(Greece, "sf_column")
# 
# # Whole map
# # Sweet god it works
# ggplot(Greece) +
#   geom_sf()
# 
# # Trying to select for the regions in question
# # Good, but too much of the peloponnese
# ggplot(Greece[Greece$id_1 == c(3,7,8 ), ]) +
#   geom_sf()
# 
# # Trying more level-2 administrative regions
# # Better, but still too much peloponnese
# ggplot(Greece[Greece$id_2 == c(4,11,13 ), ]) +
#   geom_sf()
# 
# # Trying to remove peloponnese
# # Doesn't work, trying a level-1 division
# ggplot(Greece[Greece$id_2 == c(4,13 ), ]) +
#   geom_sf()
# 
# # Searching for Athens, the Isthmus, and Boeotia
# # Way off
# ggplot(Greece[Greece$id_1 == c(3,8), ]) +
#   geom_sf()
# 
# Greece2 <- sf::st_read("data/GRC_adm3.shp")
# class(Greece2)
# attr(Greece2, "sf_column")
# 
# Greece_select <- Greece2[Greece2$ID_1 == c(3,7,8), ]
# # I'm testing if I can call multiple geom_sf() objects to form a 
# # continuous map
# 
# # First, the full map
# # Nothing?
# ggplot(Greece_select) +
#   geom_sf()
# 
# # Wildly off from what I want
# ggplot(Greece_select[Greece_select$ID_1 == c(3,7,8), ]) +
#   geom_sf()
# 
# # Trying without selecting
# # Similar results, though a bit better
# ggplot(Greece2[Greece2$ID_1 == c(3,7,8), ]) +
#   geom_sf()
# 
# # Going to see what the full thing gives me
# # WOW THAT'S A LOT
# ggplot(Greece2) +
#   geom_sf()
# #### Trying the method here:
# # https://milospopovic.net/crisp-topography-map-with-r/
# # Load libraries
# library(elevatr)
# library(terra)
# library(tidyverse)
# library(sf)
# library(giscoR)
# library(marmap)
# library(raster) # Along with elevatr, for making line maps
# 
# # 1. GET COUNTRY MAP
# #---------
# 
# crsLONGLAT <- "+proj=longlat +datum=WGS84 +no_defs"
# 
# get_sf <- function(country_sf, country_transformed) {
#   
#   country_sf <- giscoR::gisco_get_countries(
#     year = "2016",
#     epsg = "4326",
#     resolution = "10",
#     country = "Greece")
#   
#   country_transformed <- st_transform(country_sf, crs = crsLONGLAT)
#   
#   return(country_transformed)
# }
# 
# country_transformed <- get_sf() 
# 
# # 2. GET ELEVATION DATA
# #---------
# 
# get_elevation_data <- function(country_elevation, country_elevation_df) {
#   
#   country_elevation <- elevatr::get_elev_raster(
#     locations = country_transformed, 
#     z = 8, # Please decrease the z value if you experience R crashing
#     clip = "locations") 
#   
#   country_elevation_df <- as.data.frame(country_elevation, xy = T) %>%
#     na.omit()
#   
#   colnames(country_elevation_df)[3] <- "elevation"
#   
#   return(country_elevation_df)
# }
# 
# country_elevation_df <- get_elevation_data()
# 
# # 3. MAP
# #---------
# 
# get_elevation_map <- function(country_map) {
#   
#   country_map <- ggplot() +
#     # Remove _select to get full country
#     geom_tile(data = country_elevation_df_select, 
#               aes(x = x, y = y, fill = elevation)) +
#     scale_fill_etopo() +
#     coord_sf(crs = crsLONGLAT)+
#     theme_minimal() +
#     theme(text = element_text(color = "#22211d"),
#           #axis.line = element_blank(),
#           #axis.text.x = element_blank(),
#           #axis.text.y = element_blank(),
#           #axis.ticks = element_blank(),
#           #axis.title.x = element_blank(),
#           #axis.title.y = element_blank(),
#           legend.position = "none",
#           #panel.grid.major = element_line(color = "white", size = 0.2),
#           #panel.grid.minor = element_blank(),
#           plot.title = element_text(size=18, color="grey20", hjust=1, vjust=-5),
#           plot.caption = element_text(size=8, color="grey70", hjust=.15, vjust=20),
#           plot.margin = unit(c(t=0, r=0, b=0, l=0),"lines"), #added these narrower margins to enlarge maps
#           plot.background = element_rect(fill = "white", color = NA), 
#           panel.background = element_rect(fill = "white", color = NA),
#           panel.border = element_blank()) +
#     labs(x = "", 
#          y = NULL, 
#          title = "Topographic map of Greece", 
#          subtitle = "", 
#          caption = "")
#   
#   return(country_map)
# }
# 
# country_map <- get_elevation_map()
# 
# ggsave(filename="greece_topo_map2.png", width=7, height=8.5, dpi = 600, device='png', country_map)
# 
# # This properly selects for Attica
# country_elevation_df_select <- country_elevation_df[country_elevation_df$x >= 22.5, ]
# country_elevation_df_select <- country_elevation_df_select[country_elevation_df_select$x <= 24.5, ]
# country_elevation_df_select <- country_elevation_df_select[country_elevation_df_select$y >= 37.5, ]
# country_elevation_df_select <- country_elevation_df_select[country_elevation_df_select$y <= 38.5, ]
# 
# ggsave(filename="central_greece.png", width=7, height=8.5, dpi = 600, device='png', country_map)

### Creating A Map --------------------------------------------------------
# Trying elevatr + raster
library(tidyverse)
library(elevatr)
library(raster) # Along with elevatr, for making line maps
library(leastcostpath)

# Generate a data frame of lat/long coordinates.
ex.df <- data.frame(x=seq(from=22.5, to=24.5, length.out=10), 
                    y=seq(from=37.5, to=38.5, length.out=10))

# Specify projection.
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Use elevatr package to get elevation data for each point.
elev <- get_elev_raster(ex.df, prj = prj_dd, z = 14, clip = "bbox")

raster::contour(elev)

# Going to try removing negative values
# Omfg it works
elev[elev<=-200] <- NA

# Athens 37.972453603, 23.726268222
# Eleusis 38.041101, 23.537401

# Megara 37.985078, 23.340163
# Pagai 38.084923, 23.197116
# Aigosthena 38.1497606, 23.2294868

# Tanagra 38.3001983333, 23.581173
# Ororpus 38.3194635, 23.7899985
# Make DF of cities
cities <- tibble(x = c(23.726268222, 23.537401, 23.340163, 23.197116, 23.2294868, 23.581173, 23.7899985),
                 y = c(37.972453603, 38.041101, 37.985078, 38.084923, 38.1497606, 38.3001983333, 38.3194635),
                 names = c("Athens", "Eleusis", "Megara", "Pagai", "Aigosthena", "Tanagra", "Oropus"),
                 colors = c("#56B4E9", "#56B4E9", "#E69F00", "#E69F00", "#E69F00", "#009E73", "#009E73"))

# Plot Elevation
plot(elev)
# Layer contour map
contour(elev,
        frame.plot = F,
        col = terrain.colors(n=10),
        xlab = "Latitude",
        ylab = "Longitude")
# Plot cities
points(
  x = cities$x,
  y = cities$y,
  col = "black",
  pch = 19,
  xlim = c(23, 24.5),
  ylim = c(37.5, 38.5),
  title(main = "Central Greece",
        sub = "Elevation in Meters")
)
# Label cities
text(x = cities$x,
     y = cities$y,
     labels = cities$names,
     col = "black",
     pos = 1)

# Lets try a least cost path analysis
cs <- create_slope_cs(dem = elev,
                      cost_function = 'tobler',
                      neighbours = 16) %>%
  "*" (create_traversal_cs(dem = elev,
                           neighbours = 16))

Ath = cbind(23.726268222, 37.972453603)
Ath = sp::SpatialPoints(Ath)

Oro = cbind(23.7899985, 38.3194635)
Oro = sp::SpatialPoints(Oro)

Tan = cbind(23.581173, 38.3001983333)
Tan = sp::SpatialPoints(Tan)

lcp <- create_lcp(cost_surface = cs, origin = Ath, destination = Oro, directional = FALSE)
lcp2 <- create_lcp(cost_surface = cs, origin = Ath, destination = Tan, directional = FALSE)

plot(raster(cs), add = T)
plot(lcp[1], add = T, col = "red")
plot(lcp2[1], add = T, col = "blue")


### Finding Midpoints -----------------------------------------------------
# Testing with anticipated results for a/h isogloss

# Athens 37.972453603, 23.726268222
# Eleusis 38.041101, 23.537401

# Megara 37.985078, 23.340163
# Pagai 38.084923, 23.197116
# Aigosthena 38.1497606, 23.2294868

# Tanagra 38.3001983333, 23.581173
# Ororpus 38.3194635, 23.7899985

damos_x <- c(
  (23.726268222 + 23.340163)/2, # Athens v Megara
  (23.537401 + 23.340163)/2,    # Eleusis v Megara
  (23.537401 + 23.197116)/2,    # Eleusis v Pagai
  (23.537401 + 23.2294868)/2,   # Eleusis v Aigosthena
  (23.726268222 + 23.581173)/2, # Athens v Tanagra
  (23.581173 + 23.7899985)/2    # Tanagra v Oropus
  )
damos_y <- c(
  (37.972453603 + 37.985078)/2,     # Athens v Megara
  (38.041101 + 37.985078)/2,        # Eleusis v Megara
  (38.041101 + 38.084923)/2,        # Eleusis v Pagai
  (38.041101 + 38.1497606)/2,       # Eleusis v Aigosthena
  (37.972453603 + 38.3001983333)/2, # Ahens v Tanagra
  (38.3001983333 + 38.3194635)/2    # Tanagra v Oropus
  )

lines(x = damos_x, y = damos_y, col = "blue", lwd = 2)
