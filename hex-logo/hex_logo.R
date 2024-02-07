# Create hex logo
library(tidyverse)

area <- oceandatr::get_area(area_name = "Bermuda",  mregions_column = "territory1")
projection <- '+proj=laea +lon_0=-64.8108333 +lat_0=32.3571917 +datum=WGS84 +units=m +no_defs'

# Create a planning grid
planning_sf <- spatialgridr::get_grid(area, projection = projection, resolution = 100000, option = "sf_hex")

planning_sf <- planning_sf %>%
  bind_cols(sf::st_centroid(planning_sf) %>%
  sf::st_coordinates(.) %>%
  as.data.frame() %>%
  dplyr::mutate(X = round(X, digits = 4),
                Y = round(Y, digits = 4)))

colors <- data.frame("Y" = unique(planning_sf$Y)) %>%
  arrange(desc(Y)) %>%
  mutate(color = seq(0, 1, length.out = nrow(.)))

planning_sf <- planning_sf %>%
  left_join(colors)

p <- ggplot() +
  geom_sf(data = planning_sf, mapping = aes(color = color), fill = NA) +
  geom_sf(data = planning_sf[c(49,50,43, 31, 32, 25, 24),], color = NA, fill = "#01665e", alpha = 0.7) +
  scale_color_gradient2(high = "#003c30", low = "white", midpoint = 0.2) +
  theme_void() +
  theme(legend.position = "none")

hexSticker::sticker(p,
                    package = "patchwise",
                    p_size=20,
                    p_y = 1.5, s_x=1.0, s_y=0.9, s_width=2.2, s_height=2.2,
                    p_color = "#003c30",
                    h_fill = "white",
                    h_color = "#003c30",
                    filename = "hex-logo/hex_logo.png",
                    white_around_sticker = TRUE) # to remove the excess plot

# need to go in manually and resave without white
