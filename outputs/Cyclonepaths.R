# ---------------------------
# LOAD LIBRARIES
# ---------------------------
packages <- c("ggplot2","dplyr","readr","maps")
for (p in packages) {
  if (!require(p, character.only = TRUE)) install.packages(p)
  library(p, character.only = TRUE)
}

# ---------------------------
# LOAD DATA
# ---------------------------
cyclone <- read_csv("D:/final_era5.csv", show_col_types = FALSE)

names(cyclone) <- tolower(names(cyclone))

# ---------------------------
# FIX LAT/LON
# ---------------------------
if ("lat" %in% names(cyclone)) cyclone$latitude <- cyclone$lat
if ("lon" %in% names(cyclone)) cyclone$longitude <- cyclone$lon

# ---------------------------
# CLEAN DATA
# ---------------------------
cyclone <- cyclone %>%
  mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) %>%
  filter(!is.na(latitude), !is.na(longitude))

# ---------------------------
# CREATE SIMPLE TRACK GROUP
# ---------------------------
cyclone <- cyclone %>%
  mutate(cyclone_id = cumsum(c(1, diff(latitude) > 2 | diff(longitude) > 2)))

# ---------------------------
# LOAD WORLD MAP
# ---------------------------
world <- map_data("world")

# ---------------------------
# CYCLONE PATH MAP (FLAT)
# ---------------------------
ggplot() +
  geom_polygon(
    data = world,
    aes(long, lat, group = group),
    fill = "gray95",
    color = "gray70",
    linewidth = 0.2
  ) +
  
  # 🔵 CYCLONE PATH (LIGHT BLUE, VERY THIN)
  geom_path(
    data = cyclone,
    aes(longitude, latitude, group = cyclone_id),
    color = "lightblue",
    linewidth = 0.3,
    alpha = 0.6,
    lineend = "round"
  ) +
  
  coord_fixed(1.3) +   # flat world map
  
  theme_void() +       # removes box/grid completely
  
  labs(
    title = "Cyclone Path Representation (Flat World Map)"
  )
