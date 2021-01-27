# PLOT MAP FOR CNHS STUDY PROTOCOL

# First download sf file for Kiribati from https://gadm.org/download_country_v3.html

library(sf)
datadir <- "Data for mapping"
outdir <- "Outputs"
kir_sf <- readRDS(file = file.path(datadir, "gadm36_KIR_0_sf.rds"))
st_geometry(kir_sf)
# Geographic CRS (coordinate reference system) WGS 84 - World Geodetic System - this is the system used by GPS
# WGS 84 equal to EPSG code: 4326

# Resolution is only at the country level (i.e., no attribute to allow for easy subsetting by island name)
plot(st_geometry(kir_sf))

# Looks like rnaturalearth package also doesn't have island-level info for Kiribati
library(rnaturalearth)
kir_ne <- ne_countries(scale = 50, returnclass = "sf", type = "countries", country = "Kiribati")

# 

# Try using Jacob's site info to get bounding box for each island and use this to subset each island
site_info <- read.csv(file = file.path(datadir, "Kiribaiti NSF Coordinates FINAL Eurich - all data.csv")) %>%
  filter(Island != "") %>% # Remove blank rows
  filter(Description %in% c("Ciguatera", "Miscellaneous")==FALSE) # remove misc and ciguatera sites
islands <- sort(unique(site_info$Island))

for (i in 1:length(islands)){
  island_i <- islands[i]
  bounds <- site_info %>%
    filter(Island == island_i) %>%
    select(Lat, Long) %>%
    summarise(max_lat = max(Lat),
              min_lat = min(Lat),
              max_long = max(Long),
              min_long = min(Long))
  box = c(xmin = bounds$min_long, xmax = bounds$max_long, ymin = bounds$min_lat, ymax = bounds$max_lat)
  # FIX IT - what's the difference between projecting this to CRS and using geom_sf vs keeping as points and using geom_point
  dive_sites <- site_info %>%
    filter(Island == island_i) %>%
    filter(Description == "Site") %>%
    select(Lat, Long) #%>%
    #st_as_sf(coords = c("Long", "Lat")) %>%
    #st_set_crs(value = 4326)
  villages <- site_info %>%
    filter(Island == island_i) %>%
    filter(Description == "Village") %>%
    select(Lat, Long, Name_cleaned)

  # Use box to crop island out of kir_sf
  island_crop <- st_crop(st_geometry(kir_sf), box)
  p <- ggplot() +
    geom_sf(data = island_crop) +
    geom_point(data = villages, aes(x = Long, y = Lat), color = "red", size = 2) +
    geom_label(data = villages, aes(x = Long, y = Lat, label = Name_cleaned), fontface = "italic", nudge_y = 0.008) +
    geom_point(data = dive_sites, aes(x = Long, y = Lat)) +
    theme_bw() +
    labs(title = island_i, x = "", y = "")
  print(p)
  ggsave(filename = file.path(outdir, paste("map_", island_i, ".png", sep = "")), width = 8, height = 8)
}

