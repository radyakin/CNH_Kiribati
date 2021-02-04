# PLOT MAP FOR CNHS STUDY PROTOCOL

# First download sf file for Kiribati from https://gadm.org/download_country_v3.html
library(tidyverse)
library(sf)
library(ggsn) # for adding north arrows and scale bar
datadir <- "Data for mapping"
outdir <- "Outputs"

# Load country polygons
kir_sf <- readRDS(file = file.path(datadir, "gadm36_KIR_0_sf.rds"))
st_geometry(kir_sf)
# Geographic CRS (coordinate reference system) WGS 84 - World Geodetic System - this is the system used by GPS
# WGS 84 equal to EPSG code: 4326

# Resolution is only at the country level (i.e., no attribute to allow for easy subsetting by island name)
plot(st_geometry(kir_sf))

# Load village and reef info from table 1
village_info <- read.csv(file = file.path(datadir, "data_from_table_1.csv")) %>%
  select(-c(Lat, Lon)) # Original Table 1 lat/longs were incorrect; get this info from Jacob's table instead

# Looks like rnaturalearth package also doesn't have island-level info for Kiribati either
# library(rnaturalearth)
# kir_ne <- ne_countries(scale = 50, returnclass = "sf", type = "countries", country = "Kiribati")

# Load Jacob's ecological sites info and join with village_info
# Also Use Jacob's ecological site info to get bounding box for each island and use this to subset each island
site_info <- read.csv(file = file.path(datadir, "Kiribaiti NSF Coordinates FINAL Eurich - all data.csv")) %>%
  filter(Island != "") %>% # Remove blank rows
  filter(Description %in% c("Ciguatera", "Miscellaneous")==FALSE) %>% # remove misc and ciguatera sites
  mutate(Notes = case_when(Notes == "FR" ~ "Forereef",
                           Notes == "LR" ~ "Lagoon",
                           Notes == "MT" ~ "Manta Tow",
                           Notes == "SB" ~ "Backreef", 
                           Notes == "SIQ" ~ "Soft-bottom backreef",
                           TRUE ~ Notes)) %>%
  left_join(village_info, by = c("Name_cleaned" = "Community.Name", "Island" = "Island.Name"))

islands <- sort(unique(site_info$Island))

# Create custom buffer for each island - based on trial error of default "0" for buffer
island_buffer <- data.frame(island = islands, xmin_buff = NA, xmax_buff = NA, ymin_buff = NA, ymax_buff = NA) %>%
  mutate(xmin_buff = case_when(island == "Abaiang" ~ -0.02,
                               island == "Abemama" ~ 0,
                               island == "Butaritari" ~ 0,
                               island == "Kiritimati" ~ -0.5, 
                               island == "N Tabiteuae" ~ 0,
                               island == "Onotoa" ~ -0.02,
                               island == "S Tabiteuae" ~ 0,
                               island == "S Tarawa" ~ 0,
                               island == "Tabuaeran" ~ 0,
                               TRUE ~ 0)) %>%
  mutate(xmax_buff = case_when(island == "Abaiang" ~ 0.02,
                               island == "Abemama" ~ 0.02,
                               island == "Butaritari" ~ 0,
                               island == "Kiritimati" ~ 0.5, 
                               island == "N Tabiteuae" ~ 0.02,
                               island == "Onotoa" ~ 0.02,
                               island == "S Tabiteuae" ~ 0.02,
                               island == "S Tarawa" ~ 0.05,
                               island == "Tabuaeran" ~ 0.5,
                               TRUE ~ 0)) %>%
  mutate(ymin_buff = case_when(island == "Abaiang" ~ 0,
                               island == "Abemama" ~ -0.5,
                               island == "Butaritari" ~ 0,
                               island == "Kiritimati" ~ -0.5, 
                               island == "N Tabiteuae" ~ -0.5,
                               island == "Onotoa" ~ -0.02,
                               island == "S Tabiteuae" ~ -0.1,
                               island == "S Tarawa" ~ 0,
                               island == "Tabuaeran" ~ -0.5,
                               TRUE ~ 0)) %>%
  mutate(ymax_buff = case_when(island == "Abaiang" ~ 0.1,
                               island == "Abemama" ~ 0.02,
                               island == "Butaritari" ~ 0,
                               island == "Kiritimati" ~ 0.5, 
                               island == "N Tabiteuae" ~ 0.05,
                               island == "Onotoa" ~ 0.02,
                               island == "S Tabiteuae" ~ 0.02,
                               island == "S Tarawa" ~ 0,
                               island == "Tabuaeran" ~ 0.5,
                               TRUE ~ 0))
  
# FIX IT - loop not working for full set of islands
for (i in 1:length(islands)){
  island_i <- islands[i]
  island_bounds <- site_info %>%
    filter(Island == island_i) %>%
    dplyr::select(Lat, Long) %>% 
    summarise(max_lat = max(Lat),
              min_lat = min(Lat),
              max_long = max(Long),
              min_long = min(Long))
  island_buffer_i <- island_buffer %>%
    filter(island == island_i)
  island_box = c(xmin = island_bounds$min_long + island_buffer_i$xmin_buff, 
          xmax = island_bounds$max_long + island_buffer_i$xmax_buff,
          ymin = island_bounds$min_lat + island_buffer_i$ymin_buff, 
          ymax = island_bounds$max_lat + island_buffer_i$ymax_buff)
  # Use box to crop island out of kir_sf
  #island_crop <- st_crop(st_geometry(kir_sf), box)
  island_crop <- st_crop(kir_sf, island_box) # Use this because scalebar() and north() prefer a full dataframe, not just the geometry
  
  
  # Subset ecological site info:
  island_dat <- site_info %>%
    filter(Island == island_i) 
  dive_sites <- island_dat %>%
    filter(Description == "Site") %>%
    filter(Notes %in% c("Forereef", "Lagoon", "Backreef", "Soft-bottom backreef")) %>%
    select(Lat, Long, Notes) #%>%
    #st_as_sf(coords = c("Long", "Lat")) %>%
    #st_set_crs(value = "WGS84") # NOTE: already checked and no difference between projecting points to CRS and using geom_sf vs just using geom_point
  # Easier to plot manta paths with geom_line vs geom_sf so don't project points to CRS
  manta_tows <- island_dat %>%
    filter(Description == "Site") %>%
    filter(Notes == "Manta Tow") %>%
    select(Lat, Long, Name_cleaned, Notes) %>%
    mutate(Manta_pair = str_replace(Name_cleaned, pattern = "a|b", replacement = "")) 
  
  # Subset village info
  villages <- island_dat %>%
    filter(Description == "Village") %>%
    select(Lat, Long, Name_cleaned, Rank.Market.Integration)
  

  p <- ggplot() +
    geom_sf(data = island_crop) +
    # VILLAGE and REEF INFO (from Table 1)
    geom_point(data = villages, aes(x = Long, y = Lat, color = Rank.Market.Integration), shape = 17, size = 3) +
    #geom_label(data = villages, aes(x = Long, y = Lat, label = Name_cleaned), fontface = "italic", nudge_y = 0.008) +
    # MANTA TOWS
    # For track lines:
    #geom_line(data = manta_tows, aes(x = Long, y = Lat, group = Manta_pair, linetype = Notes), color = "blue", size = 1) +
    # For midpoint of track line:
    geom_point(data = manta_tows %>%
                 group_by(Manta_pair) %>%
                 summarise(avg_lat = mean(Lat),
                           avg_long = mean(Long)), aes(x = avg_long, y = avg_lat), color = "black") +
    # Plot dive sites COLLAPSE all sampling strata - label as "eco sites"
    # with geom_point
    geom_point(data = dive_sites, aes(x = Long, y = Lat), color = "black") +
    # with geom_sf
    # geom_sf(data = dive_sites, aes(color = Notes)) +
    theme_bw() +
    # Add scale bar and north arrow
    scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 2, location = "bottomleft", st.size = 3, model = "WGS84") +
    # For S Tabiteuae, change scalebar:
    #scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 2, location = "topright", st.size = 3, model = "WGS84") +
    # For S Tarawa, change scalebar:
    #scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 2, location = "bottomright", st.size = 3, model = "WGS84", height = 0.05) +
    north(island_crop, symbol = 12, location = "topright") +
    labs(title = island_i, x = "", y = "", color = "", linetype = "", shape = "")
  print(p)
  ggsave(filename = file.path(outdir, paste("map_island-", island_i, ".png", sep = "")), width = 8, height = 8)
}

# PLOT Pacific Ocean region showing Gilbert vs Line Islands
library(rnaturalearth)
kir_region <- ne_countries(scale = 50, returnclass = "sf", type = "countries", country = c("Kiribati", "Australia", "New Zealand", "Papua New Guinea", "Indonesia"))
kir_shift <- st_shift_longitude(kir_region)

ggplot() + 
  #geom_sf(data = kir_region, fill = "NA", colour = "black") +
  geom_sf(data = kir_shift, fill = "NA", colour = "black") +
  geom_rect(aes(xmin = 165, xmax = 180, ymin = -5, ymax = 5), fill = NA, color = "black") +
  geom_rect(aes(xmin = 195, xmax = 205, ymin = 0, ymax = 8), fill = NA, color = "black") +
  theme_bw()
ggsave(filename = file.path(outdir, paste("map_region.png", sep = "")), width = 8, height = 8)


# Zoom into Gilbert vs Line Islands
gilbert_islands <- c("Abaiang", "Abemama", "Butaritari", "N Tabiteuea", "N Tarawa", "S Tabiteuea", "S Tarawa", "Onotoa")
line_islands <- c("Kiritimati", "Tabuaeran")

# First Gilbert:
archipel_bounds <- site_info %>%
  filter(Island %in% gilbert_islands) %>%
  dplyr::select(Lat, Long) %>%
  summarise(max_lat = max(Lat),
            min_lat = min(Lat),
            max_long = max(Long),
            min_long = min(Long))
archipel_box = c(xmin = archipel_bounds$min_long + 0, 
        xmax = archipel_bounds$max_long + 0,
        ymin = archipel_bounds$min_lat + 0, 
        ymax = archipel_bounds$max_lat + 0)

# Use box to crop gilbert out of full country map
gilbert_crop <- st_crop(kir_sf, archipel_box) # Use this because scalebar() and north() prefer a full dataframe, not just the geometry
gilbert_buffer <- st_buffer(gilbert_crop, dist = 0.1, joinStyle = "ROUND")
# Warning message: All geometry functions (e.g., st_intersects) assumes coordinates are planar (i.e., projected coordinates with planar units like "km")
# Here stick with units arc degrees (near the equator, dist = 0.1 approximates 11.1 km)

# Add ISLAND NAMES TO POLYGONS
# Split MULTIPOLYGON into separate POLYGONS
gilbert_buffer_split <- gilbert_buffer %>% # initiate dataframe for loop
  st_cast("POLYGON") %>%
  mutate(island_name = "Not sampled")

# Loop through each island, create bounding box from site info, and match polygons in gilbert buffer by testing if polygons intersect
for (i in 1:length(islands)){
  island_i <- islands[i]
  island_bounds <- site_info %>%
    filter(Island == island_i) %>%
    dplyr::select(Lat, Long) %>% 
    summarise(max_lat = max(Lat),
              min_lat = min(Lat),
              max_long = max(Long),
              min_long = min(Long))
  island_buffer_i <- island_buffer %>%
    filter(island == island_i)
  
  island_polygon = data.frame(xmin = island_bounds$min_long + island_buffer_i$xmin_buff, 
                              xmax = island_bounds$max_long + island_buffer_i$xmax_buff,
                              ymin = island_bounds$min_lat + island_buffer_i$ymin_buff, 
                              ymax = island_bounds$max_lat + island_buffer_i$ymax_buff) %>%
    # Turn xmin/max and ymin/max into four coordinates
    pivot_longer(cols = all_of(c("xmin", "xmax")), names_to = "x", values_to = "lon") %>%
    pivot_longer(cols = all_of(c("ymin", "ymax")), names_to = "y", values_to = "lat") %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326, remove = F) %>%
    # Combine coordinates and cast to polygon
    st_combine() %>%
    st_cast("POLYGON")
  
  # test if any of the polygons intersect with island_polygon - if so, assign name island_i
  # loop through all island_i and island_polygons
  gilbert_buffer_split <- gilbert_buffer_split %>%
    mutate(island_test = st_intersects(geometry, island_polygon, sparse = FALSE)) %>%
    mutate(island_name = case_when(island_name != "Not sampled" ~ island_name,
                                   island_test == TRUE ~ island_i,
                                   TRUE ~ "Not sampled")) %>%
    select(-island_test) 
}
# Warning message: All geometry functions (e.g., st_intersects) assumes coordinates are planar (i.e., projected coordinates with planar units like "km")
# Here stick with units arc degrees (near the equator, dist = 0.1 approximates 11.1 km)

# LEFT OFF HERE:
# NEXT: Split Tabiteuae and Tarawa polygons into NORTH and SOUTH
# Apparently Abaiang and Tarawa are a single polygon? loop matches to Abaiang, but doesn't match Tarawa since this is part of Abaiang in the data frame


# FIX IT: Add color-coding to buffer zones by reef health
p <- ggplot() +
  geom_sf(data = gilbert_buffer) +
  geom_sf(data = gilbert_crop) +
  theme_bw() +
  # Add scale bar and north arrow
  north(gilbert_crop, symbol = 12) +
  labs(title = "Gilbert Islands", x = "", y = "")
  # FIX IT - scalebar not working
  #scalebar(data = gilbert_crop, transform = TRUE, dist_unit = "km", dist = 2000, location = "bottomleft", model = "WGS84")

print(p)
ggsave(filename = file.path(outdir, paste("map_region-gilbert-islands.png", sep = "")), width = 8, height = 8)

# Repeat for Line Islands
bounds <- site_info %>%
  filter(Island %in% line_islands) %>%
  dplyr::select(Lat, Long) %>%
  summarise(max_lat = max(Lat),
            min_lat = min(Lat),
            max_long = max(Long),
            min_long = min(Long))
box = c(xmin = bounds$min_long + 0, 
        xmax = bounds$max_long + 0.5,
        ymin = bounds$min_lat - 0.5, 
        ymax = bounds$max_lat + 0.5)

# Use box to crop island out of kir_sf
line_islands_crop <- st_crop(kir_sf, box) # Use this because scalebar() and north() prefer a full dataframe, not just the geometry
p <- ggplot() +
  geom_sf(data = line_islands_crop) +
  theme_bw() +
  # Add scale bar and north arrow
  north(line_islands_crop, symbol = 12) +
  labs(title = "Line Islands", x = "", y = "")
# FIX IT - scalebar not working
#scalebar(data = line_islands_crop, transform = TRUE, dist_unit = "km", dist = 2000, location = "bottomleft", model = "WGS84")

print(p)
ggsave(filename = file.path(outdir, paste("map_region-line-islands.png", sep = "")), width = 8, height = 8)

