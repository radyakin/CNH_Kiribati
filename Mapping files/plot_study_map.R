# PLOT MAP FOR CNHS STUDY PROTOCOL
# Author: Kelvin Gorospe
# All input files can be found in Google Drive: Kiribati/Data/Data for Mapping


rm(list=ls())
library(tidyverse)
library(sf)
library(ggsn) # for adding north arrows and scale bar
library(marmap)
library(metR) # for labeling contour lines
library(rnaturalearth) # ne_countries (world polygons)
library(RColorBrewer) # for display.brewer.pal
library(cowplot) # multipanel plotting
library(ggrepel) # for geom_label_repel
library(rgdal) # for readOGR
datadir <- "Mapping files"
outdir <- "Outputs"

# Load country polygons
kir_sf <- readRDS(file = file.path(datadir, "gadm36_KIR_0_sf.rds")) # sf file for Kiribati from https://gadm.org/download_country_v3.html
st_geometry(kir_sf)
# Geographic CRS (coordinate reference system) WGS 84 - World Geodetic System - this is the system used by GPS
# WGS 84 equal to EPSG code: 4326

# Resolution is only at the country level (i.e., no attribute to allow for easy subsetting by island name)
#plot(st_geometry(kir_sf))

# Load village and reef info from table 1
village_info <- read.csv(file = file.path(datadir, "data_from_table_1.csv")) %>%
  # Clean village name to match Jacob's data
  mutate(Community.Name = if_else(Community.Name == "Kuuma" & Island.Name == "Butaritari", true = "Kuma", false = Community.Name)) %>%
  select(-c(Lat, Lon)) # Original Table 1 lat/longs were incorrect; get this info from Jacob's table instead

# Looks like rnaturalearth package also doesn't have island-level info for Kiribati either
# Will need to crop each island out of kir_sf manually using island_box below
# library(rnaturalearth)
# kir_ne <- ne_countries(scale = 50, returnclass = "sf", type = "countries", country = "Kiribati")

# Load Jacob's ecological sites info and join with village_info
# Also Use Jacob's ecological site info to approximate a plotting window for each island and use this to crop out each island from the full kir_sf
eco_info <- read.csv(file = file.path(datadir, "Kiribaiti NSF Coordinates FINAL Eurich_20210218.csv")) %>%
  mutate(Island = if_else(str_detect(Island, "Tabiteuae"), true = str_replace(Island, "Tabiteuae", replacement = "Tabiteuea"), false = Island))

## KEY TO NOTES COLUMN:
# mutate(Notes = case_when(Notes == "FR" ~ "Forereef",
#                          Notes == "LR" ~ "Lagoon",
#                          Notes == "MT" ~ "Manta Tow",
#                          Notes == "SB" ~ "Backreef", 
#                          Notes == "SIQ" ~ "Soft-bottom backreef",
#                          TRUE ~ Notes)) 

# Check village names match between datasets
setdiff(eco_info %>% filter(Description == "Village") %>% pull(Name_cleaned), village_info$Community.Name)
setdiff(village_info$Community.Name, eco_info %>% filter(Description == "Village") %>% pull(Name_cleaned))

# Check island names
setdiff(eco_info %>%  pull(Island), village_info$Island.Name)
setdiff(village_info$Island.Name, eco_info %>%  pull(Island))

# Filter out Ciguatera site (not plotting this)
# Filter out Mantas - clean this separately
site_info_no_manta <- eco_info %>%
  filter(Island != "") %>% # Remove blank rows
  # NOTE there is one Ciguatera site that was also sampled for Reef Health, but leaving this out for simplicity
  filter(Description %in% c("Ciguatera", "Miscellaneous")==FALSE) %>% # remove misc and ciguatera sites
  filter(Notes != "MT")  # MT = manta tow

# Clean Manta Tow data
manta_tows_to_points <- eco_info %>%
  filter(Description == "Site" & Notes == "MT") %>%
  mutate(Name_cleaned = str_replace(Name_cleaned, pattern = "a|b", replacement = "")) %>%
  mutate(Name_original = str_replace(Name_original, pattern = "a|b", replacement = "")) %>%
  group_by(Name_cleaned, Name_original) %>%
  mutate(Lat = mean(Lat), 
         Long = mean(Long)) %>%
  ungroup()

# RBIND Manta and non Manta, create plot_eco_label column, and join with village info
site_info <- site_info_no_manta %>%
  bind_rows(manta_tows_to_points) %>%
  mutate(plot_eco_label = case_when(Description == "Site" & Reef_Quality_Sampling == "No" ~ "Ecology",
                                    Description == "Site" & Reef_Quality_Sampling == "Yes" ~ "Ecology + Water Quality",
                                    TRUE ~ "Other")) %>%
  left_join(village_info, by = c("Name_cleaned" = "Community.Name", "Island" = "Island.Name"))

# Create plotting window around each island
# Based on trial error of default "0" for buffer
# Strategy is to use the max/min  Lat/Long of the dive sites as a starting point in site_info
# Then in order to plot the full extent of each island, add additional space to these dive sites as defined by island_buffer below
islands <- sort(unique(site_info$Island))
island_buffer <- data.frame(island = islands, xmin_buff = NA, xmax_buff = NA, ymin_buff = NA, ymax_buff = NA) %>%
  mutate(xmin_buff = case_when(island == "Abaiang" ~ -0.165,
                               island == "Abemama" ~ -0.02,
                               island == "Butaritari" ~ -0.02,
                               island == "Kiritimati" ~ -0.03, 
                               island == "N Tabiteuea" ~ -0.02,
                               island == "N Tarawa" ~ -0.01,
                               island == "Onotoa" ~ -0.02,
                               island == "S Tabiteuea" ~ -0.01,
                               island == "S Tarawa" ~ 0,
                               island == "Tabuaeran" ~ -0.01,
                               TRUE ~ 0)) %>%
  mutate(xmax_buff = case_when(island == "Abaiang" ~ 0.03,
                               island == "Abemama" ~ 0.02,
                               island == "Butaritari" ~ 0.02,
                               island == "Kiritimati" ~ 0.25, 
                               island == "N Tabiteuea" ~ 0.13,
                               island == "N Tarawa" ~ 0.02,
                               island == "Onotoa" ~ 0.02,
                               island == "S Tabiteuea" ~ 0.02,
                               island == "S Tarawa" ~ 0.05,
                               island == "Tabuaeran" ~ 0.08,
                               TRUE ~ 0)) %>%
  mutate(ymin_buff = case_when(island == "Abaiang" ~ -0.01,
                               island == "Abemama" ~ -0.038,
                               island == "Butaritari" ~ -0.015,
                               island == "Kiritimati" ~ -0.215, 
                               island == "N Tabiteuea" ~ -0.091,
                               island == "N Tarawa" ~ -0.02,
                               island == "Onotoa" ~ -0.02,
                               island == "S Tabiteuea" ~ -0.01,
                               island == "S Tarawa" ~ -0.011,
                               island == "Tabuaeran" ~ -0.016,
                               TRUE ~ 0)) %>%
  mutate(ymax_buff = case_when(island == "Abaiang" ~ 0.12,
                               island == "Abemama" ~ 0.02,
                               island == "Butaritari" ~ 0.005,
                               island == "Kiritimati" ~ 0.04, 
                               island == "N Tabiteuea" ~ 0.05,
                               island == "Onotoa" ~ 0.02,
                               island == "S Tabiteuea" ~ 0.062,
                               island == "S Tarawa" ~ 0.01,
                               island == "Tabuaeran" ~ 0.04,
                               TRUE ~ 0))

# Set colors for market integration
display.brewer.pal(8, "Oranges")
market_colors <- brewer.pal(8, "Oranges")[c(4, 6, 8)]

# Loop through islands, plot each island to console, and assign to individual ggplot objects
for (i in 1:length(islands)){
  island_i <- islands[i]
  
  # Get the min/max Lat/long dive site for each island
  island_bounds <- site_info %>%
    filter(Island == island_i) %>%
    dplyr::select(Lat, Long) %>% 
    summarise(max_lat = max(Lat),
              min_lat = min(Lat),
              max_long = max(Long),
              min_long = min(Long))
 
  # Add a buffer around the dive sites to get a custom plotting window for each island
  island_buffer_i <- island_buffer %>%
    filter(island == island_i)
  island_box = c(xmin = island_bounds$min_long + island_buffer_i$xmin_buff, 
                 xmax = island_bounds$max_long + island_buffer_i$xmax_buff,
                 ymin = island_bounds$min_lat + island_buffer_i$ymin_buff, 
                 ymax = island_bounds$max_lat + island_buffer_i$ymax_buff)
  
  # Use island_box to crop island out of kir_sf
  #island_crop <- st_crop(st_geometry(kir_sf), box)
  island_crop <- st_crop(kir_sf, island_box) # Use this because scalebar() and north() prefer a full dataframe, not just the geometry
  
  # Use same box to get bathy data, see tutorial: https://cran.r-project.org/web/packages/marmap/vignettes/marmap.pdf
  # Minor adjustments to island_box are necessary depending on the island
  if (island_i == "N Tabiteuea"){
    island_bathy_i <- getNOAA.bathy(lon1 = island_box[[1]], lon2 = island_box[[2]] + 0.1, lat1 = island_box[[3]] - 0.05, lat2 = island_box[[4]], resolution = 1, keep = TRUE, path = datadir) 
  } else if (island_i == "N Tarawa"){
    island_bathy_i <- getNOAA.bathy(lon1 = island_box[[1]], lon2 = island_box[[2]] + 0.1, lat1 = island_box[[3]], lat2 = island_box[[4]] + 0.1, resolution = 1, keep = TRUE, path = datadir) 
  } else if (island_i == "Onotoa"){
    island_bathy_i <- getNOAA.bathy(lon1 = island_box[[1]], lon2 = island_box[[2]], lat1 = island_box[[3]], lat2 = island_box[[4]] + 0.05, resolution = 1, keep = TRUE, path = datadir) 
  } else if (island_i == "S Tabiteuea"){
    island_bathy_i <- getNOAA.bathy(lon1 = island_box[[1]], lon2 = island_box[[2]] + 0.1, lat1 = island_box[[3]], lat2 = island_box[[4]] + 0.1, resolution = 1, keep = TRUE, path = datadir) 
  } else if (island_i == "Tabuaeran"){
    island_bathy_i <- getNOAA.bathy(lon1 = island_box[[1]] - 0.05, lon2 = island_box[[2]], lat1 = island_box[[3]], lat2 = island_box[[4]] + 0.05, resolution = 1, keep = TRUE, path = datadir) 
  } else {
    island_bathy_i <- getNOAA.bathy(lon1 = island_box[[1]], lon2 = island_box[[2]], lat1 = island_box[[3]], lat2 = island_box[[4]], resolution = 1, keep = TRUE, path = datadir)
  }
  # fortify() not necessary: geom_contour automatically converts bathy object to data.frame
  island_bathy_i <- fortify(island_bathy_i)
  
  # Subset ecological site info:
  island_dat <- site_info %>%
    filter(Island == island_i) 
  
  eco_and_quality_sites <- island_dat %>%
    filter(Description == "Site")

  #st_as_sf(coords = c("Long", "Lat")) %>%
  #st_set_crs(value = "WGS84") # NOTE: already checked and no difference between projecting points to CRS and using geom_sf vs just using geom_point
  
  # IF plotting manta tows, easier to plot manta paths with geom_line vs geom_sf so don't project points to CRS
  # manta_tows <- island_dat %>%
  #   filter(Description == "Site") %>%
  #   filter(Notes == "Manta Tow") %>%
  #   select(Lat, Long, Name_cleaned, Notes) %>%
  #   mutate(Manta_pair = str_replace(Name_cleaned, pattern = "a|b", replacement = "")) 
  
  # Subset village info
  villages <- island_dat %>%
    filter(Description == "Village") %>%
    select(Lat, Long, Name_cleaned, Rank.Market.Integration) %>%
    mutate(Rank.Market.Integration = as.factor(Rank.Market.Integration)) %>%
    # Set order of factors for plot
    mutate(Rank.Market.Integration = fct_relevel(Rank.Market.Integration, c("Low", "Medium", "High")))
  
  max_depth <- max(abs(island_bathy_i$z))
  
  p <- ggplot() +
    # Turn off geom_contour_fill to leave out colored bathys
    #geom_contour_fill(data = island_bathy_i, aes(x = x, y = y, z = z), show.legend = FALSE, binwidth = 500) +
    geom_contour(data = island_bathy_i, aes(x = x, y = y, z = z), colour = "gray", binwidth = 500) +
    metR::geom_text_contour(data = island_bathy_i, aes(x=x, y=y, z = z*-1), size = 3, stroke = 0.2, breaks = seq(500, max_depth, by = 500), label.placement = label_placement_n(n=1)) +
    geom_sf(data = island_crop, colour = "black") +
    # Village and market integration info (from Table 1)
    geom_point(data = villages, shape = 17, size = 3, aes(x = Long, y = Lat, color = Rank.Market.Integration)) +
    scale_color_manual(values = market_colors) +
    # Manta Tow track lines
    #geom_line(data = manta_tows, aes(x = Long, y = Lat, group = Manta_pair, linetype = Notes), color = "blue", size = 1) +
    # All UVC surveys and Manta tows collapsed into a single sampling type (ecology)
    # Display ecology (UVC + manta) and water sampling sites with geom_point
    geom_point(data = eco_and_quality_sites, aes(x = Long, y = Lat, shape = plot_eco_label)) +
    scale_shape_manual(values = c(1, 19)) +
    # with geom_sf
    # geom_sf(data = dive_sites, aes(color = Notes)) +
    theme_bw() +
    #theme(legend.position = "none") +
    # Give all graphs a legend, and if needed just reset as p + theme(legend.position = "none") downstream
    theme(legend.position = "right", legend.direction = "vertical", legend.box = "vertical", legend.box.just = "left", legend.text = element_text(size = 8), legend.title = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(title = island_i, x = "", y = "", color = "Hypothesized Market Integration", shape = "Reef Sampling Sites", linetype = "", shape = "") +
    # use coord_sf to cut off geom_contour which extends beyond map limits
    coord_sf(xlim = c(island_box[[1]], island_box[[2]]), ylim = c(island_box[[3]], island_box[[4]]), expand = FALSE)
  
  # CONDITIONALLY add scalebar
  if (island_i == "Butaritari"){
    # Custom scalebar for Butaritari, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 4, location = "topleft", st.size = 4, st.dist = 0.06, height = 0.06, model = "WGS84", anchor = c(x = 172.72, y = 3.15), st.bottom = FALSE)
  } else if (island_i == "Kiritimati"){
    # Custom scalebar for Kiritimati, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 4, location = "bottomleft", st.size = 4, st.dist = 0.06, height = 0.06, model = "WGS84", anchor = c(x = -157.57 , y = 1.68), st.bottom = FALSE)
  } else if (island_i == "N Tabiteuea"){
    # Custom scalebar for N Tabiteuea, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 4, location = "bottomleft", st.size = 4, st.dist = 0.06, height = 0.06, model = "WGS84", anchor = c(x = 174.65, y = -1.375), st.bottom = FALSE)
  } else if (island_i == "N Tarawa"){
    # Custom scalebar for N Tarawa, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 4, location = "bottomleft", st.size = 4, st.dist = 0.06, height = 0.06, model = "WGS84", anchor = c(x = 172.925, y = 1.4), st.bottom = FALSE)
  } else if (island_i == "Onotoa"){
    # Custom scalebar for Onotoa, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 4, location = "bottomleft", st.size = 4, st.dist = 0.06, height = 0.06, model = "WGS84", anchor = c(x = 175.505, y = -1.95), st.bottom = FALSE)
  } else if (island_i == "S Tabiteuea"){
    # Custom scalebar for S Tabiteuea, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 4, location = "bottomleft", st.size = 4, st.dist = 0.06, height = 0.06, model = "WGS84", anchor = c(x = 174.92, y = -1.54), st.bottom = FALSE)
  } else if (island_i == "S Tarawa"){
    # Custom scalebar for S Tarawa, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 4, location = "bottomright", st.size = 4, st.dist = 0.12, height = 0.06, model = "WGS84", st.bottom = FALSE)
  } else if (island_i == "Tabuaeran"){
    # Custom scalebar for Tabuaeran, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 4, location = "topright", st.size = 4, st.dist = 0.06, height = 0.06, model = "WGS84", anchor = c(x = -159.24, y = 3.921), st.bottom = FALSE)
  } else {
    # Add default scale bar
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 4, location = "bottomleft", st.size = 4, st.dist = 0.06, height = 0.06, model = "WGS84", st.bottom = FALSE)
  }
  #print(p)
  assign(paste("p_", str_to_lower(str_replace(island_i, " ", "_")), sep = ""), p)
  # Warning message:
  #   Computation failed in `stat_text_contour()`:
  #   object 'rlang_hash' not found 
  # If warning message appears, try reinstalling rlang package
  #ggsave(filename = file.path(outdir, paste("map_island-", island_i, ".png", sep = "")), width = 8, height = 8)
}

#########################################################################################################
#########################################################################################################
# PLOT Pacific Ocean region showing Gilbert vs Line Islands
kir_region <- ne_countries(scale = 50, returnclass = "sf", type = "countries", country = c("Kiribati", "Australia", "New Zealand", "Papua New Guinea", "Indonesia", "Solomon Islands", "New Caledonia", "Vanuatu", "Malaysia", "Philippines", "Vietnam", "Cambodia", "Thailand"))
# Kiribati straddles 180 degrees, so use st_shift_longitude to re-center
kir_shift <- st_shift_longitude(kir_region)

# Define location of box to be drawn around each archipelago
# These should match xlim and ylim of the gilbert and line island maps
line_box <- geom_rect(aes(xmin = 199.5, xmax = 205.5, ymin = 0, ymax = 6), fill = NA, color = "blue", size = 1)
gilbert_box <- geom_rect(aes(xmin = 172.5, xmax = 179.5, ymin = -4, ymax = 4), fill = NA, color = "blue", size = 1)


# Define location of label for each archipelago
line_label <- st_point(c(202, 10))
gilbert_label <- st_point(c(174.5, 8))
phoenix_label <- st_point(c(188, 3))
labels_sf <- st_as_sf(data.frame(name = c("Gilbert", "Line", "Phoenix"),
                            geom = st_sfc(gilbert_label, line_label, phoenix_label)))
st_crs(labels_sf) <- st_crs(kir_shift)
labels_sf_shift <- st_shift_longitude(labels_sf)

# For regional maps, plot Kiribati EEZ
# First download shapefile FOLDER of EEZ data from here: https://www.marineregions.org/downloads.php
# Link to download can be found under: Marine and land zones: the union of world country boundaries and EEZ's version 3
eez <- readOGR(dsn = "/Users/kgorospe/Documents/Git/CNH_Kiribati/Data for mapping/EEZ_land_union_v3_202003")
kir_eez <- eez[eez$ISO_SOV1 == "KIR",]
kir_eez_sf <- st_as_sf(kir_eez)
kir_eez_shift <- st_shift_longitude(kir_eez_sf)

#kir_eez_fort <- fortify(kir_eez)

# Plot the regional map with both archipelagos indicated by boxes and labels
p_region <- ggplot() + 
  #geom_sf(data = kir_region, fill = "NA", colour = "black") +
  geom_sf(data = kir_shift, fill = "NA", colour = "black") +
  geom_sf(data = kir_eez_shift, colour = "black", fill = "NA") +
  #geom_path(data = kir_eez_fort, aes(x = long, y = lat)) +
  gilbert_box +
  line_box +
  geom_sf_label(data = labels_sf_shift, aes(label = name), size = 4, nudge_y = c(0, 0, 0), nudge_x = c(0, 0, 0)) +
  ylim(-50, 10) +
  xlim(130, 210) +
  labs(x = "", y = "") +
  scalebar(x.min = 130, x.max = 210, y.min = -50, y.max = 10, transform = TRUE, dist_unit = "km", dist = 1000, location = "bottomleft", st.size = 3, model = "WGS84", st.bottom = FALSE) +
  theme_bw()
print(p_region)
#ggsave(filename = file.path(outdir, paste("map_region.png", sep = "")), width = 8, height = 8)

# Alternatively, plot the region with each archipelago highlighted separately:
p_region_line <- ggplot() + 
  #geom_sf(data = kir_region, fill = "NA", colour = "black") +
  geom_sf(data = kir_shift, fill = "NA", colour = "black") +
  line_box +
  ylim(-50, 10) +
  labs(x = "", y = "") +
  north(kir_shift, symbol = 12, location = "bottomright", anchor = c(x = 210, y = -40)) +
  scalebar(kir_shift, transform = TRUE, dist_unit = "km", dist = 1000, location = "bottomright", st.size = 3, model = "WGS84", anchor = c(x = 210, y = -45)) +
  theme_bw()
print(p_region_line)

p_region_gilbert <- ggplot() + 
  #geom_sf(data = kir_region, fill = "NA", colour = "black") +
  geom_sf(data = kir_shift, fill = "NA", colour = "black") +
  gilbert_box + 
  ylim(-50, 10) +
  labs(x = "", y = "") +
  #xlim(120, -150) +
  north(kir_shift, symbol = 12, location = "bottomright", anchor = c(x = 210, y = -40)) +
  scalebar(kir_shift, transform = TRUE, dist_unit = "km", dist = 1000, location = "bottomright", st.size = 3, model = "WGS84", anchor = c(x = 210, y = -45)) +
  theme_bw()
print(p_region_gilbert)

#########################################################################################################
#########################################################################################################
# Plot Gilbert and Line Island archipelagos
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
        ymin = archipel_bounds$min_lat - 1, 
        ymax = archipel_bounds$max_lat + 3)

# Use box to crop gilbert out of full country map

gilbert_crop <- st_crop(kir_sf, archipel_box) # Use this because scalebar() and north() prefer a full dataframe, not just the geometry

# Create a layer of islands with a buffer around each island,
# First, by splitting the MULTIPOLYGON into separate POLYGONS
gilbert_cast <- gilbert_crop %>%
  st_cast("POLYGON") %>%
  mutate(island_name = "Not sampled")

# Then add buffer around all polygons and join back together again to form single multipolygon layer
# Label this as "Not sampled", all sampled polygons will be overlayed with a second layer
gilbert_buffer <- st_buffer(gilbert_cast, dist = 0.15, joinStyle = "ROUND") %>%
  mutate(island_name = "Not sampled") %>%
  group_by(island_name) %>%
  summarise(n = n())

# Loop through each island, create bounding box from site info, crop, and create column label for each island out of the gilbert islands
islands_labeled <- NULL
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
  
  # Crop out each island from within the gilbert islands and label it in the column island_name
  island_crop <- gilbert_crop %>% 
    st_crop(island_polygon) %>%
    mutate(island_name = island_i)
  # Check that the correct island was cropped out:
  # ggplot() +
  #   geom_sf(data = island_crop)

  islands_labeled <- rbind(islands_labeled, island_crop)
 
}
# Warning message: All geometry functions (e.g., st_intersects) assumes coordinates are planar (i.e., projected coordinates with planar units like "km")
# Here stick with units arc degrees (near the equator, dist = 0.1 approximates 11.1 km)

# Create second layer of islands with a buffer around each island, but now label with Reef Health information
sampled_buffer <- st_buffer(islands_labeled, dist = 0.15, joinStyle = "ROUND") %>%
  left_join(village_info %>% select(Island.Name, Rank.Reef.Health) %>% unique(), by = c("island_name" = "Island.Name")) %>%
  mutate(Rank.Reef.Health = if_else(island_name == "Not sampled", true = "Not sampled", false = Rank.Reef.Health)) %>%
  mutate(Rank.Reef.Health = if_else(Rank.Reef.Health == "Med", true = "Medium", false = Rank.Reef.Health)) %>%
  mutate(Rank.Reef.Health = as.factor(Rank.Reef.Health)) %>%
  mutate(Rank.Reef.Health = fct_relevel(Rank.Reef.Health, "Low", "Medium", "High"))

# Check the two gilbert layers
# ggplot() +
#   geom_sf(data = gilbert_buffer)
# ggplot() +
#   geom_sf(data = sampled_buffer)

#display.brewer.pal(8, "Greens")
reef_colors <- brewer.pal(8, "Greens")[c(8, 6, 4)]

p_gilbert <- ggplot() +
  geom_sf(data = gilbert_buffer, aes(fill = island_name)) +
  geom_sf(data = sampled_buffer, aes(fill = Rank.Reef.Health)) +
  scale_fill_manual(breaks = c("High", "Medium", "Low", "Not sampled"), values = c(reef_colors, "gray")) +
  # Plot islands:
  geom_sf(data = gilbert_crop, color = "black", size = 0.1) +
  # nudge all labels to the right by one unit, nudge S Tarawa down by 0.07
  #geom_sf_label(data = sampled_buffer, aes(label = island_name), size = 5, nudge_x = c(2, 2, 2, 2, 2, 2, 2, 2), nudge_y = c(0.75, 0, 0.5, 0, 0.25, -0.5, -0.25, -0.07)) +
  geom_label_repel(data = sampled_buffer, aes(label = island_name, geometry = geometry), stat = "sf_coordinates", size = 4, nudge_x = 3, direction = "y") +
  xlim(172.5, 179.5) +
  ylim(-4, 4) +
  theme_bw() +
  labs(title = "Gilbert Islands", x = "", y = "", fill = "Hypothesized Reef Health") +
  scalebar(x.min = 172.5, x.max = 179.5, y.min = -4, y.max = 5, transform = TRUE, dist_unit = "km", dist = 150, location = "bottomleft", model = "WGS84", st.dist = 0.05, st.size = 3, st.bottom = FALSE) +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size = 10), legend.title = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))

print(p_gilbert)
#ggsave(filename = file.path(outdir, paste("map_region-gilbert-islands.png", sep = "")), width = 8, height = 8)

#########################################################################################################
#########################################################################################################
# Do the same for the Lins islands: ONLY DIFFERENCE is line islands only has two factor levels for reef health (low, medium)

# Repeat for Line Islands
line_bounds <- site_info %>%
  filter(Island %in% line_islands) %>%
  dplyr::select(Lat, Long) %>%
  summarise(max_lat = max(Lat),
            min_lat = min(Lat),
            max_long = max(Long),
            min_long = min(Long))
line_box = c(xmin = line_bounds$min_long - 1, 
        xmax = line_bounds$max_long + 0.5,
        ymin = line_bounds$min_lat - 1, 
        ymax = line_bounds$max_lat + 3)

# Use box to crop island out of kir_sf
line_islands_crop <- st_crop(kir_sf, line_box) # Use this because scalebar() and north() prefer a full dataframe, not just the geometry
# Specify bbox
#attr(line_box, "class") <- "bbox"
#attr(st_geometry(line_islands_crop), "bbox") <- line_box

# Split MULTIPOLYGON into separate POLYGONS
line_islands_cast <- line_islands_crop %>%
  st_cast("POLYGON") %>%
  mutate(island_name = "Not sampled")

# Add buffer around all polygons
# Then group together to join buffers
line_islands_buffer <- st_buffer(line_islands_cast, dist = 0.15, joinStyle = "ROUND") %>%
  mutate(island_name = "Not sampled") %>%
  group_by(island_name) %>%
  summarise(n = n())
#attr(st_geometry(line_islands_buffer), "bbox") <- line_box

line_islands_labeled <- NULL
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
  
  # TRY CROPPING as was done in the island plots above
  island_crop <- line_islands_crop %>% 
    st_crop(island_polygon) %>%
    mutate(island_name = island_i)
  # ggplot() +
  #   geom_sf(data = island_crop)
  
  line_islands_labeled <- rbind(line_islands_labeled, island_crop)

}

# Add buffer around all sampled island polygons
sampled_lines_buffer <- st_buffer(line_islands_labeled, dist = 0.15, joinStyle = "ROUND") %>%
  left_join(village_info %>% select(Island.Name, Rank.Reef.Health) %>% unique(), by = c("island_name" = "Island.Name")) %>%
  mutate(Rank.Reef.Health = if_else(island_name == "Not sampled", true = "Not sampled", false = Rank.Reef.Health)) %>%
  mutate(Rank.Reef.Health = if_else(Rank.Reef.Health == "Med", true = "Medium", false = Rank.Reef.Health)) %>%
  mutate(Rank.Reef.Health = as.factor(Rank.Reef.Health)) %>%
  mutate(Rank.Reef.Health = fct_relevel(Rank.Reef.Health, "Low", "Medium"))
#attr(st_geometry(sampled_lines_buffer), "bbox") <- line_box

# Check two line islands layers
# ggplot() +
#   geom_sf(data = line_islands_buffer)
# ggplot() +
#   geom_sf(data = sampled_lines_buffer)

p_line <- ggplot() +
  geom_sf(data = line_islands_buffer, aes(fill = island_name)) +
  geom_sf(data = sampled_lines_buffer, aes(fill = Rank.Reef.Health)) +
  scale_fill_manual(breaks = c("Medium", "Low", "Not sampled"), values = c(reef_colors[-1], "gray")) + # Have to remove first color in reef_colors because only Medium and Low 
  geom_sf(data = line_islands_crop, color = "black", size = 0.1) +
  #geom_sf_label(data = sampled_lines_buffer, aes(label = island_name), size = 2, nudge_y = 0.5) +
  geom_label_repel(data = sampled_lines_buffer, aes(label = island_name, geometry = geometry), stat = "sf_coordinates", size = 4, nudge_x = 0.5, nudge_y = 1) +
  theme_bw() +
  xlim(-160.5, -156.5) +
  ylim(0, 6) +
  # Add scale bar and north arrow
  #north(line_islands_crop, symbol = 12) +
  labs(title = "Line Islands", x = "", y = "", fill = "Hypothesized Reef Health") +
  scalebar(x.min = -160.5, y.min = 0, x.max = -156.5, y.max = 6, transform = TRUE, dist_unit = "km", dist = 150, location = "bottomleft", model = "WGS84", st.size = 3, st.dist = 0.05, st.bottom = FALSE) +
  theme(legend.position = "bottom", legend.direction = "horizontal", legend.text = element_text(size = 10), legend.title = element_text(size = 10), axis.text.x = element_text(angle = 45, hjust = 1))

plot(p_line)
#ggsave(filename = file.path(outdir, paste("map_region-line-islands.png", sep = "")), width = 8, height = 8)

######################################################################################################
# COMBINE INTO MULTIPANEL PLOTS
######################################################################################################
# Default panel theme is no legend and plot margin = 0
panel_theme <- theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Remove legend from all plots
p_abaiang_panel <- p_abaiang + panel_theme
p_abemama_panel <- p_abemama + panel_theme
p_butaritari_panel <- p_butaritari + theme(legend.position = "none", plot.margin = unit(c(0, -0.5, 0, 0), "cm"))

p_kiritimati_panel <- p_kiritimati + panel_theme 
p_n_tabiteuea_panel <- p_n_tabiteuea + panel_theme
p_n_tarawa_panel <- p_n_tarawa + panel_theme
p_onotoa_panel <- p_onotoa + theme(legend.position = "none", plot.margin = unit(c(0, 0.5, 0, 0), "cm")) # add small margin to final plot in row

p_s_tabiteuea_panel <- p_s_tabiteuea + panel_theme
p_s_tarawa_panel <- p_s_tarawa + panel_theme
p_tabuaeran_panel <- p_tabuaeran  + theme(legend.position = "none", plot.margin = unit(c(0, 0.5, 0, 0), "cm"))

# Insets are difficult to control once we move to multipanel image - remove inset for now
# p_gilbert_inset <- ggdraw(p_gilbert + panel_theme) + draw_plot(p_region_gilbert + theme_half_open(), x = 1, y = 0.98, width = 0.65, height = 0.65, vjust = 1, hjust = 1) #+ panel_theme
# p_gilbert_inset
# ggsave(filename = file.path(outdir, "map_figure-X_gilbert-with-inset.png"), width = 8.5, height = 11)
# 
# p_line_inset <- ggdraw(p_line + panel_theme) + draw_plot(p_region_line + theme_half_open(), x = 1, y = 0.95, width = 0.65, height = 0.65, vjust = 1, hjust = 1)
# p_line_inset
# ggsave(filename = file.path(outdir, "map_figure-X_line-with-inset.png"), width = 8.5, height = 11)

p_gilbert_panel <- p_gilbert + panel_theme
p_line_panel <- p_line + panel_theme
p_region_panel <- p_region + panel_theme

# Get legends
legend_theme <- theme(legend.margin = margin(c(0, 0, 0, 0)), 
                      legend.box.margin = margin(c(0, 0, 0, 0)), 
                      legend.text = element_text(size = 10), 
                      legend.title = element_text(size = 10))
market_legend <- get_legend(p_kiritimati + legend_theme) # any island legend should work
reef_legend <- get_legend(p_gilbert + legend_theme) # use gilbert legend (line islands legend only has two categories for reef health)

######################################################################################################
# FIGURE 1:
# REMINDER: regional map must be plotted on a separate row from archipelago maps for their to be any chance of seeing Hypothesized Reef Health and island name labels
# FIGURES MUST BE SAVED (ggsave) for the final layout to be seen (don't rely on plotting window)
# STRATEGY is to first plot all panels as is, then use theme(plot.margin = unit(c(x, x, x, x), "cm")) to adjust spacing
# REMINDERS: adjust relative sizes by either adding or removing margins (works better than rel_heights and rel_widths)
# removing margin (using a negative number) causes plot to get bigger as it fills in the missing space
# adding margins causes plot to get smaller 

archipelago_row <- plot_grid(p_gilbert_panel + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                             p_line_panel + theme(plot.margin = unit(c(0, 0.2, 0, -3), "cm")),
                             nrow = 1,
                             ncol = 2,
                             rel_widths = c(1, 0.8))

plot_grid(p_region_panel + theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm")),
          archipelago_row + theme(plot.margin = unit(c(0, 0, -3, 1.5), "cm")),
          reef_legend,
          rel_heights = c(1.7, 0.95, 1),
          nrow = 3,
          ncol = 1)
ggsave(filename = file.path(outdir, "map_figure-X_region-and-archipelagos.png"), width = 8.5, height = 11)

######################################################################################################
# Figure 2: version 1 - islands ordered alphabetically
# REMINDERS: adjust relative sizes by either adding or removing margins (works better than rel_heights and rel_widths)
# removing margin (using a negative number) causes plot to get bigger as it fills in the missing space
# adding margins causes plot to get smaller

# REMINDER: need to shift panels over towards the market_legend; for some reason unable to shift market_legend using plot.margin
row1 <- plot_grid(p_abaiang_panel + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                  p_abemama_panel + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                  market_legend,
                  nrow = 1,
                  ncol = 3)
#row1
#ggsave(filename = file.path(outdir, "map_figure-X_row1.png"), width = 8.5, height = 11)

row2 <- plot_grid(p_butaritari_panel + theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")), # Otherwise right axis gets cutoff
                  p_kiritimati_panel + theme(plot.margin = unit(c(0, 0, 0, -0.2), "cm")),
                  p_n_tabiteuea_panel + theme(plot.margin = unit(c(0, 0.2, 0, 0), "cm")),
                  nrow = 1,
                  ncol = 3)
#row2
#ggsave(filename = file.path(outdir, "map_figure-X_row2.png"), width = 8.5, height = 11)

row3 <- plot_grid(p_n_tarawa_panel + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                  p_onotoa_panel + theme(plot.margin = unit(c(0, 0, 0, -1), "cm")),
                  p_s_tabiteuea_panel + theme(plot.margin = unit(c(0, 0.2, 0, 0), "cm")),
                  nrow = 1,
                  ncol = 3)
#row3
#ggsave(filename = file.path(outdir, "map_figure-X_row3.png"), width = 8.5, height = 11)


row4 <- plot_grid(p_s_tarawa_panel + theme(plot.margin = unit(c(0, 0, 0, 0.5), "cm")),
                  p_tabuaeran_panel + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                  nrow = 1,
                  ncol = 2,
                  rel_widths = c(1, 1))
#row4
#ggsave(filename = file.path(outdir, "map_figure-X_row4.png"), width = 8.5, height = 11)

# Combine rows together
plot_grid(row1 + theme(plot.margin = unit(c(0.2, 0, 0, 0), "cm")), # Otherwise top of island name titles get cut off
          row2,
          row3 + theme(plot.margin = unit(c(-0.5, 0, -0.5, 0), "cm")),
          row4 + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
          nrow = 4,
          ncol = 1,
          rel_heights = c(0.8, 1, 0.8, 0.8))
#align = 'v', axis = 'lr')
ggsave(filename = file.path(outdir, "map_figure-X_individual-islands_alpha-order.png"), width = 8.5, height = 11)


######################################################################################################
# Figure 2: version 2 - island grouped by island chain and then ordered by latitude
# REMINDERS: adjust relative sizes by either adding or removing margins (works better than rel_heights and rel_widths)
# removing margin (using a negative number) causes plot to get bigger as it fills in the missing space
# adding margins causes plot to get smaller
row1 <- plot_grid(p_butaritari_panel + theme(plot.margin = unit(c(0, -0.7, 0, 0), "cm")),
                  p_abaiang_panel + theme(plot.margin = unit(c(0, -1.5, 0, 0), "cm")), 
                  p_n_tarawa_panel,
                  nrow = 1,
                  ncol = 3)
#row1
#ggsave(filename = file.path(outdir, "map_figure-X_row1.png"), width = 8.5, height = 11)

row2 <- plot_grid(p_s_tarawa_panel + theme(plot.margin = unit(c(0, -1.5, 0, 1.5), "cm")),
                  p_abemama_panel,
                  nrow = 1,
                  ncol = 2)
#row2
#ggsave(filename = file.path(outdir, "map_figure-X_row2.png"), width = 8.5, height = 11)

row3 <- plot_grid(p_n_tabiteuea_panel + theme(plot.margin = unit(c(0, 0, 0, -1), "cm")), 
                  p_s_tabiteuea_panel + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")), 
                  p_onotoa_panel + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                  nrow = 1,
                  ncol = 3)
#row3
#ggsave(filename = file.path(outdir, "map_figure-X_row3.png"), width = 8.5, height = 11)

row4 <- plot_grid(p_tabuaeran_panel + theme(plot.margin = unit(c(0, 0, 0, 0), "cm")),
                  p_kiritimati_panel + theme(plot.margin = unit(c(0, -0.7, 0, 0), "cm")),
                  market_legend,
                  nrow = 1,
                  ncol = 3)
#row4
#ggsave(filename = file.path(outdir, "map_figure-X_row4.png"), width = 8.5, height = 11)

# Combine rows together
plot_grid(row1 + theme(plot.margin = unit(c(0.2, 0, -0.5, 0), "cm")), # Otherwise top of island name titles get cut off
          row2, 
          row3 + theme(plot.margin = unit(c(-0.5, 0, 0, 0), "cm")), 
          row4 + theme(plot.margin = unit(c(-0.5, 0, 0, 0), "cm")),
          nrow = 4, 
          ncol = 1,
          rel_heights = c(0.8, 0.8, 0.8, 1))
#align = 'v', axis = 'lr')
ggsave(filename = file.path(outdir, "map_figure-X_individual-islands_ordered-by-chain-and-latitude.png"), width = 8.5, height = 11)
