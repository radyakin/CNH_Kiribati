# PLOT MAP OF ECOLOGICAL SITES FOR NSF-CNHS STUDY (modified from plot_study_map.R)
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
datadir <- "Data for mapping"
outdir <- "Outputs"

# Load country polygons
kir_sf <- readRDS(file = file.path(datadir, "gadm36_KIR_0_sf.rds")) # sf file for Kiribati from https://gadm.org/download_country_v3.html
st_geometry(kir_sf)
# Geographic CRS (coordinate reference system) WGS 84 - World Geodetic System - this is the system used by GPS
# WGS 84 equal to EPSG code: 4326

# Resolution is only at the country level (i.e., no attribute to allow for easy subsetting by island name)
#plot(st_geometry(kir_sf))

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

# RBIND Manta and non Manta, create plot_eco_label column, filter down to only Gilbert Islands
site_info_split_islands <- site_info_no_manta %>%
  bind_rows(manta_tows_to_points) %>%
  mutate(plot_eco_label = case_when(Description == "Site" & Reef_Quality_Sampling == "No" ~ "Ecology",
                                    Description == "Site" & Reef_Quality_Sampling == "Yes" ~ "Ecology + Water Quality",
                                    TRUE ~ "Other")) %>%
  filter(Island %in% c("Tabuaeran", "Kiritimati")==FALSE)

# Combine N and S Tarawa and N and S Tab into single islands
site_info_combined_islands <- site_info_split_islands %>%
  filter(str_detect(Island, "Tarawa|Tabiteuea")) %>%
  mutate(Island = case_when(str_detect(Island, "Tarawa") ~ "Tarawa",
                            str_detect(Island, "Tabiteuea") ~ "Tabiteuea",
                            TRUE ~ Island)) 

# Rowbind "combined" (Tarawa, Tabiteuea) and split (N/S Tarawa and N/S Tabiteuea) versions
site_info_all <- site_info_split_islands %>%
  bind_rows(site_info_combined_islands)
  
# Create plotting window around each island (Note: this is copied from the full Kirimati study protocol code (Line Islands not relevant here)
# Based on trial error of default "0" for buffer
# Strategy is to use the max/min  Lat/Long of the dive sites as a starting point in site_info
# Then in order to plot the full extent of each island, add additional space to these dive sites as defined by island_buffer below
islands <- sort(unique(c(site_info_all$Island, site_info_split_islands$Island)))
island_buffer_all <- data.frame(island = islands, xmin_buff = NA, xmax_buff = NA, ymin_buff = NA, ymax_buff = NA) %>%
  mutate(xmin_buff = case_when(island == "Abaiang" ~ -0.165,
                               island == "Abemama" ~ -0.02,
                               island == "Butaritari" ~ -0.02,
                               island == "Kiritimati" ~ -0.03, 
                               island == "N Tabiteuea" ~ -0.02,
                               island == "N Tarawa" ~ -0.01,
                               island == "Onotoa" ~ -0.02,
                               island == "S Tabiteuea" ~ -0.01,
                               island == "S Tarawa" ~ -0.01,
                               island == "Tabuaeran" ~ -0.01,
                               island == "Tabiteuea" ~ -0.02,
                               island == "Tarawa" ~ -0.01,
                               TRUE ~ 0)) %>%
  mutate(xmax_buff = case_when(island == "Abaiang" ~ 0.03,
                               island == "Abemama" ~ 0.02,
                               island == "Butaritari" ~ 0.02,
                               island == "Kiritimati" ~ 0.25, 
                               island == "N Tabiteuea" ~ 0.14,
                               island == "N Tarawa" ~ 0.05,
                               island == "Onotoa" ~ 0.02,
                               island == "S Tabiteuea" ~ 0.02,
                               island == "S Tarawa" ~ 0.05,
                               island == "Tabuaeran" ~ 0.08,
                               island == "Tabiteuea" ~ 0.02,
                               island == "Tarawa" ~ 0.05,
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
                               island == "Tabiteuea" ~ -0.02,
                               island == "Tarawa" ~ -0.02,
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
                               island == "Tabiteuea" ~ 0.05,
                               island == "Tarawa" ~ 0.01,
                               TRUE ~ 0))

# Use site_info_all and island_buffer_all to create all possible versions of islands (e.g., N and S Tarawa as well as "full" Tarawa)
# Loop through islands, plot each island to console, and assign to individual ggplot objects
for (i in 1:length(islands)){
  island_i <- islands[i]
  
  # Get the min/max Lat/long dive site for each island
  island_bounds <- site_info_all %>%
    filter(Island == island_i) %>%
    dplyr::select(Lat, Long) %>% 
    summarise(max_lat = max(Lat),
              min_lat = min(Lat),
              max_long = max(Long),
              min_long = min(Long))
 
  # Add a buffer around the dive sites to get a custom plotting window for each island
  island_buffer_i <- island_buffer_all %>%
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
  } else if (island_i == "Tarawa"){
    island_bathy_i <- getNOAA.bathy(lon1 = island_box[[1]] - 0.05, lon2 = island_box[[2]], lat1 = island_box[[3]], lat2 = island_box[[4]] + 0.05, resolution = 1, keep = TRUE, path = datadir) 
  } else if (island_i == "Tabiteuea"){
    island_bathy_i <- getNOAA.bathy(lon1 = island_box[[1]], lon2 = island_box[[2]], lat1 = island_box[[3]], lat2 = island_box[[4]] + 0.1, resolution = 1, keep = TRUE, path = datadir) 
  } else {
    island_bathy_i <- getNOAA.bathy(lon1 = island_box[[1]], lon2 = island_box[[2]], lat1 = island_box[[3]], lat2 = island_box[[4]], resolution = 1, keep = TRUE, path = datadir)
  }
  # fortify() not necessary: geom_contour automatically converts bathy object to data.frame
  island_bathy_i <- fortify(island_bathy_i)
  
  # Subset ecological site info:
  island_dat <- site_info_all %>%
    filter(Island == island_i) 
  
  # Filter down to just the fore reef sites
  eco_and_quality_sites <- island_dat %>%
    filter(Description == "Site" & Notes %in% c("FR", "MT"))

  #st_as_sf(coords = c("Long", "Lat")) %>%
  #st_set_crs(value = "WGS84") # NOTE: already checked and no difference between projecting points to CRS and using geom_sf vs just using geom_point
  
  # IF plotting manta tows, easier to plot manta paths with geom_line vs geom_sf so don't project points to CRS
  # manta_tows <- island_dat %>%
  #   filter(Description == "Site") %>%
  #   filter(Notes == "Manta Tow") %>%
  #   select(Lat, Long, Name_cleaned, Notes) %>%
  #   mutate(Manta_pair = str_replace(Name_cleaned, pattern = "a|b", replacement = "")) 
  
  max_depth <- max(abs(island_bathy_i$z))
  
  p <- ggplot() +
    # Turn off geom_contour_fill to leave out colored bathys
    #geom_contour_fill(data = island_bathy_i, aes(x = x, y = y, z = z), show.legend = FALSE, binwidth = 500) +
    geom_contour(data = island_bathy_i, aes(x = x, y = y, z = z), colour = "gray", binwidth = 500) +
    metR::geom_text_contour(data = island_bathy_i, aes(x=x, y=y, z = z*-1), stroke = 0.2, breaks = seq(500, max_depth, by = 500), label.placement = label_placement_n(n=1)) +
    geom_sf(data = island_crop, colour = "black") +
    # Manta Tow track lines
    #geom_line(data = manta_tows, aes(x = Long, y = Lat, group = Manta_pair, linetype = Notes), color = "blue", size = 1) +
    # All UVC surveys and Manta tows collapsed into a single sampling type (ecology)
    # Display ecology (UVC + manta) and water sampling sites with geom_point - use different shape for Manta Tows
    geom_point(data = eco_and_quality_sites, aes(x = Long, y = Lat, shape = Notes)) +
    scale_shape_manual(values = c(1, 19)) +
    # with geom_sf
    # geom_sf(data = dive_sites, aes(color = Notes)) +
    theme_bw() +
    #theme(legend.position = "none") +
    # Give all graphs a legend, and if needed just reset as p + theme(legend.position = "none") downstream
    theme(legend.position = "right", legend.direction = "vertical", legend.box = "vertical", legend.box.just = "left", legend.text = element_text(size = 8), legend.title = element_text(size = 10)) +
    labs(title = island_i, x = "", y = "", color = "Hypothesized Market Integration", shape = "Survey Type", linetype = "", shape = "") +
    # use coord_sf to cut off geom_contour which extends beyond map limits
    coord_sf(xlim = c(island_box[[1]], island_box[[2]]), ylim = c(island_box[[3]], island_box[[4]]), expand = FALSE)
  
  # CONDITIONALLY add scalebar
  if (island_i == "Kiritimati"){
    # Custom scalebar for Kiritimati, change scalebar:
    p <- p + 
      scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 5, location = "bottomleft", st.size = 3, model = "WGS84")
  } else if (island_i == "N Tabiteuea"){
    # Custom scalebar for N Tabiteuea, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 2, location = "bottomleft", st.size = 3, model = "WGS84", anchor = c(x = 174.65, y = -1.375))
  } else if (island_i == "N Tarawa"){
    # Custom scalebar for N Tarawa, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 2, location = "bottomleft", st.size = 3, model = "WGS84", anchor = c(x = 172.95, y = 1.4))
  } else if (island_i == "S Tabiteuea"){
    # Custom scalebar for S Tabiteuea, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 2, location = "bottomleft", st.size = 3, model = "WGS84", anchor = c(x = 174.92, y = -1.54))
  } else if (island_i == "S Taraw"){
    # Custom scalebar for S Tarawa, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 2, location = "bottomleft", st.size = 3, model = "WGS84", anchor = c(x = 172.91, y = 1.335))
  } else if (island_i == "Tabuaeran"){
    # Custom scalebar for Tabuaeran, change scalebar:
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 2, location = "bottomleft", st.size = 3, model = "WGS84")
  } else {
    # Add default scale bar
    p <- p + scalebar(island_crop, transform = TRUE, dist_unit = "km", dist = 2, location = "bottomleft", st.size = 3, model = "WGS84")
  }
  print(p)
  assign(paste("p_", str_to_lower(str_replace(island_i, " ", "_")), sep = ""), p)
  # Warning message:
  #   Computation failed in `stat_text_contour()`:
  #   object 'rlang_hash' not found 
  # If warning message appears, try reinstalling rlang package
  #ggsave(filename = file.path(outdir, paste("map_island-", island_i, ".png", sep = "")), width = 8, height = 8)
}

#########################################################################################################
#########################################################################################################
# PLOT Pacific Ocean region showing Gilbert Islands
kir_region <- ne_countries(scale = 50, returnclass = "sf", type = "countries", country = c("Kiribati", "Australia", "New Zealand", "Papua New Guinea", "Indonesia", "Solomon Islands", "New Caledonia", "Vanuatu", "Malaysia", "Philippines", "Vietnam", "Cambodia", "Thailand"))
# Kiribati straddles 180 degrees, so use st_shift_longitude to re-center
kir_shift <- st_shift_longitude(kir_region)

# Define location of box to be drawn around Gilbert archipelago
gilbert_box <- geom_rect(aes(xmin = 172, xmax = 179.5, ymin = -1.5, ymax = 3.5), fill = NA, color = "purple", size = 1)

# Define location of label
gilbert_label <- st_point(c(174.5, 0.5))
labels_sf <- st_as_sf(data.frame(name = "Gilbert",
                            geom = st_sfc(gilbert_label, line_label)))
st_crs(labels_sf) <- st_crs(kir_shift)
labels_sf_shift <- st_shift_longitude(labels_sf)

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
# Plot Gilbert archipelago
# JACOB if you want to plot Hypothesized Reef Health, then you'll need "split" "islands since Reef Health is different for N vs South Tabiteuea
# In other words, use site_info_split_islands and island_buffer_split_islands

gilbert_islands <- sort(unique(site_info_split_islands$Island))

island_buffer_split_islands <- island_buffer_all %>%
  filter(island %in% c("Tabiteuea", "Tarawa") == FALSE)

archipel_bounds <- site_info_split_islands %>%
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

# Loop through each island, create bounding box from site info, crop, and create column label for each island out of the gilbert islands
islands_labeled <- NULL
for (i in 1:length(gilbert_islands)){
  island_i <- gilbert_islands[i]
  island_bounds <- site_info_split_islands %>%
    filter(Island == island_i) %>%
    dplyr::select(Lat, Long) %>% 
    summarise(max_lat = max(Lat),
              min_lat = min(Lat),
              max_long = max(Long),
              min_long = min(Long))
  island_buffer_i <- island_buffer_split_islands %>%
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

##############
# Create a buffer around each island that has ecological data
# First, load village and reef info from table 1
village_info <- read.csv(file = file.path(datadir, "data_from_table_1.csv")) %>%
  # Clean village name to match Jacob's data
  mutate(Community.Name = if_else(Community.Name == "Kuuma" & Island.Name == "Butaritari", true = "Kuma", false = Community.Name)) %>%
  select(-c(Lat, Lon)) # Original Table 1 lat/longs were incorrect; get this info from Jacob's table instead

sampled_buffer <- st_buffer(islands_labeled, dist = 0.05, joinStyle = "ROUND") %>%
  left_join(village_info %>% select(Island.Name, Rank.Reef.Health) %>% unique(), by = c("island_name" = "Island.Name")) %>%
  mutate(Rank.Reef.Health = if_else(island_name == "Not sampled", true = "Not sampled", false = Rank.Reef.Health)) %>%
  mutate(Rank.Reef.Health = if_else(Rank.Reef.Health == "Med", true = "Medium", false = Rank.Reef.Health)) %>%
  mutate(Rank.Reef.Health = as.factor(Rank.Reef.Health)) %>%
  mutate(Rank.Reef.Health = fct_relevel(Rank.Reef.Health, "Low", "Medium", "High"))
###############

# Check the two gilbert layers
# ggplot() +
#   geom_sf(data = gilbert_buffer)
# ggplot() +
#   geom_sf(data = sampled_buffer)

display.brewer.pal(8, "Greens")
reef_colors <- brewer.pal(8, "Greens")[c(8, 6, 4)]

# JACOB, remove aes(fill = Rank.Reef.Health) if you don't want to plot Hypothesized Reef Health
p_gilbert <- ggplot() +
  # JACOB - if you don't want a buffer around the sampled islands remove geom_sf(data = sampled_buffer)
  # Plot buffer around all sampled islands
  geom_sf(data = sampled_buffer, aes(fill = Rank.Reef.Health)) +
  scale_fill_manual(breaks = c("High", "Medium", "Low", "Not sampled"), values = c(reef_colors, "gray")) +
  # Plot islands:
  geom_sf(data = gilbert_crop, color = "black", size = 0.1) +
  # nudge all labels to the right by one unit, nudge S Tarawa down by 0.07
  geom_sf_label(data = sampled_buffer, aes(label = island_name), size = 2, nudge_x = 1, nudge_y = c(0, 0, 0, 0, 0, 0, 0, -0.07)) +
  theme_bw() +
  xlim(172.5, 179) +
  labs(title = "", x = "", y = "", fill = "Hypothesized Reef Health") +
  scalebar(data = gilbert_crop, transform = TRUE, dist_unit = "km", dist = 100, location = "bottomleft", model = "WGS84", st.size = 3) +
  theme(legend.position = "right", legend.direction = "vertical", legend.text = element_text(size = 8), legend.title = element_text(size = 10))

print(p_gilbert)
#ggsave(filename = file.path(outdir, paste("map_region-gilbert-islands.png", sep = "")), width = 8, height = 8)

######################################################################################################
# COMBINE INTO MULTIPANEL PLOTS
######################################################################################################

# Default panel theme is no legend
panel_theme <- theme(legend.position = "none", plot.margin = unit(c(0, 0, 0, 0), "cm"))

# Remove legend from all plots:
p_abaiang_panel <- p_abaiang + panel_theme
p_abemama_panel <- p_abemama + panel_theme
p_butaritari_panel <- p_butaritari + theme(legend.position = "none", plot.margin = unit(c(0, 0.5, 0, 0), "cm")) # add small margin to final plot in row
p_onotoa_panel <- p_onotoa + panel_theme
p_tabiteuea_panel <- p_tabiteuea + panel_theme
p_tarawa_panel <- p_tarawa + panel_theme
p_gilbert_panel <- p_gilbert + panel_theme

# In case you want to use them, individual ggplot objects were also created for the "split" island versions
#p_n_tabiteuea_panel <- p_n_tabiteuea + panel_theme
#p_n_tarawa_panel <- p_n_tarawa + panel_theme
#p_s_tabiteuea_panel <- p_s_tabiteuea + panel_theme
#p_s_tarawa_panel <- p_s_tarawa + panel_theme

# Get legends
legend_theme <- theme(legend.margin = margin(c(0, 0, 0, 0)),
                      legend.box.margin = margin(0, 0, 0, 0),
                      legend.text = element_text(size = 7),
                      legend.title = element_text(size = 7))
reef_legend <- get_legend(p_gilbert + legend_theme)
# eco_legend <- get_legend(p_abaiang + legend_theme) # currently using p_tarawa to plot eco_legend

# Arrange panels by row
row1 <- plot_grid(p_abaiang_panel, 
                  p_abemama_panel,
                  p_butaritari_panel,
                  nrow = 1, ncol = 3,
                  rel_widths = c(1.1, 1, 1.6)) # adjust relative widths of each plot

row2 <- plot_grid(p_onotoa_panel,
                  p_tabiteuea_panel, 
                  p_tarawa, # use p_tarawa (with legend) instead of p_tarawa_panel
                  nrow = 1, ncol = 3,
                  rel_widths = c(1, 1.1, 1.55),
                  align = "h", axis = "t")

row3 <- plot_grid(p_region_gilbert, p_gilbert_panel, reef_legend,
                  nrow = 1, ncol = 3,
                  rel_widths = c(1.1, 1, 1),
                  align = "h", axis = "t")

plot_grid(row1,
          row2,
          row3,
          nrow = 3,
          ncol = 1,
          rel_heights = c(1, 1.2, 1),
          align = "v", axis = "l")

# When ready to save:
#ggsave(filename = file.path(outdir, "map_figure-X_gilbert-islands.png"), width = 11, height = 8.5)

