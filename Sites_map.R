# FIX IT - ggmap now requires API key with credit card information (https://cloud.google.com/maps-platform/pricing)
# TITLE: CNH Sites map
# AUTHOR: Jessica Gephart
# DATE: 16 Jan 18
#__________________________________________________________________________________________#
# Load packages
#__________________________________________________________________________________________#
require (ggmap) || {install.packages("ggmap"); require(ggmap)}
require (maps) || {install.packages("maps"); require(maps)}
require (mapdata) || {install.packages("mapdata"); require(mapdata)}
require (dplyr) || {install.packages("dplyr"); require(dplyr)}
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)

#__________________________________________________________________________________________#
# Load data
#__________________________________________________________________________________________#
datadir <- "Data for mapping"
site <- read.csv(file.path(datadir, "Sites_v2.csv"))
site$HIES_SampleSize <- as.numeric(site$HIES_SampleSize)

site_E <- filter(site, Long > 0)
site_W <- filter(site, Long < 0)
site_W$Long <- 180 + (180-(-1*site_W$Long))

site <- rbind(site_E, site_W)
#__________________________________________________________________________________________#
# Initiate map
#__________________________________________________________________________________________#
# location bounding box: left/bottom/right/top bounding box
#Lat <- mean(min(site$Lat), max(site$Lat))
#Long <- mean(min(site$Long), max(site$Long))

Lat <- 1
Long <- 180

map <- get_map(location = c(lon = Long, lat = Lat), zoom = 4) 

g <- ggmap(map) + 
  geom_point(aes(x = Long, y = Lat, size=HIES_SampleSize, 
                 colour=factor(Development)), data = site, alpha=0.5)
g

#__________________________________________________________________________________________#
#
#__________________________________________________________________________________________#