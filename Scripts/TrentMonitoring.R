#libraries ----
library(sf)
library(tmap)
library(leaflet)
library(osmdata)
library(tidyverse)

#trent river plot----
coords <- c(-2.23, 52.77, -0.64, 53.72)

trent <- opq(bbox = coords) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

riverLines <- trent$osm_lines %>% 
  filter(name=='River Trent')

ggplot()+
  geom_sf(data = river_lines)

#trent monitoring----
##fish

bulkCounts <- read_csv('Data/FW_Fish_Bulk_Measurements_2024-11-14.csv')

filtBC <- bulkCounts %>% 
  select(c('SITE_NAME', 'EVENT_DATE', 'SPECIES_NAME', 'LATIN_NAME', 'BULK_FISH_COUNT', 'BULK_FISH_WEIGHT'))

(
  ggplot(filtBC, aes(x = SITE_NAME, y = SPECIES_NAME))+
    geom_point(aes(size = BULK_FISH_COUNT))
)

fishCounts <- read_csv('Data/FW_Fish_Counts_2024-11-14.csv')
filtFC <- fishCounts %>% 
  select(c('SITE_NAME', 'SPECIES_NAME', 'ALL_RUNS'))

(
  ggplot(filtFC, aes(x = SITE_NAME, y = SPECIES_NAME ))+
    geom_point(aes(size = ALL_RUNS))
)
