#libraries ----
library(sf)
library(tmap)
library(leaflet)
library(osmdata)
library(tidyverse)
library(RColorBrewer)

#trent river plot----
coords <- c(-2.23, 52.77, -0.64, 53.72)

trent <- opq(bbox = coords) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

riverLines <- trent$osm_lines %>% 
  filter(name=='River Trent')

#transform to BNG
riverLines <- st_transform(riverLines, crs = 27700)

ggplot()+
  geom_sf(data = riverLines)

#trent monitoring----

#access monitoring sites
monitorSites <- read_csv('Data/FW_Fish_Sites_2024-11-14.csv')

#transform to spatial object
SFmonitorSites <- st_as_sf(monitorSites, coords = c("SITE_RANKED_EASTING", "SITE_RANKED_NORTHING"), crs = 27700)

#visualise
ggplot()+
  geom_sf(data = SFmonitorSites)+
  geom_sf(data = riverLines, color = 'blue')

#not all sites are on the trent - buffer river and extract only sites that are on the trent

#define a buffer for the river
buffer <- 50

#create a buffer around river
riverLinesBuff <- st_buffer(riverLines, dist = buffer)

#filter to keep only sites that are within the buffer zone
SFmonitorSitesTrent <- st_filter(SFmonitorSites, riverLinesBuff)

# Extract the Y-coordinate (Northing) from the geometry
SFmonitorSitesTrent$Northing <- st_coordinates(SFmonitorSitesTrent)[, 2]

# Reorder to plot north to south
SFmonitorSitesTrent$SITE_NAME <- factor(SFmonitorSitesTrent$SITE_NAME,
                                        levels = SFmonitorSitesTrent$SITE_NAME[order(-SFmonitorSitesTrent$Northing)])

ggplot()+
  geom_sf(data = riverLinesBuff, color = 'blue')+
  geom_sf(data = SFmonitorSitesTrent, aes(color = SITE_NAME), size = 3, alpha = 1)+
  geom_sf(data = SFmonitorSitesTrent, shape = 1, size = 3, alpha = 1, color = 'black') +   
  scale_color_manual(values = brewer.pal((length(unique(SFmonitorSitesTrent$SITE_NAME))),"Set3"))

##fish----

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
