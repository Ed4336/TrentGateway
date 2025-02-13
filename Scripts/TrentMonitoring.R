#libraries ----
library(sf)
library(tmap)
library(leaflet)
library(osmdata)
library(tidyverse)
library(ggspatial)
library(viridis)

#trent river plot----
coords <- c(-2.23, 52.77, -0.64, 53.72)

trent <- opq(bbox = coords) %>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

riverLines <- trent$osm_lines %>% 
  filter(name=='River Trent')

#transform to BNG
riverLines <- st_transform(riverLines, crs = 27700)

#action zones plot----
#build df of Action zone limits - need to check these so far fairly arbitrary
AZs <- data.frame(
  name = c( "Sawley", "Attenborough", "Colwick", "Gunthorpe", "Fiskerton", "Winthorpe", "Cromwell" ),
  lat = c( 52.866587, 52.912018, 52.944593, 52.986480, 53.054877, 53.100151, 53.140612 ),
  lon = c( -1.334265, -1.204438, -1.111819, -0.994467, -0.894392, -0.800712, -0.792022)
)

# Convert points to sf
AZs_sf <- st_as_sf(AZs, coords = c("lon", "lat"), crs = 4326) %>%
  #transform to bng
  st_transform(crs = 27700)

AZs_sf_filtered <- AZs_sf %>%
  filter(!name %in% c("Sawley", "Cromwell"))

# Create diagonal lines extending through each point
line_list <- lapply(1:nrow(AZs_sf), function(i) {
  point <- AZs_sf[i, ]
  x <- st_coordinates(point)[1]
  y <- st_coordinates(point)[2]
  
  # Define a start and end point to create a diagonal line
  line_coords <- matrix(
    c(x - 5000, y + 5000,  # Offset up-left (NW)
      x + 5000, y - 5000), # Offset down-right (SE)
    ncol = 2, byrow = TRUE
  )
  
  # Create a linestring
  st_linestring(line_coords)
})

# Convert to an sf object
lines_sf <- st_sfc(line_list, crs = 27700) %>%
  st_as_sf() %>%
  mutate(name = AZs_sf$name)

##trim to target area ----
# Get the coordinates for Sawley and Cromwell
sawley_coords <- st_coordinates(AZs_sf %>% filter(name == "Sawley"))
cromwell_coords <- st_coordinates(AZs_sf %>% filter(name == "Cromwell"))

# Create a bounding box with the extent of Sawley and Cromwell
bbox <- st_sfc(
  st_polygon(list(rbind(
    c(sawley_coords[1] - 10000, sawley_coords[2] - 10000),  # bottom-left corner
    c(sawley_coords[1] - 10000, cromwell_coords[2] + 10000), # top-left corner
    c(cromwell_coords[1] + 10000, cromwell_coords[2] + 10000), # top-right corner
    c(cromwell_coords[1] + 10000, sawley_coords[2] - 10000), # bottom-right corner
    c(sawley_coords[1] - 10000, sawley_coords[2] - 10000)  # close the polygon
  ))),
  crs = 27700
)

# Crop river lines by the bounding box
riverLines_filtered <- st_crop(riverLines, bbox)

# Filter out the Sawley and Cromwell lines from lines_sf
lines_sf_filtered <- lines_sf %>%
  filter(!name %in% c("Sawley", "Cromwell"))

ggplot()+
  annotation_map_tile(type = "osm", zoom = 10)+
  geom_sf(data = riverLines_filtered, color = 'blue', linewidth = 1)+
  geom_sf(data = lines_sf_filtered, linetype = "dotted", color = "black", linewidth = 1)
  

#trent monitoring----

#access monitoring sites
monitorSites <- read_csv('Data/FW_Fish_Sites_2024-11-14.csv')

#transform to spatial object
SFmonitorSites <- st_as_sf(monitorSites, coords = c("SITE_RANKED_EASTING", "SITE_RANKED_NORTHING"), crs = 27700)

#select sites that are on the trent
SFmonitorSitesTrent <- SFmonitorSites %>% 
  filter(SITE_PARENT_NAME == 'TRENT')

#filter to the sites that are within trent gateway
monitorSites_filtered <- st_crop(SFmonitorSitesTrent, bbox)

# Extract the Y-coordinate (Northing) from the geometry
SFmonitorSitesTrent$Northing <- st_coordinates(SFmonitorSitesTrent)[, 2]

# Reorder to plot north to south
SFmonitorSitesTrent$SITE_NAME <- factor(SFmonitorSitesTrent$SITE_NAME,
                                        levels = SFmonitorSitesTrent$SITE_NAME[order(-SFmonitorSitesTrent$Northing)])#visualise

ggplot()+
  annotation_map_tile(type = "osm", zoom = 10)+
  geom_sf(data = riverLines_filtered, color = 'blue', linewidth = 1)+
  geom_sf(data = lines_sf_filtered, linetype = "dotted", color = "black", linewidth = 1)+
  geom_sf(data = SFmonitorSitesTrent, aes(color = SITE_NAME), size = 3, alpha = 1)+
  geom_sf(data = SFmonitorSitesTrent, shape = 1, size = 3, alpha = 1, color = 'black') + 
  scale_color_manual(values = viridis(length(unique(SFmonitorSitesTrent$SITE_NAME))))

##fish----

fishCounts <- read_csv('Data/FW_Fish_Counts_2024-11-14.csv')

fishCounts <- st_as_sf(fishCounts, coords = c("SURVEY_RANKED_EASTING", "SURVEY_RANKED_NORTHING"), crs = 27700)

filtFC <- fishCounts %>% 
  filter(SITE_ID %in% SFmonitorSitesTrent$SITE_ID)


# Reorder to plot north to south

filtFC$Northing <- st_coordinates(filtFC)[, 2]

filtFC$SITE_NAME <- factor(filtFC$SITE_NAME,
         levels = unique(filtFC$SITE_NAME[order(-filtFC$Northing)]))

(
 survey.type.date <- ggplot(filtFC, aes(x = SITE_NAME, fill = SURVEY_METHOD))+
    geom_bar(position = 'dodge')+
    facet_wrap(~EVENT_DATE_YEAR, scales = 'free')+ 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
)

(
  survey.catch.tot <- ggplot(filtFC, aes(x = reorder(SPECIES_NAME, -ALL_RUNS), y = ALL_RUNS, fill = SITE_NAME)) +
    geom_bar(stat = 'identity') +
    #scale_y_log10(labels = scales::comma_format())+
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    facet_wrap(~EVENT_DATE_YEAR, scales = 'free', drop = T) +
    labs(x = "Species (Ordered by Abundance)", title = "Species Abundance per Site and Year")
)

filtFC.summary <- filtFC %>% 
  group_by(SITE_NAME, SPECIES_NAME) %>%
  summarise(Total_Count = sum(ALL_RUNS), .groups = "drop")

(
  ggplot(filtFC.summary, aes(x = "", y = Total_Count, fill = SPECIES_NAME)) +
    geom_bar(stat = 'identity') +
    coord_polar(theta = 'y') +
    facet_wrap(~SITE_NAME, scales = "free_y") +
    labs(title = "Species Composition per Site and Year", 
         x = NULL, y = "Total Count")
)

#it would probably be good to fold some of these species into an 'other category'
