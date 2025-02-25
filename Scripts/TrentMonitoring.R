#libraries ----
library(sf)
library(tmap)
library(leaflet)
library(osmdata)
library(tidyverse)
library(ggspatial)
library(ggrepel)
library(viridis)
library(egg)

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


##individual action zone plots----
###create river sections----
create_bboxes <- function(points_sf) {
  bbox_list <- list()
  
  for (i in 1:(nrow(points_sf) - 1)) {
    p1 <- st_coordinates(points_sf[i, ])
    p2 <- st_coordinates(points_sf[i + 1, ])
    
    bbox_coords <- rbind(
      c(min(p1[1], p2[1]) - 1000, min(p1[2], p2[2]) - 1000),  # Bottom-left
      c(min(p1[1], p2[1]) - 1000, max(p1[2], p2[2]) + 1000),  # Top-left
      c(max(p1[1], p2[1]) + 1000, max(p1[2], p2[2]) + 1000),  # Top-right
      c(max(p1[1], p2[1]) + 1000, min(p1[2], p2[2]) - 1000),  # Bottom-right
      c(min(p1[1], p2[1]) - 1000, min(p1[2], p2[2]) - 1000)   # Close polygon
    )
    
    bbox_list[[i]] <- st_polygon(list(bbox_coords))
  }
  
  # Convert to an sf object
  bboxes_sf <- st_sf(
    name = paste(points_sf$name[-length(points_sf$name)], 
                 points_sf$name[-1], sep = "-"),
    geometry = st_sfc(bbox_list, crs = 27700)
  )
  
  return(bboxes_sf)
}

# Generate bounding boxes
bboxes_sf <- create_bboxes(AZs_sf)

# Create a list to store cropped river sections
cropped_river_sections <- list()

for (i in 1:nrow(bboxes_sf)) {
  bbox <- bboxes_sf[i, ]
  cropped_river_sections[[i]] <- st_crop(riverLines_filtered, bbox)
}

# Assign names based on the bounding box names
names(cropped_river_sections) <- bboxes_sf$name


###add in monitoring sites----

# Create a list to store filtered monitoring sites per river section
monitoring_sites_by_section <- lapply(1:nrow(bboxes_sf), function(i) {
  bbox <- bboxes_sf[i, ]
  sites_in_bbox <- SFmonitorSitesTrent[st_intersects(SFmonitorSitesTrent, bbox, sparse = FALSE), ]
  return(sites_in_bbox)
})

# Assign names to match river sections
names(monitoring_sites_by_section) <- bboxes_sf$name

plot_river_section <- function(section_name, river_section, bbox, monitoring_sites) {
  ggplot() +
    annotation_map_tile(type = "osm", zoom = 12)+
    geom_sf(data = river_section, color = "blue", size = 1) +
    geom_sf(data = bbox, fill = NA, color = "black", linetype = 'dashed') +
    geom_sf(data = monitoring_sites, aes(color = SITE_NAME), size = 3) +
    scale_color_manual(values = rainbow(nrow(monitoring_sites))) +  # Optional: Assign unique colors
    guides(color = guide_legend(title = "Monitoring Sites")) +  # Add legend title
    ggtitle(paste("River Section:", section_name)) +
    theme_minimal()+
    theme(
      axis.title = element_blank(),  # Remove axis titles
      axis.text = element_blank(),   # Remove axis text (labels)
      axis.ticks = element_blank()   # Remove axis ticks
    )
    
}

# Generate and display all plots with monitoring sites and names
plot_list <- setNames(
  lapply(names(cropped_river_sections), function(section) {
    plot_river_section(
      section,
      cropped_river_sections[[section]], 
      bboxes_sf[bboxes_sf$name == section, ],
      monitoring_sites_by_section[[section]]
    )
  }),
  names(cropped_river_sections)  # Assign section names as list names
)

# Print all plots
for (p in plot_list) print(p)

#Save the plots
# Iterate over each plot in the plot_list
for (section in names(plot_list)) {
  # Define the filename for each plot (e.g., "Sawley-Attenborough.png")
  filename <- paste0(section, "_plot.png")
  
  # Save the plot using ggsave
  ggsave(filename, plot = plot_list[[section]], 
         width = 14, height = 8, units = "cm", dpi = 300)
}

##fish----

fishCounts <- read_csv('Data/FW_Fish_Counts_2024-11-14.csv')

fishCounts <- st_as_sf(fishCounts, coords = c("SURVEY_RANKED_EASTING", "SURVEY_RANKED_NORTHING"), crs = 27700)

filtFC <- fishCounts %>% 
  filter(SITE_ID %in% SFmonitorSitesTrent$SITE_ID)

filtFC.summary <- filtFC %>% 
  group_by(SITE_NAME, SPECIES_NAME, EVENT_DATE_YEAR) %>%
  summarise(Total_Count = sum(ALL_RUNS), .groups = "drop")

##fish catch plots----

# Create a list to store individual plots for each site
site_survey_plot <- list()
site_abundance_plot <- list()
site_proportion_plot <- list()

# Loop through each unique SITE_NAME for the different plots
for (site in unique(filtFC$SITE_NAME)) {
  
  # Filter data for each site
  site_data <- filtFC %>% filter(SITE_NAME == site)
  site_summary <- filtFC.summary %>% filter(SITE_NAME == site)
  
  # Plot for Survey Type and Amount of Catch per Year
  site_survey_plot[[site]] <- ggplot(site_data, aes(x = as.factor(EVENT_DATE_YEAR), fill = SURVEY_METHOD)) +
    geom_bar(stat = 'count',
             position = 'dodge') +
    #facet_wrap(~EVENT_DATE_YEAR, scales = 'free') + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1),
          legend.position = 'bottom') +
    labs(title = paste("Survey Type:", site), 
         y = "Number of Surveys",
         x = NULL,
         fill = 'Method')+
    scale_y_continuous(
      breaks = seq(0, max(table(site_data$EVENT_DATE_YEAR)), by = 1),  # Major ticks every 1 unit
      minor_breaks = seq(0, max(table(site_data$EVENT_DATE_YEAR)), by = 1))
  
  # Plot for Species Abundance per Site and Year
  site_abundance_plot[[site]] <- ggplot(site_summary, aes(x = SPECIES_NAME,
                                                          y = Total_Count,
                                                          fill = SPECIES_NAME)) +
    geom_bar(stat = 'identity') +
    facet_wrap(~EVENT_DATE_YEAR, scales = 'free') +
    theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8)) +
    labs(title = paste("Species Abundance:", site),
         x = NULL,
         y = "Total Abundance")+
    theme(legend.position = 'none')
    
    # Plot for Species Proportion Across All Surveys
  site_proportion_plot[[site]] <- ggplot(site_summary, aes(x = "",
                                                           y = Total_Count, fill = SPECIES_NAME)) +
    geom_bar(stat = 'identity') +
    coord_polar(theta = 'y') +
    labs(x = NULL, y = "Proportion",
         fill = 'Species') +
    theme(axis.text.x = element_blank())
}

##build plots based on site----

# Combine the individual plots for each site (you can select the plots you want)
combined_survey_plots <- list()

for (site in names(site_survey_plot)) {
  
  # Get the individual plots for the site
  survey_plot <- site_survey_plot[[site]]
  abundance_plot <- site_abundance_plot[[site]]
  #proportion_plot <- site_proportion_plot[[site]]not sure this is necessary 
  
  # Combine the plots using egg::ggarrange()
  combined_plot <- ggarrange(survey_plot, abundance_plot,
                             ncol = 2, nrow = 1,
                             widths = c(1,2))
  
  # Display the combined plot
  combined_survey_plots[[site]] <- combined_plot
}

print(names(combined_survey_plots))

#Save the plots
# Iterate over each plot in the plot_list
for (section in names(combined_survey_plots)) {
  # Define the filename for each plot (e.g., "Sawley-Attenborough.png")
  filename <- paste0(section, "_plot.png")
  
  # Save the plot using ggsave
  ggsave(filename, plot = combined_survey_plots[[section]], 
         width = 22.78, height = 15.58, units = "cm", dpi = 300)
}


###combine the river sections and the plots of the species present----
#looks like you cant ggarrange an object that has already had the arrange called on it- possibly clearer to just leave as maps anyway





