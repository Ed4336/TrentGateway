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
  group_by(SITE_NAME, SPECIES_NAME, EVENT_DATE_YEAR) %>%
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
#other----

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
    geom_bar(position = 'dodge') +
    #facet_wrap(~EVENT_DATE_YEAR, scales = 'free') + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
    labs(title = paste("Survey Type:", site), 
         y = "Number of Surveys",
         x = NULL,
         fill = 'Method')
  
  # Plot for Species Abundance per Site and Year
  site_abundance_plot[[site]] <- ggplot(site_summary, aes(x = SPECIES_NAME,
                                                          y = Total_Count,
                                                          fill = SPECIES_NAME)) +
    geom_bar(stat = 'identity') +
    facet_wrap(~EVENT_DATE_YEAR, scales = 'free') +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
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

# Display each plot for Survey Type and Catch per Year
for (site in names(site_survey_plot)) {
  print(site_survey_plot[[site]])
}

# Display each plot for Species Abundance per Site and Year
for (site in names(site_abundance_plot)) {
  print(site_abundance_plot[[site]])
}

# Display each plot for Species Proportion Across All Surveys
for (site in names(site_proportion_plot)) {
  print(site_proportion_plot[[site]])
}


#another failed plan----

manual_action_zones_df <- data.frame(
  Site_Name = c("Winthorpe-Cromwell", "Fiskerton-Winthorpe", "Fiskerton-Winthorpe", "Fiskerton-Winthorpe", "Fiskerton-Winthorpe", 
                "Gunthorpe-Fiskerton", "Gunthorpe-Fiskerton", "Gunthorpe-Fiskerton", 
                "Colwick-Gunthorpe", "Colwick-Gunthorpe", 
                "Sawley-Attenborough"),
  Action_Zone = c("Holme Net", "Muskham Bridge Net", "Nether Lock to Cromwell", "Mill Lane (Newark Dyke)", "Farndon Net", 
                  "DS Hazleford Weir", "Nabbs Island", "Kneeton Net", 
                  "Stroke Bardolph Net", "Colwick Marina Net", 
                  "Sawley Weir Net"))