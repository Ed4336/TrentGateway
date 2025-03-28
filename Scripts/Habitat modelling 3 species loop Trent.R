# Define the list of species
species.list <- c('Barbus barbus',  
                  'Rutilus rutilus',  
                  'Leuciscus leuciscus',  
                  'Leuciscus cephalus',  
                  'Perca fluviatilis',  
                  'Abramis brama',  
                  'Gobio gobio',  
                  'Esox lucius',  
                  'Sander lucioperca',  
                  'Anguilla anguilla',  
                  'Platichthys flesus',  
                  'Gasterosteus aculeatus',  
                  'Phoxinus phoxinus',  
                  'Barbatula barbatula',  
                  'Abramis bjoerkna',  
                  'Alburnus alburnus',  
                  'Pomatoschistus microps',  
                  'Thymallus thymallus',  
                  'Cottus gobio',  
                  'Cobitis taenia',  
                  'Gymnocephalus cernuus',  
                  'Lampetra planeri',  
                  'Scardinius erythrophthalmus',  
                  'Tinca tinca',  
                  'Cyprinus carpio',  
                  'Salmo trutta',  
                  'Pungitius pungitius',  
                  'Salmo salar',  
                  'Carassius auratus')

set.seed(1)

# Loop through each species
for (species.name in species.list) {
  
  cat("Processing", species.name, "...\n")
  
  # Fetch occurrence data from GBIF
  species.occurrences <- occ_search(scientificName = species.name, country = 'GB',
                                    basisOfRecord ='HUMAN_OBSERVATION',
                                    limit = 100000,
                                    year = '2000,2025')
  
  #Write CSV of records
  occurrence_data <- species.occurrences$data
  occurrence_filepath <- file.path("Data", "Outputs", "Models", "Trent Species", paste0(species.name, "occurrences.csv")) 
  write.csv(occurrence_data, occurrence_filepath)
  
  # Clean data
  species.occurrences.clean <- species.occurrences$data %>% 
    dplyr::select(decimalLongitude, decimalLatitude) %>% 
    na.omit() %>% 
    dplyr::distinct()
  
  #Write CSV of cleaned data
  
  occurrence_clean_filepath <- file.path("Data", "Outputs", paste0(species.name, "cleaned_occurrences.csv")) 
  write.csv(species.occurrences.clean, occurrence_clean_filepath)
  
  # Extract environmental variables
  species.occurrences.vars <- terra::extract(envs.final, species.occurrences.clean, ID = FALSE)
  species.occurrences.vars.na.filt <- na.omit(species.occurrences.vars)
  
  # Calculate MESS
  species.occurences.MESS <- predicts::mess(envs.final, species.occurrences.vars.na.filt)
  
  # Convert occurrences to spatial object
  species.occurrences.sf <- sf::st_as_sf(species.occurrences.clean, coords = c("decimalLongitude", "decimalLatitude"),
                                         crs = terra::crs(envs.final))
  
  # Transform projection
  eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
  species.occurrences.sf <- sf::st_transform(species.occurrences.sf, eckertIV)
  
  # Create buffer zone
  species.occurrences.buffer <- sf::st_buffer(species.occurrences.sf, dist = 50000) %>% 
    sf::st_union() %>% 
    sf::st_sf() %>% 
    sf::st_transform(crs = terra::crs(envs.final))
  
  # Crop and mask environmental data
  envs.bg <- terra::crop(envs.final, species.occurrences.buffer)
  envs.bg <- terra::mask(envs.bg, species.occurrences.buffer)
  
  # Sample background points
  bg <- terra::spatSample(envs.bg, size = 10000, na.rm = TRUE, values = FALSE, xy = TRUE) %>% 
    as.data.frame()
  
  colnames(bg) <- colnames(species.occurrences.clean)
  
  # Model tuning arguments
  tune.args <- list(fc = c("L", "LQ", "LQH"), rm = 1:5)
  
  # Run model
  e.mx <- ENMevaluate(occs = species.occurrences.clean, envs = envs.final, bg = bg,
                      algorithm = 'maxnet', partitions = 'block',
                      tune.args = tune.args, parallel = TRUE)
  
  # Select optimal model
  res <- eval.results(e.mx)
  opt.seq <- res %>% 
    filter(!is.na(or.10p.avg)) %>% 
    filter(or.10p.avg == min(or.10p.avg)) %>% 
    filter(cbi.val.avg == max(cbi.val.avg))
  
  mod.seq <- eval.models(e.mx)[[opt.seq$tune.args]]
  
  # Generate predictions
  pred.seq <- eval.predictions(e.mx)[[as.character(opt.seq$tune.args)]]
  
  # Save results
  output_filename <- file.path("Data", "Outputs", "Models", "Trent Species", paste0(species.name, "_model.tif"))
  terra::writeRaster(pred.seq, filename = output_filename, overwrite = TRUE)
  
  cat("Completed processing for", species.name, "\n\n")
  
  closeAllConnections()
  
}