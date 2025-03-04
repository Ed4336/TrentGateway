#libraries----
# Install required packages only if not already installed
required_packages <- c(
  "ENMeval", "raster", "dplyr", "sf", "geodata", "terra", 
  "remotes", "rasterVis", "ggplot2", "ecospat", "parallel", "rJava", "rgbif")

# Install missing packages
new_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) {
  install.packages(new_packages, quiet = TRUE)
}

# Install rmaxent and ENMTools from GitHub if not already installed
if(!requireNamespace("rmaxent", quietly = TRUE)) {
  remotes::install_github("johnbaums/rmaxent", quiet = TRUE)
}

if(!requireNamespace("ENMTools", quietly = TRUE)) {
  remotes::install_github("danlwarren/ENMTools", quiet = TRUE)
}

# Load required libraries
library(ENMTools)
library(ENMeval)
library(raster)
library(sf)
library(geodata)
library(terra)
library(rmaxent)
library(rasterVis)
library(ecospat)
library(tidyverse)
library(parallel)
library(rJava)
library(rgbif)

#Wrangling all environmental variables####

#Get UK extent####
uk.extent <- gadm(country = 'GBR', level = 0,path = "Data")
uk.extent
uk.extent.df <- c(-8.649996, 1.764393, 49.86531, 60.84548)
plot(uk.extent)

#Get landcover data####

dir.create("Data/Envt layers")

download.file("http://data.earthenv.org/streams/landcover_average.nc",
              paste("Data/Envt layers","landcover_average.nc", sep = "/"), mode = "wb")

#add downloaded layers to raster stack
landcover.stack <- raster::stack("Data/Envt layers/landcover_average.nc")

#rename layers
names(landcover.stack) <- c('evergreen/deciduous_needleleaf_trees', 'evergreen_broadleaf_trees', 
                            'deciduous_broadleaf_trees', 'mixed_other_trees', 'shrubs', 
                            'herbaceous_vegetation', 'cultivated_and_managed_vegetation', 
                            'regularly_flooded_shrub/herbaceous_vegetation', 'urban/built-up', 
                            'snow/ice','barren_lands/sparse_vegetation', 'open_water')

#check there are the right amount of layers present
raster::nlayers(landcover.stack)

#crop raster layers
landcover.uk <- raster::crop(landcover.stack, uk.extent.df)
plot(landcover.uk)

#create new folder to house layers
dir.create("Data/Envt layers/landcover uk")

#write to disk
terra::writeRaster(landcover.uk, 
                   file.path("Data/Envt layers/landcover uk", names(landcover.stack)), 
                   bylayer=T, format="GTiff", overwrite=T)

#Get bioclim data ####

download.file("http://data.earthenv.org/streams/hydroclim_average+sum.nc",
              paste("Data/Envt layers","hydroclim_average+sum.nc", sep = "/"), mode = "wb")

bioclim.stack <- raster::stack("Data/Envt layers/hydroclim_average+sum.nc")

names(bioclim.stack) <- c("Annual Mean Upstream Temperature",
                          "Mean Upstream Diurnal Range","Upstream Isothermality",
                          "Upstream Temperature Seasonality",
                          "Maximum Upstream Temperature of Warmest Month",
                          "Minimum Upstream Temperature of Coldest Month",
                          "Upstream Temperature Annual Range",
                          "Mean Upstream Temperature of Wettest Quarter",
                          "Mean Upstream Temperature of Driest Quarter",
                          "Mean Upstream Temperature of Warmest Quarter",
                          "Mean Upstream Temperature of Coldest Quarter",
                          "Annual Upstream Precipitation",
                          "Upstream Precipitation of Wettest Month",
                          "Upstream Precipitation of Driest Month",
                          "Upstream Precipitation Seasonality (Coefficient of Variation)",
                          "Upstream Precipitation of Wettest Quarter",
                          "Upstream Precipitation of Driest Quarter",
                          "Upstream Precipitation of Warmest Quarter",
                          "Upstream Precipitation of Coldest Quarter")

raster::nlayers(bioclim.stack)

bioclim.uk <- raster::crop(bioclim.stack,uk.extent.df)
plot(bioclim.uk)


dir.create("Data/Envt layers/bioclim uk")

terra::writeRaster(bioclim.uk, 
                   file.path("Data/Envt layers/bioclim uk", names(bioclim.stack)), 
                   bylayer=T, format="GTiff", overwrite=T)

#Get topo data####

download.file("http://data.earthenv.org/streams/flow_acc.nc",
              paste("Data/Envt layers","flow_acc.nc", sep = "/"), mode = "wb")

download.file("http://data.earthenv.org/streams/slope.nc",
              paste("Data/Envt layers","slope.nc", sep = "/"), mode = "wb")

download.file("http://data.earthenv.org/streams/elevation.nc",
              paste("Data/Envt layers","elevation.nc", sep = "/"), mode = "wb")

topo.stack <- raster::stack(c("Data/Envt layers/flow_acc.nc",
                              "Data/Envt layers/slope.nc",
                              "Data/Envt layers/elevation.nc"))

names(topo.stack) <- c("dem_min",
                       "dem_max",
                       "dem_range",
                       "dem_avg",
                       "slope_min",   
                       "slope_max",
                       "slope_range",
                       "slope_avg",
                       "flow_length", 
                       "flow_acc")

raster::nlayers(topo.stack)

topo.uk <- raster::crop(topo.stack,uk.extent.df)
plot(topo.uk)

dir.create("Data/Envt layers/topo uk")

terra::writeRaster(topo.uk, 
                   file.path("Data/Envt layers/topo uk", names(topo.stack)), 
                   bylayer=T, format="GTiff", overwrite=T)

#collect all variables together
all.vars <- raster::stack(
  unlist(lapply(c("Data/Envt layers/topo uk", 
                  "Data/Envt layers/landcover uk", 
                  "Data/Envt layers/bioclim uk"), 
                function(folder) list.files(folder, pattern = "\\.tif$", full.names = TRUE)))
)

# Check the stack
print(all.vars)

#Occurrence data####

#Reproducibility####
set.seed(12)

#Load data####

# Define species name
species_name <- "Rutilus rutilus"

# Fetch occurrence data from GBIF
species_occurrences <- occ_search(scientificName = species_name, country = "GB")

# Convert occurrence data to a data frame
species_occurrences_df <- as.data.frame(species_occurrences$data)

# Extract and clean coordinate data
species_coordinates <- species_occurrences_df %>% 
  dplyr::select(species, decimalLatitude, decimalLongitude, year) %>%  # Keep necessary columns
  filter(year > 2000) %>%  # Keep only recent observations
  na.omit() %>%  # Remove NA values
  dplyr::select(decimalLongitude, decimalLatitude) %>%  # Only keep coordinates
  distinct()  # Remove duplicates

# Visualize raw occurrence points
ggplot(species_coordinates, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point()

# Data thinning: ensure only 1 occurrence per raster cell
species_cells <- raster::extract(all.vars[[1]], species_coordinates, cellnumbers = TRUE)  # Extract raster cell numbers
duplicates_mask <- duplicated(species_cells[,1])  # Identify duplicate raster cells
species_coordinates_thinned <- species_coordinates[!duplicates_mask, ]  # Keep only the first instance of duplicates

# Visualize thinned occurrences
ggplot(species_coordinates_thinned, aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_point(color = "blue") +
  ggtitle("Thinned Occurrences: One per Raster Cell")

# Multivariate Environmental Similarity Surface (MESS) ####
# Select pertinent variables for species of choice
# Create a vector with the names of the selected variables

selected_variables <- c(
  "Annual.Mean.Upstream.Temperature",
  "Upstream.Precipitation.of.Wettest",
  "flow_acc",
  "slope_avg",
  "open_water",
  "flow_length",
  "Upstream.Precipitation.Seasonality..Coefficient.of.Variation",
  "dem_max",
  "barren_lands.sparse_vegetation",
  "shrubs",
  "deciduous_broadleaf_trees",
  "evergreen.deciduous_needleleaf_trees",
  "evergreen_broadleaf_trees",
  "mixed_other_trees",
  "Upstream.Temperature.Seasonality",
  "urban.built"
)

# Select only the desired layers
selected_rasters <- all.vars[[selected_variables]]

# Remove categorical variables before proceeding
selected_rasters <- selected_rasters[[!sapply(selected_rasters, is.factor)]]

# Calculate environmental similarity between occurrence points and environmental layers
occurrence_similarity <- similarity(selected_rasters, species_coordinates)
env_similarity_surface <- occurrence_similarity$similarity_min

# Visualize environmental similarity surface and occurrence points
occurrence_points_sp <- sp::SpatialPoints(species_coordinates)  # Convert coordinates to SpatialPoints
rasterVis::levelplot(env_similarity_surface, main="Environmental Similarity Surface", margin=FALSE) +
  latticeExtra::layer(sp.points(occurrence_points_sp, col="black"))

# Transform occurrence points to a spatial object (sf)
species_occurrences_sf <- st_as_sf(species_coordinates_thinned, coords = c("decimalLongitude", "decimalLatitude"), crs=4326)

# Transform projection from WGS84 (degrees) to Eckert IV (meters)
eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
species_occurrences_sf <- sf::st_transform(species_occurrences_sf, crs=eckertIV)

# Visualize transformed occurrence points
plot(st_geometry(species_occurrences_sf), main="Transformed Occurrence Points (Eckert IV)")

# Generate pseudo-absence from local background ####
# Buffer all occurrences by 50km
occurrence_buffer <- sf::st_buffer(species_occurrences_sf, dist=50000) %>% 
  sf::st_union() %>% 
  sf::st_sf() %>% 
  sf::st_transform(crs = raster::crs(selected_rasters))

# Plot raster and buffer
plot(selected_rasters[[1]], main=names(selected_rasters)[1])
points(species_coordinates)
plot(occurrence_buffer, border="blue", lwd=3, add=T)

# Crop environmental rasters to buffered extent
buffered_extent <- raster::crop(selected_rasters, occurrence_buffer)

# Mask rasters to shape of buffers
buffered_extent <- raster::mask(buffered_extent, occurrence_buffer)

# Plot final raster
plot(buffered_extent[[1]], main=names(selected_rasters)[1])
points(species_coordinates_thinned)
plot(occurrence_buffer, border="blue", lwd=3, add=T)

#Random sample within buffered area####  
bg <- dismo::randomPoints(buffered_extent,n=10000) %>% 
  as.data.frame()
colnames(bg) <- colnames(species_coordinates_thinned)

plot(buffered_extent[[1]])
points(bg,pch=20, cex=0.2)

#Spatial data partitioning####

head(bg)
head(species_coordinates_thinned)
block <- get.block(species_coordinates_thinned,bg,orientation = "lat_lon")

#check for even no. of occurrences per partition
table(block$occs.grp)

#plot partitions on to a predictor raster for visualisation
evalplot.grps(pts = species_coordinates_thinned,pts.grp = block$occs.grp,envs = buffered_extent)

#review correlation of environmental variables
cor.matrix <- raster.cor.matrix(selected_rasters)

cor.matrix.df <- cor.matrix %>%
  rownames_to_column(var = "Layer1") %>%
  pivot_longer(cols = -Layer1, names_to = "Layer2", values_to = "Correlation")
#visualise
ggplot(cor.matrix.df, aes(x = Layer1, y = Layer2, fill = Correlation))+
  geom_tile()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Running model####

#Building a separate model for combinations of feature classes (Linear, Quadratic, Hinge, etc.) and regularisation multipliers
#This uses the maxnet package not maxent.jar.
#Other features are occs - occurrence data, envs - environmental variables, bg - background datapoints
mx.1 <- ENMevaluate(occs = species_coordinates_thinned, envs = selected_rasters, bg=bg,
                         algorithm = "maxent.jar", partitions = "block",
                         tune.args=list(fc="L", rm=1:2), parallel = T, numCores = 6)

mx.1


#Building a model which uses a wider range of feature classes and regularisation multipliers
mx <- ENMevaluate(occs = species_coordinates_thinned, envs = selected_rasters, bg=bg,
                       algorithm = "maxent.jar", partitions = "block",
                       tune.args=list(fc=c("L","LQ", "LQH", "H"), rm=1:5), parallel = T, numCores = 6)

# This errors with hinge model variants included if using the maxnet algorithm
mx

eval.variable.importance(mx)

# We can plot more than one statistic at once with ggplot facetting.
evalplot.stats(e = mx, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm")

# Sometimes the error bars make it hard to visualize the plot, so we can try turning them off.
evalplot.stats(e = mx, stats = c("or.mtp", "auc.val"), color = "fc", x.var = "rm", 
               error.bars = FALSE)

# Overall results
res <- eval.results(mx)

# Select the model with delta AICc equal to 0, or the one with the lowest AICc score.
# In practice, models with delta AICc scores less than 2 are usually considered statistically equivalent.

opt.aicc <- res %>% filter(delta.AICc == 0)
opt.aicc

# This dplyr operation executes the sequential criteria explained above.
opt.seq <- res %>% 
  filter(or.10p.avg == min(or.10p.avg)) %>% 
  filter(auc.val.avg == max(auc.val.avg))
opt.seq


# We can select a single model from the ENMevaluation object using the tune.args of our optimal model.

mod.seq <- eval.models(mx)[[opt.seq$tune.args]]

plot(mod.seq)
#The below works for models using the maxnet algorithm

# Here are the non-zero coefficients in our model.
#mod.seq$betas

# And these are the marginal response curves for the predictor variables wit non-zero 
# coefficients in our model. We define the y-axis to be the cloglog transformation, which
# is an approximation of occurrence probability (with assumptions) bounded by 0 and 1
# (Phillips et al. 2017).

#plot(mod.seq, type = "cloglog")

#maxent.jar models are built slightly differently and use the below
#maxent.jar models use the dismo::response() function for this
dismo::response(eval.models(mx)[[opt.seq$tune.args]])

# We can select the model predictions for our optimal model the same way we did for the 
# model object above.
pred.seq <- eval.predictions(mx)[[opt.seq$tune.args]]
pred.seq

#change background col of plot to black 
#par(bg="black"")

plot(pred.seq)

#crop model to the trent####
riverLines_filtered_wgs84 <- st_transform(riverLines_filtered, crs(pred.seq))
bbox_wgs84 <- st_bbox(riverLines_filtered_wgs84)

pred.seq_trent <- crop(pred.seq, extent(riverLines_filtered_wgs84))

pred.seq_trent <- mask(pred.seq_trent, riverLines_filtered_wgs84)

plot(pred.seq_trent)

pred.seq_trent_df <- as.data.frame(rasterToPoints(pred.seq_trent), stringsAsFactors = FALSE)
colnames(pred.seq_trent_df) <- c("x", "y", "value")  # Rename columns

ggplot() +
  #annotation_map_tile(type = 'osm', zoom = 10) +  # Set zoom level
  geom_tile(data = pred.seq_trent_df, aes(x = x, y = y, fill = value)) +  # Use geom_tile instead of geom_raster
  scale_fill_viridis_c(option = "magma", name = "MaxEnt Prediction") +  # Adjust color scale
  geom_sf(data = riverLines_filtered_wgs84, color = "blue", size = 1) +  # Plot the vector layer
  theme_minimal() +
  labs(title = "MaxEnt Model Prediction (River Trent)",
       x = "Longitude",
       y = "Latitude") +
  theme(legend.position = "right")
