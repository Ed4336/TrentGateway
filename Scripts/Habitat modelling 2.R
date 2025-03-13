#libraries----

# Load packages -- the order here is important because some pkg functions overwrite others.
library(ENMeval)
library(geodata)
library(usdm)
library(blockCV)
library(sf)
library(tibble)
library(ggplot2)
library(knitr)
library(terra)
library(dplyr)
library(rgbif)

set.seed(12)

#load environmental layers----
#NNB the below is now redundant and has been fixed, but including for posterity in the full code - remove this for final version
#NB this currently uses the raster package to process the downloaded ncdf files, whilst this package is outdated it reads files using ncdf4 package, which allows for faster reading of files and more convienent processing of files with many layers - in this instance each file contains multiple layers, which are rasters themselves - see https://gis.stackexchange.com/questions/413105/terrarast-vs-rasterbrick-for-loading-in-nc-files

##Get UK extent----
uk.extent <- gadm(country = 'GBR', level = 0,path = "Data")
uk.extent
uk.extent.df <- c(-8.649996, 1.764393, 49.86531, 60.84548)
plot(uk.extent)

##Get landcover data----
#NB when using terra::rast a warning reading 'X axis unit (degrees_east) is different from Y axis unit (degrees_north). SRS will ignore axis unit and be likely wrong. (GDAL error 1)' will display. This occurs because the units are labelled differently, despite both being decimal degrees. This is an issue with how the ncdf file has been organised.


dir.create("Data/Envt layers")

download.file("http://data.earthenv.org/streams/landcover_average.nc",
              paste("Data/Envt layers","landcover_average.nc", sep = "/"), mode = "wb")

#add downloaded layers to raster stack
landcover.stack <- terra::rast("Data/Envt layers/landcover_average.nc")

#rename layers
names(landcover.stack) <- c('evergreen/deciduous_needleleaf_trees',
                            'evergreen_broadleaf_trees', 
                            'deciduous_broadleaf_trees',
                            'mixed_other_trees',
                            'shrubs', 
                            'herbaceous_vegetation',
                            'cultivated_and_managed_vegetation', 
                            'regularly_flooded_shrub/herbaceous_vegetation',
                            'urban/built-up', 
                            'snow/ice','barren_lands/sparse_vegetation',
                            'open_water')

##Get bioclim data----

download.file("http://data.earthenv.org/streams/hydroclim_average+sum.nc",
              paste("Data/Envt layers","hydroclim_average+sum.nc", sep = "/"), mode = "wb")

bioclim.stack <- terra::rast("Data/Envt layers/hydroclim_average+sum.nc")

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

##Get topo data----

download.file("http://data.earthenv.org/streams/flow_acc.nc",
              paste("Data/Envt layers","flow_acc.nc", sep = "/"), mode = "wb")

download.file("http://data.earthenv.org/streams/slope.nc",
              paste("Data/Envt layers","slope.nc", sep = "/"), mode = "wb")

download.file("http://data.earthenv.org/streams/elevation.nc",
              paste("Data/Envt layers","elevation.nc", sep = "/"), mode = "wb")

topo.stack <- terra::rast(c("Data/Envt layers/flow_acc.nc",
                              "Data/Envt layers/slope.nc",
                              "Data/Envt layers/elevation.nc"))

names(topo.stack) <- c("flow_length", 
                       "flow_acc",
                       "slope_min",   
                       "slope_max",
                       "slope_range",
                       "slope_avg",
                       "dem_min",
                       "dem_max",
                       "dem_range",
                       "dem_avg")

##Collect all layers together and crop----

envs <- c(landcover.stack,
          bioclim.stack,
          topo.stack)

envs.cropped <- terra::crop(envs, uk.extent)

#check all of the layers have been downloaded, stacked and cropped correctly
print(envs.cropped)

#plot the first layer to visualise
plot(envs.cropped[[1]])

##Select variables to use----

#display all of the names and numbers of the variables available
print(names(envs.cropped))

#select the variables required - easier to enter the numbers displayed in the print call here
envs.selected <- envs.cropped[[c(9,11,13,19,24,32,37,41)]]

#it is a good idea to check for autocorrelation between variables, whilst this is somewhat redundant when modelling streams, as all variables are typically correlated to some degree it is best practice to reduce this if possible
envs.vif <- usdm::vif(envs.selected)

#view the VIF - scores >10 are often problematic as this shows high levels of correlation with other variables. Here it was to be expected, we are modelling streams, which by their nature are highly correlated. With the variables selected here Avg precipitation and flow length score high VIF - this makes sense when considering the other variables, but we do want to keep these.

print(envs.vif)

#here flow is recommended for removal, all scores now look pretty good. barrier mapping is often inconsistent, so excluding this is probably advisable in this instance.

envs.vif.rem <- usdm::vifstep(envs.selected)

print(c(envs.vif.rem, 'Excluded variables:', envs.vif.rem@excluded))

envs.vif.rem.names <- envs.vif.rem@excluded

#trim our environmental variables to exclude those with high levels of VIF

envs.final <- envs.selected[[!names(envs.selected) %in% envs.vif.rem.names]]

#write these to disk - we no longer have to go through this process again
terra::writeRaster(envs.final,
                   filename = file.path("Data", "Envt layers", "envs_final.tif"),
                   overwrite = TRUE)

envs.final <- terra::rast("Data/Envt layers/envs_final.tif")

#Get occurrence data----

#Enter the scientific name for the species of interest - at the moment this only supports a single species at a time. This does also reduce the processing time

species.name <- 'Rutilus rutilus'

#Fetch occurrence data from GBIF

#LOOK IN TO MODIFYING QUERY TO INCREASE NUMBER OF RESULTS AND SPECIFY OBSERVATION TYPE AND YEAR ETC
species.occurrences <- occ_search(scientificName = species.name, country = 'GB')

#Clean data
species.occurrences.clean <- species.occurrences$data %>% 
  dplyr::filter(year>2000) %>% 
  dplyr::select(decimalLongitude,
                decimalLatitude) %>% 
  #remove any data that doesnt have complete coordinate sets
  na.omit() %>% 
  #remove any duplicated occurrences
  dplyr::distinct()

##Spatial thinning of data----

#FOR REVIEW THIS IS LEAVING VERY SMALL AMOUNTS OF DATA LEFTOVER

#Spatial autocorrelation occurs when there are multiple occurrences at the same location(s) this biases these locations and can negatively affect model performance.

#species.occurrences.cells <- terra::extract(envs.final[[4]], species.occurrences.clean, cellnumbers = T, ID = F )

#this can be used to visualise how the occurrences are being decided for duplication
#species.occurrences.clean$cell_number <- species.occurrences.cells[, 1]

#identify the duplicate occurrences within cells
#species.occurrences.cells.dups <- duplicated(species.occurrences.cells[,1])

#remove duplicates so there is only one occurrence per raster cell

#species.occurences.thin <- species.occurrences.clean[!species.occurrences.cells.dups,]

#MESS (multivariate environmental similarity)----
#this is used to assess similatiry between layers used for predictions - we don't want to train a model and project it to soemwhere wildly dissimilar - not really an issue here as this is the UK only.

#extract env variables at occ points
species.occurrences.vars <- terra::extract(envs.final, species.occurrences.clean, ID= F)

#remove any NAs in the dataset
species.occurrences.vars.na.filt <- na.omit(species.occurrences.vars)

#calculate the MESS for the envs
species.occurences.MESS <- predicts::mess(envs.final, species.occurrences.vars.na.filt)

#visualise
plot(species.occurences.MESS)

#as you would expect for the UK this is extremely similar for the vast majority

#Background datapoints----
#select background area from which to sample in order to provide background info on datapoints - we will create a buffered area surrounding the current occurrence points

#set occurences as a spatial object
species.occurrences.sf <- sf::st_as_sf(species.occurrences.clean, coords = c("decimalLongitude", "decimalLatitude"),
                                   crs = terra::crs(envs.final))

#transform the current projection from WGS to eckert IV which allows for a meters as a unit
eckertIV <- "+proj=eck4 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

species.occurrences.sf <- sf::st_transform(species.occurrences.sf, eckertIV)

#set buffer zone around each of the points - lets go for 50km as the UK is only small and out env layers arent too varied - we can then join this into a continuous polygon and take our background samples from there

species.occurrences.buffer <- sf::st_buffer(species.occurrences.sf, dist = 50000) %>% 
  sf::st_union() %>% 
  sf::st_sf() %>% 
  sf::st_transform(crs = terra::crs(envs.final))


plot(envs.final[[1]])
points(species.occurrences.clean)
plot(species.occurrences.buffer, border = 'blue', lwd = 2, add = T)

#crop envs to buffered extent and then mask to the shape of the rasters

envs.bg <- terra::crop(envs.final, species.occurrences.buffer)

envs.bg <- terra::mask(envs.bg, species.occurrences.buffer)

plot(envs.bg[[1]])
points(species.occurrences.clean)

#randomly sample background points from the buffered area

bg <- terra::spatSample(envs.bg, size = 10000, na.rm = T,
                        values = F, xy = T) %>% 
  as.data.frame() 

colnames(bg) <- colnames(species.occurrences.clean)

plot(envs.final[[1]])
points(bg)

#this is possible overkill, but we can tidy this as necessary

#Spatial partitioning----
#there are multiple methods to do this with varying pros and cons, see https://jamiemkass.github.io/ENMeval/articles/ENMeval-2.0-vignette.html for more

##k means clustering----

#assign the number of groups that you wish to use
grp.n <- 5

kmeans <- kmeans(species.occurrences.clean, grp.n)

species.occurrences.partition <- kmeans$cluster

evalplot.grps(pts = species.occurrences.clean, pts.grp = species.occurrences.partition, envs = envs.bg)

#when using a 'custom' partitioning method we need to assign identifiers for group and bg points

#assing background records

bg.partition <- rep(0, nrow(bg))

evalplot.grps(pts = bg, pts.grp = bg.partition, envs = envs.bg)

