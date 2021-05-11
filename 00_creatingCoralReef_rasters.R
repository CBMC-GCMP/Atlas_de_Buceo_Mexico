# Joy Kumagai and Fabio
# Date: Feb 2021
# Raserizing Multiple Habitat Data
# Marine Habitat Protection Indicator or Marine Protection Index (MPI)


# Loading packages --------------------------------------------------------

library(sf)
library(tidyverse)
library(raster)
library(tools)
library(fasterize)
library(doParallel)
library(foreach)

# Loading data ------------------------------------------------------------

shapefiles <- list.files("shp/HabitatdataMX/", pattern = "\\.shp$", full.names = T)

r <- raster(test, res = 0.25)

# Rasterizing habitats with parallel processing ------------------------------------------------------------

cl <- makeCluster(9) 
registerDoParallel(cl)


foreach(i = 1:length(shapefiles)) %dopar% {
        require(tidyverse)
        require(sf)
        require(raster)
        require(tools)
        
        habitat <- st_read(shapefiles[i]) %>% 
                mutate(constant = 1) # for rasterization 
        
        
        # Setting Area 
        if ("REP_AREA_K" %in% colnames(habitat)) {
                habitat$area = habitat$REP_AREA_K
                if (class(habitat$area) != "numeric") {
                        habitat$area = 0 
                }
        } else { habitat$area = 0 }
        
        
        
        # Convert multipoints to points 
        if (unique(st_geometry_type(habitat)) == "MULTIPOINT") { 
                habitat <- st_cast(habitat, "POINT")
                print("Converted MULTIPOINT TO POINT")
        }
        
        # Convert Points and Polygons to Raster
        if (unique(st_geometry_type(habitat)) == "POINT") {
                if (sum(habitat$area) > 0) { # if there is area reported, buffer 
                        habitat <- habitat %>% 
                                mutate(area = ifelse(REP_AREA_K == 0, 1, REP_AREA_K),
                                       radius = (sqrt(area/3.14))/0.01)
                        print("Attempting to Buffer points")
                        habitat <- st_buffer(habitat, dist = habitat$radius)
                        
                        # Rasterize the resulting polygons 
                        print("Attempting to convert buffered points to raster")
                        habitatR <- rasterize(habitat, r, progress = "text", field = "constant")
                        
                } else {
                        # If there is no area reported, rasterize immediately 
                        print("Attempting to convert points to raster")
                        habitatR <- rasterize(habitat, r, progress = "text", field = "constant")
                }
        } else {
                # Rasterize polygons 
                print("Attempting to convert polygons to raster")
                habitatR <- fasterize::fasterize(habitat, r, field = "constant") 
        }
        
        #### Export ####
        writeRaster(habitatR, 
                    filename = paste0("shp/HabitatdataMX/", 
                                      file_path_sans_ext(basename(shapefiles[i])), 
                                      "_habitat.tif"),
                    overwrite = TRUE)
        print(paste0("Rasterized and written to ", file_path_sans_ext(basename(shapefiles[i])), "_habitat.tif" ))
        
}



## merging points and polygons 

## Full raster list
rasters <- list.files("shp/HabitatdataMX/", pattern = "\\.tif$", full.names = T)

do.call(merge, list(raster(rasters[1]), raster(rasters[2]))) %>% 
        writeRaster(., filename = "raster/CoralReefs_habitat.tif", overwrite = TRUE)

unlink(rasters)

