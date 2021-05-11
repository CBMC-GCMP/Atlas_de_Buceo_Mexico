library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggthemes)
library(patchwork)

spdf_mx <- st_transform(st_as_sf(ne_countries(country = 'mexico')), crs = 4326)


mpa_layer <- st_read("shp/old/final_mpa_layer.shp")


mpa_metadata <- read.csv("shp/old/MPA_metadata.csv", na.strings = "")

mpa_layer <- mpa_layer %>% 
        select(ANP, ID, nombre_1)

final_mpa <- merge(mpa_layer, mpa_metadata, by = c("ID", "ANP"))

final_mpa$Pesca <- replace_na(final_mpa$Pesca, "NA")

final_mpa$Pesca <- factor(final_mpa$Pesca, levels = c("NA", "ND", "NO", "SI"), labels = c("Sin manejo", "No especificado", "Prohibida", "Permitida"))

final_mpa$Buceo <- replace_na(final_mpa$Buceo, "NA")

final_mpa$Buceo <- factor(final_mpa$Buceo, levels = c("NA", "ND", "NO", "SI"), labels = c("Sin manejo", "No especificado", "Prohibido", "Permitido"))


dive_sites <- st_read('shp/dive_sites.shp')



test <- st_join(dive_sites, final_mpa)

test <- test %>% 
        mutate(protection_level = ifelse(is.na(ANP), "Sin Proteccion", "Into MPA"), 
               core_zone = ifelse(Pesca == "Prohibida", "Pesca Prohibida", "Amortiguamiento"),
               protection_level = ifelse(is.na(core_zone), "Sin Proteccion", core_zone)) %>% 
        select(-core_zone) %>% 
        mutate(protection_level = factor(protection_level, levels = c("Sin Proteccion", "Amortiguamiento", "Pesca Prohibida"))) 
