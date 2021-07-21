library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggthemes)
library(patchwork)

#Old shapefile
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

#Updated MPA & Metadata

mpa_v1 <- st_read("shp/MPA_MX_v1.0_01032021/MPA_MX_c1-0.shp")

metadata_v1.01 <- read.csv("data/MPA_metadata_v.1.01_21072021.csv", na.strings ="")
metadata_v1.01[, 23] <- NULL

metadata_v1.01 <- metadata_v1.01 %>% 
        rename(
                pesca_comercial= Pesca.Comercial,
                pesca_deportiva= Pesca.Deportiva,
                Pesca= Pesca,
                Buceo= Cat_Buceo,
                PdM= Lo.que.dice.el.PM
        )
mpa_v1 <- mpa_v1 %>%
        select(ANP, ID, nombr_1)

mpa_v1.01 <- merge(mpa_v1, metadata_v1.01, by = c("ID", "ANP"))

mpa_v1.01$Pesca <- replace_na(mpa_v1.01$Pesca, "NA")

mpa_v1.01$Pesca <- factor(mpa_v1.01$Pesca, levels = c("NA", "ND", "NO", "SI"), labels = c("Sin manejo", "No especificado", "Prohibida", "Permitida"))

mpa_v1.01$Buceo <- replace_na(mpa_v1.01$Buceo, "NA")

mpa_v1.01$Buceo <- factor(mpa_v1.01$Buceo, levels = c("NA", "NO SE MENCIONA", "AMBIGUO", "PROHIBIDO", "PERMITIDO"), labels = c("Sin manejo", "No se menciona", "Ambiguo", "Prohibido", "Permitido"))

st_write(mpa_v1.01, "shp/MPA_MX_v1.01_21072021/MPA_MX_c1-01.shp")

unique(final_mpa$Buceo)
unique(mpa_v1.01$Pesca)

        
