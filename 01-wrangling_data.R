
# Loading libraries -------------------------------------------------------

library(sf)
library(googlesheets4)
library(tidyverse)


# Loading data -------------------------------------------------------

mpa <- st_read('shp/cleaned_mpas_mx.shp') %>% 
        select(-c(Poligono_1:Area_km2))


dive_sites <- st_read('shp/dive_sites.shp')
dive_sites %>% 
        filter(state == "JALISCO", locality == "PUERTO_VALLARTA")

# Loading metadata table from google drive
metadata <- read_sheet('https://docs.google.com/spreadsheets/d/1DU2vztusyT-Cv0pUHTFcOVw3o_cZIVRqYov3gYDhy2g/edit#gid=0', sheet = 2)

# loading numbered IDs
ids <- read_sheet('https://docs.google.com/spreadsheets/d/1DU2vztusyT-Cv0pUHTFcOVw3o_cZIVRqYov3gYDhy2g/edit#gid=0', sheet = 1, skip = 2) %>% 
        select(ID, NOMBRE) %>% 
        rename(ANP = NOMBRE) 




mpa <- merge(mpa, ids, by = c("ANP"), all.x = TRUE) %>% 
        relocate(ID, .before = ANP)

names(mpa) <- c("ID" , "NOMBRE", "CODIGO_SIG", "REGION", "ESTADO", "MUNICIPIO", "ZONA_OR", "SUBZONA_OR", "CATEGORIA_MOD", "geometry")

# Dive sites data wrangling -------------------------------------------------------

dive_sites <- dive_sites %>%
        mutate(
                protection = str_replace(protection, "Not take zone", "No Take Zone"),
                state = str_replace(state, "_", " "),
                locality = str_replace(locality, "Banco Chinchorros", "Banco Chinchorro")
        ) %>%
        select(-c(popularity:accessibil))




df <- metadata %>% select(ID, NOMBRE, CATEGORIA, PM, FECHA_PUB, ACTIVIDAD, PESCA, ZONA, SUBZONA)


m_mpa <- merge(mpa, df, all.x = TRUE, by = c("ID"))


joined_df <- st_join(dive_sites, mpa)

final <- joined_df %>% 
        select(id, region, state, locality, site_name, latitude, longitude, protection, ID, NOMBRE, REGION, ESTADO, MUNICIPIO, ZONA_OR, SUBZONA_OR, CATEGORIA_MOD)

final <- final %>%  
        janitor::clean_names() %>% 
        mutate(anp = replace_na(nombre, "Sin Proteccion"),
               categoria = replace_na(categoria_mod, "Sin Proteccion"),
               region_2 = coalesce(region_2, region),
               region_2 = str_replace(region_2, "Yucatan Peninsula", "Península de Yucatán y Caribe Mexicano"),
               region_2 = str_replace(region_2, "Gulf of Mexico", "Planicie Costera y Golfo de México"),
               region_2 = str_replace(region_2, "Golfo de Mexico", "Planicie Costera y Golfo de México"),
               region_2 = str_replace(region_2, "Mexican Pacific", "Pacifico Mexicano"),
               region_2 = str_replace(region_2, "Northwest Mexico", "Noroeste Mexicano"),
        ) %>%
        select(c(id, site_name), everything())


names(final) <- c("id_sitio", "nombre_sitio", "region",
                  "region_1", "estado", "localidad", 
                  "latitud", "longitud", "proteccion", 
                  "nombre_anp", "categoria", "zonificacion", "subzonificacion", "geometry"
)


dive_sites <- final

rm(list=setdiff(ls(), c("dive_sites", "mpa")))

final %>% 
        select(c(id_2, nombre, zona_or, subzona_or, categoria_mod), everything()) %>% 
        filter(!is.na(id_2))



write.csv(as.data.frame(mpa) %>% 
                  select(-geometry),
          "mpa_cleaned.csv", row.names = F)

write.csv(
        
        final %>% 
                select(c(id_2, nombre, zona_or, subzona_or, categoria_mod), everything()) %>% 
                filter(is.na(id_2)),
        "outputs/unprotected_sites.csv", row.names = F
        
)
