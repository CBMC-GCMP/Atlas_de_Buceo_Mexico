library(sf)
library(googlesheets4)
library(tidyverse)

mpa <- st_read('shp/cleaned_mpas_mx.shp') %>% 
        select(-c(Poligono_1:Area_km2))


ds <- st_read("shp/dive_sites.shp")


ds <- ds %>% 
        mutate(protection = str_replace(protection, "Not take zone", "No Take Zone"), 
               state = str_replace(state, "_", " "),
               locality = str_replace(locality, "Banco Chinchorros", "Banco Chinchorro")) %>% 
        select(-c(popularity:accessibil))




df <- read_sheet('https://docs.google.com/spreadsheets/d/1DU2vztusyT-Cv0pUHTFcOVw3o_cZIVRqYov3gYDhy2g/edit#gid=0', sheet = 2)
ids <- read_sheet('https://docs.google.com/spreadsheets/d/1DU2vztusyT-Cv0pUHTFcOVw3o_cZIVRqYov3gYDhy2g/edit#gid=0', sheet = 1, skip=2) %>% 
        select(ID, NOMBRE)


df <- df %>% 
        select(ID, NOMBRE, CATEGORIA, PM, FECHA_PUB, ACTIVIDAD, PESCA)
df2 <- merge(df, ids, by = c("ID", "NOMBRE"), all.x = TRUE)

m_mpa <- merge(mpa, df, all.x = TRUE, by = "ID")


joined_df <- st_join(ds, mpa)

final <- joined_df %>% 
        select(id, region, Region_1, state, locality, site_name, latitude, longitude, protection, ANP, Categoria, Zonificaci, Subzonific)

final <- final %>%  
        janitor::clean_names() %>% 
        mutate(anp = replace_na(anp, "Sin Proteccion"),
               categoria = replace_na(categoria, "Sin Proteccion"), 
               region_1 = coalesce(region_1, region),
               region_1 = str_replace(region_1, "Yucatan Peninsula", "Península de Yucatán y Caribe Mexicano"),
               region_1 = str_replace(region_1, "Gulf of Mexico", "Planicie Costera y Golfo de México"),
               region_1 = str_replace(region_1, "Golfo de Mexico", "Planicie Costera y Golfo de México"),
               region_1 = str_replace(region_1, "Mexican Pacific", "Pacifico Mexicano"),
               region_1 = str_replace(region_1, "Northwest Mexico", "Noroeste Mexicano"),
        ) %>% 
        select(c(id, site_name), everything())


names(final) <- c("id_sitio", "nombre_sitio", "region",
                  "region_1", "estado", "localidad", 
                  "latitud", "longitud", "proteccion", 
                  "nombre_anp", "categoria", "zonificacion", "subzonificacion", "geometry"
)


dive_sites <- final

rm(list=setdiff(ls(), c("dive_sites", "mpa")))
