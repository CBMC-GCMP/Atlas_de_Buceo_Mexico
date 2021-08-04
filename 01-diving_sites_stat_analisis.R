library(tidyverse)
library(sf)
library(ggthemes)
library(rnaturalearth)
spdf_mx <- st_transform(st_as_sf(ne_countries(country = 'mexico')), crs = 4326)


mpas <- st_read('shp/MPAs_Merged/MPAs_merged.shp')


## testing summarising
mpas$Categoria <- str_replace(mpas$Categoria, "Zona de Amortiguamento", "Zona de Amortiguamiento")

mpas$Categoria <- factor(mpas$Categoria, levels = c("Zona de Amortiguamiento", "Zona Núcleo", "Sin PM", "APFF"))

mpas <- mpas %>% 
        mutate(Categoria = replace_na(Categoria, "Sin PM"))
       
mpa_category <- mpas %>% 
        group_by(Categoria) %>% 
        summarise()




(p1 <- ggplot(mpa_category)+
                geom_sf(data = spdf_mx, 
                        fill = "gray90", 
                        col = NA)+
                geom_sf(data = mpa_category, 
                        aes(fill = Categoria), 
                        col = "#f03b20")+
                scale_color_brewer(palette = "Set1")+
                coord_sf(xlim = c(-118, -85), 
                         ylim = c(13, 32), 
                         expand = TRUE) +
                theme_bw(base_family = "Lato")+
                guides(color = guide_legend(nrow = 3,
                                            byrow = TRUE, 
                                            override.aes = list(size=5)))+
                theme_map()+
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5, size = 12),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = "right"))
