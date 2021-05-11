library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggthemes)
library(patchwork)
library(hrbrthemes)

spdf_mx <- st_transform(st_as_sf(ne_countries(country = 'mexico')), crs = 4326)


mpa_layer <- st_read("shp/old/final_mpa_layer.shp")


mpa_metadata <- read.csv("shp/old/MPA_metadata.csv", na.strings="")

mpa_layer <- mpa_layer %>% 
        select(ANP, ID)

final_mpa <- merge(mpa_layer, mpa_metadata, by = c("ID", "ANP"))

final_mpa$Pesca <- replace_na(final_mpa$Pesca, "NA")

final_mpa$Pesca <- factor(final_mpa$Pesca, levels = c("NA", "ND", "NO", "SI"), labels = c("Sin manejo", "No especificado", "Prohibida", "Permitida"))

final_mpa$Buceo <- replace_na(final_mpa$Buceo, "NA")

final_mpa$Buceo <- factor(final_mpa$Buceo, levels = c("NA", "ND", "NO", "SI"), labels = c("Sin manejo", "No especificado", "Prohibido", "Permitido"))


(p1 <- final_mpa %>% 
                ggplot() +
                geom_sf(data = spdf_mx, 
                        fill = "gray90", 
                        col = NA) +
                geom_sf(data = final_mpa, 
                        aes(fill = Pesca), 
                        col = NA) +
                coord_sf(xlim = c(-118, -85), 
                         ylim = c(13, 32), 
                         expand = TRUE) +
                facet_wrap(~Pesca) +
                scale_fill_manual(values = c("gray30", "#fee391", "firebrick", "#005a32"), aesthetics = c("color", "fill"), labels = c("No especificado", "Prohibido", "Permitido", "Sin Manejo")) +
                #scale_color_manual(values = c(, "#fc4e2a", "#005a32", "black"), labels = c("No especificado", "Prohibido", "Permitido", "Sin Manejo")) +
                labs(subtitle = "Pesca")+
                theme_bw(base_family = "Lato") +
                theme_map() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5, size = 12),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = ""))



(p2 <- final_mpa %>% 
                ggplot() +
                geom_sf(data = spdf_mx, 
                        fill = "gray90", 
                        col = NA) +
                geom_sf(data = final_mpa, 
                        aes(fill = Buceo), 
                        col = NA) +
                coord_sf(xlim = c(-118, -85), 
                         ylim = c(13, 32), 
                         expand = TRUE) +
                facet_wrap(~Buceo) +
                scale_fill_manual(values = c("gray30", "#fee391", "firebrick", "#005a32"), aesthetics = c("color", "fill"), labels = c("No especificado", "Prohibido", "Permitido", "Sin Manejo")) +
                labs(subtitle = "Buceo") +
                theme_bw(base_family = "Lato") +
                theme_map() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5, size = 12),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = ""))

p1

ggsave('figs/mpas_fishery.png', dpi = 300, height = 8, width = 8)



p2

ggsave('figs/mpas_buceo.png', dpi = 300, height = 8, width = 8)




## Map of conflict areas Diving Fisheries

(p1 <- final_mpa %>% 
        filter(Pesca == "Permitida", Buceo == "Permitido") %>% 
        ggplot() +
        geom_sf(data = spdf_mx, 
                fill = "gray90", 
                col = NA) +
        geom_sf(
                fill = "firebrick", 
                col = NA) +
        coord_sf(xlim = c(-118, -85), 
                 ylim = c(13, 32), 
                 expand = TRUE) +
        labs(subtitle = "Conflicto") +
        theme_bw(base_family = "Lato") +
        theme_map() +
        theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
              strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
              axis.title = element_blank(),
              strip.text.y = element_text(hjust = .5, size = 12),
              panel.grid.minor.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = NA), 
              legend.position = ""))


(p2 <- final_mpa %>% 
        filter(Pesca == "Prohibida", Buceo == "Permitido") %>% 
        ggplot() +
        geom_sf(data = spdf_mx, 
                fill = "gray90", 
                col = NA) +
        geom_sf(
                fill = "darkgreen", 
                col = NA) +
        coord_sf(xlim = c(-118, -85), 
                 ylim = c(13, 32), 
                 expand = TRUE) +
        labs(subtitle = "Sinergia") +
        theme_bw(base_family = "Lato") +
        theme_map() +
        theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
              strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
              axis.title = element_blank(),
              strip.text.y = element_text(hjust = .5, size = 12),
              panel.grid.minor.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = NA), 
              legend.position = ""))

p1+p2

ggsave('figs/mpas_buceo_vs_pesca.png', dpi = 300, height = 3, width = 8)



## Mexico stat by EEZ
EEZ <- 3269386

library(units)

(p1 <- final_mpa %>% 
        select(ID, Pesca) %>% 
        mutate(Area = set_units(st_area(.), km^2)) %>% 
        group_by(Pesca) %>% 
        summarise(Area = sum(Area)) %>%
        as.data.frame() %>% 
        mutate(Area_dbl = as.double(Area)) %>% 
        ggplot(aes(x = reorder(Pesca, Area_dbl), y = Area_dbl/1000, fill = Pesca)) +
        geom_col() +
        ylim(0, 600) +
        labs(x = "Pesca", y = "Area * 10^3 (km^2)") +
        theme_ipsum() +
        theme(legend.position = ""))

(p1 <- final_mpa %>% 
                select(ID, Pesca) %>% 
                mutate(Area = set_units(st_area(.), km^2)) %>% 
                group_by(Pesca) %>% 
                summarise(Area = sum(Area)) %>%
                as.data.frame() %>% 
                select(-geometry) %>% 
                mutate(Area_dbl = as.double(Area)) %>% 
                mutate(Area_perc_eez = (Area_dbl/EEZ)*100) %>%
                mutate(EEZ = rep("EEZ area", 4)) %>% 
                filter(Pesca != "No especificado") %>% 
                ungroup() %>% 
                mutate(tot_perc = sum(Area_perc_eez)) %>% 
                add_row(Pesca = "EEZ", Area = NA, Area_dbl = 3269386, Area_perc_eez = 100 - 21.78272, EEZ = "EEZ area", tot_perc = 100) %>% 
                ggplot(aes(x = EEZ, y = Area_perc_eez, fill = Pesca, label = paste(round(Area_perc_eez, 1), "%"))) +
                geom_col() +
                labs(subtitle = "Área totál en AMP: 21.7%") + 
                geom_text(position = position_stack(vjust = .5)) +
                scale_fill_manual(values = c("grey90", "gray50", "darkgreen", "pink"), name = " ",  labels = c("EEZ", "Pesca Permitida", "Pesca Prohibida", "Sin manejo")) +
                labs(x = "", y = "") +
                theme_ipsum() + 
                theme(panel.grid.minor = element_blank(),
                      panel.grid.major = element_blank(),
                      axis.title = element_blank(),
                      axis.text.x = element_blank(),
                      axis.text.y = element_blank(),
                      legend.position = "bottom"
                      )
        )

ggsave('figs/Total_protected_area.png', dpi = 300, height = 6, width = 4)

p2 <- final_mpa %>% 
        select(ANP, Pesca) %>% 
        mutate(Area = set_units(st_area(.), km^2)) %>% 
        group_by(ANP, Pesca) %>% 
        summarise(Area = round(sum(Area), 1)) %>%
        as.data.frame() %>% 
        select(-geometry) %>% 
        mutate(Area_dbl = as.double(Area)) %>% 
        mutate(Area_perc_eez = round((Area_dbl/EEZ)*100, 4)) %>%
        #mutate(EEZ = rep("EEZ area", 4)) %>% 
        filter(Pesca != "No especificado") %>% 
        arrange(-Area_perc_eez, Pesca) %>% 
        top_n(10) %>% 
        ggplot(aes(y = reorder(ANP,Area_perc_eez), x = Area_perc_eez, fill = Pesca)) +
        geom_col() +
        geom_text(aes(label = round(Area_perc_eez, 1)), vjust = .4, hjust = -.5, colour = "black")+
        scale_fill_manual(values = c("pink", "darkgreen", "gray50"), name = " ") +
        labs(y = "", x = "Porcentaje de EEZ", subtitle = "Contribuciones a la Protección") +
        xlim(0,18)+
        theme_ipsum() +
        theme(legend.position = "", 
              panel.grid.major.y = element_blank(), 
              axis.text.y = element_text(size=9))


p2+p1+
        plot_layout(guides = 'collect', widths = c(2, 1))

ggsave('figs/Total_protected_area_desglosado.png', dpi = 300, height = 5, width = 7)


## Mexican stat by area

(p1 <- final_mpa %>% 
        select(ID, ANP, Pesca) %>% 
        mutate(Area = set_units(st_area(.), km^2)) %>% 
        group_by(ANP, Pesca) %>% 
        summarise(Area = sum(Area)) %>% 
        as.data.frame() %>% 
        select(-geometry) %>% 
        mutate(Area_dbl = as.double(Area)) %>% 
        select(-Area) %>% 
        pivot_wider(ANP, names_from = "Pesca", values_from = "Area_dbl") %>% 
        replace(is.na(.), 0) %>% 
        select(ANP:Permitida) %>% 
        mutate(Razón = Prohibida/Permitida) %>% 
        arrange(-Razón) %>%
        top_n(10) %>% 
        ggplot(aes(y = reorder(ANP,Razón), x = Razón, fill = Razón)) +
        geom_col() +
        scale_fill_distiller(direction = 1, palette = "Spectral") +
        labs(y = "", x = "Prohibida/Permitida", subtitle = "Mejores 10") +
        theme_ipsum() +
        theme(legend.position = ""))

(p2 <- final_mpa %>% 
                select(ID, ANP, Pesca) %>% 
                mutate(Area = set_units(st_area(.), km^2)) %>% 
                group_by(ANP, Pesca) %>% 
                summarise(Area = sum(Area)) %>% 
                as.data.frame() %>% 
                select(-geometry) %>% 
                mutate(Area_dbl = as.double(Area)) %>% 
                select(-Area) %>% 
                pivot_wider(ANP, names_from = "Pesca", values_from = "Area_dbl") %>% 
                replace(is.na(.), 0) %>% 
                select(ANP:Permitida) %>% 
                mutate(Razón = Prohibida/Permitida) %>% 
                arrange(-Razón) %>%
                top_n(-10) %>% 
                ggplot(aes(y = reorder(ANP,Razón), x = Razón, fill = Razón)) +
                geom_col() +
                scale_fill_distiller(direction = 1, palette = "Spectral") +
                labs(y = "", x = "Prohibida/Permitida", subtitle = "Peores 10") +
                theme_ipsum() +
                theme(legend.position = ""))
p1/p2        

ggsave("figs/best_worst_ratio.png", dpi = 300, height = 10, width = 10)





amm_sites <- test %>% 
        select(ANP, Pesca) %>% 
        as.data.frame() %>% 
        select(-geometry) %>% 
        filter(Pesca == "Permitida") %>% 
        unique() %>% 
        select(ANP) %>% 
        as.vector()


EEZ <- 3269386

final_mpa %>% 
                filter(ANP %in% amm_sites$ANP) %>% 
                select(ID, ANP, Pesca) %>% 
                mutate(Area = set_units(st_area(.), km^2)) %>% 
                as.data.frame() %>% 
                select(-geometry) %>% 
                mutate(Area_dbl = as.double(Area)) %>% 
                select(-Area) %>% 
                group_by(Pesca) %>% 
                summarise(tot_area = sum(Area_dbl))
                
            