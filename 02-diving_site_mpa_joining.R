library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggthemes)
library(hrbrthemes)
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
        mutate(protection_level = ifelse(is.na(ANP), "Sin Protección", "Into MPA"), 
               core_zone = ifelse(Pesca == "Prohibida", "Pesca Prohibida", "Amortiguamiento"),
               protection_level = ifelse(is.na(core_zone), "Sin Protección", core_zone)) %>% 
        select(-core_zone) %>% 
        mutate(protection_level = factor(protection_level, levels = c("Sin Protección", "Amortiguamiento", "Pesca Prohibida"))) 

View(test)
ggplot(test) +
                 geom_sf(data = spdf_mx, 
                         fill = "gray90", 
                         col = NA) +
                 geom_sf(data = final_mpa, 
                         col = "#f03b20") +
                 geom_sf(data = test,
                         aes(fill = protection_level), 
                         alpha = .5, 
                         pch = 21) +
                 scale_color_brewer(palette = "Set1") +
                 coord_sf(xlim = c(-118, -85), 
                          ylim = c(13, 32), 
                          expand = TRUE) +
                 theme_bw(base_family = "Lato") +
                 guides(color = guide_legend(nrow = 3,
                                             byrow = TRUE, 
                                             override.aes = list(size=5))) +
                 
                 facet_grid(protection_level~.,
                            switch = "y") +
                 theme_map() +
                 theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                       strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                       axis.title = element_blank(),
                       strip.text.y = element_text(hjust = .5, size = 12),
                       panel.grid.minor.x = element_line(color = NA),
                       panel.grid.major.y = element_line(color = NA), 
                       legend.position = "")


ggsave('figs/map_sites.png', dpi = 300, width = 10, height = 10)



test %>% 
        group_by(protection_level) %>% 
        summarise(N = n_distinct(id))

test %>% 
        as.data.frame() %>% 
        select(-geometry) %>% 
        group_by(region, protection_level) %>% 
        summarise(N = n_distinct(id)) %>% 
        pivot_wider(region, names_from = "protection_level", values_from = "N") %>% 
        mutate(total_sites = sum(`Sin Protección` + Amortiguamiento + `Pesca Prohibida`)) %>% 
        mutate(anp_notake = (`Pesca Prohibida`/(Amortiguamiento + `Pesca Prohibida`))*100)

test %>% 
        as.data.frame() %>% 
        select(region, site_name, popularity, protection_level) %>% 
        mutate(popularity = as.numeric(popularity)) %>% 
        group_by(region, protection_level) %>% 
        summarise(n = n(), mean_pop = mean(popularity, na.rm = T)) %>% 
        arrange(region, -mean_pop) %>% 
        ungroup() %>% 
        mutate(pop_scaled = as.vector(scales::rescale(mean_pop, to = c(0, 1), from = range(mean_pop, na.rm = TRUE, finite = TRUE)))) %>% 
        ggplot(aes(x=protection_level, y = pop_scaled,  fill = protection_level)) +
        geom_col() +
        facet_grid(~region, scales = "free_x", switch = "x") +
        scale_fill_manual(values = c("#91091e", "#fb743e", "#6ddccf"), name = "Nivel de Protección") +
        #geom_jitter(aes(group = protection_level), col = "gray90") + 
        theme_ipsum() +
        labs(x = "", y = "Indice de popularidad de los sitios") +
        theme(strip.text.x = element_text(angle = 0, hjust = .5),
              axis.text.x = element_blank(), 
              legend.position = "bottom")

ggsave("figs/site_popularity_by_protection_region.png", dpi = 300, height = 5, width = 7)


test %>% 
        as.data.frame() %>% 
        select(region, site_name, popularity, protection_level) %>% 
        mutate(popularity = as.numeric(popularity)) %>% 
        ggplot(aes(x=protection_level, y = log1p(popularity),  fill = protection_level)) +
        geom_violin(trim=FALSE) +
        facet_grid(~region, scales = "free_x", switch = "x") +
        #geom_jitter(aes(group = protection_level), col = "gray90") +
        geom_boxplot(width = 0.1, fill = "white") +
        theme_ipsum() +
        theme(strip.text.x = element_text(angle = 0, hjust = .5),
              axis.text.x = element_blank())


## Sitios en AMP sin Protección

(p1 <- test %>% 
        filter(region == "Gulf of Mexico") %>%         
        ggplot() + 
        geom_sf(data = spdf_mx, 
                fill = "gray90", 
                col = NA) +
        geom_sf(data = final_mpa, 
                col = "#f03b20") +
        geom_sf(
                aes(fill = protection_level), 
                alpha = .5, 
                pch = 21) +
        scale_color_brewer(palette = "Set1") +
        coord_sf(xlim = c(-99, -93), 
                  ylim = c(18.2, 23), 
                  expand = TRUE) +
        theme_bw(base_family = "Lato") +
        guides(color = guide_legend(nrow = 3,
                                    byrow = TRUE, 
                                    override.aes = list(size = 5))) +
        
        facet_grid(region~protection_level,
                   switch = "y") +
        theme_map() +
        theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
              strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
              axis.title = element_blank(),
              strip.text.y = element_text(hjust = .5, size = 12),
              panel.grid.minor.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = NA), 
              legend.position = ""))



(p2 <- test %>% 
                filter(region == "Yucatan Peninsula") %>%         
                ggplot() + 
                geom_sf(data = spdf_mx, 
                        fill = "gray90", 
                        col = NA) +
                geom_sf(data = final_mpa, 
                        col = "#f03b20") +
                geom_sf(
                        aes(fill = protection_level), 
                        alpha = .5, 
                        pch = 21) +
                scale_color_brewer(palette = "Set1") +
                coord_sf(xlim = c(-90, -85), 
                         ylim = c(15, 23), 
                         expand = TRUE) +
                theme_bw(base_family = "Lato") +
                guides(color = guide_legend(nrow = 3,
                                            byrow = TRUE, 
                                            override.aes = list(size = 5))) +
                
                facet_grid(region~protection_level,
                           switch = "y") +
                theme_map() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5, size = 12),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = ""))


(p3 <- test %>% 
                filter(region == "Mexican Pacific") %>%         
                ggplot() + 
                geom_sf(data = spdf_mx, 
                        fill = "gray90", 
                        col = NA) +
                geom_sf(data = final_mpa, 
                        col = "#f03b20") +
                geom_sf(
                        aes(fill = protection_level), 
                        alpha = .5, 
                        pch = 21) +
                scale_color_brewer(palette = "Set1") +
                coord_sf(xlim = c(-115, -99), 
                         ylim = c(13, 22), 
                         expand = TRUE) +
                theme_bw(base_family = "Lato") +
                guides(color = guide_legend(nrow = 3,
                                            byrow = TRUE, 
                                            override.aes = list(size = 5))) +
                
                facet_grid(region~protection_level,
                           switch = "y") +
                theme_map() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5, size = 12),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = ""))


(p4 <- test %>% 
                filter(region == "Northwest Mexico") %>%         
                ggplot() + 
                geom_sf(data = spdf_mx, 
                        fill = "gray90", 
                        col = NA) +
                geom_sf(data = final_mpa, 
                        col = "#f03b20") +
                geom_sf(
                        aes(fill = protection_level), 
                        alpha = .5, 
                        pch = 21) +
                scale_color_brewer(palette = "Set1") +
                coord_sf(xlim = c(-118, -100), 
                         ylim = c(21, 32), 
                         expand = TRUE) +
                theme_bw(base_family = "Lato") +
                guides(color = guide_legend(nrow = 3,
                                            byrow = TRUE, 
                                            override.aes = list(size = 5))) +
                
                facet_grid(region~protection_level,
                           switch = "y") +
                theme_map() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5, size = 12),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = ""))




test %>% 
        select(region, site_name, PM) %>% 
        filter(PM == "NO") %>%
        as.data.frame() %>% 
        group_by(region) %>%
        count()

test %>% 
                select(region, site_name, PM) %>% 
                filter(PM == "NO") %>%    
                ggplot() + 
                geom_sf(data = spdf_mx, 
                        fill = "gray90", 
                        col = NA) +
                geom_sf(data = final_mpa, 
                        col = "#f03b20") +
                geom_sf(
                        fill = "black", 
                        alpha = .5, 
                        pch = 21) +
                scale_color_brewer(palette = "Set1") +
                coord_sf(xlim = c(-118, -100), 
                         ylim = c(21, 32), 
                         expand = TRUE) +
                theme_bw(base_family = "Lato") +
                guides(color = guide_legend(nrow = 3,
                                            byrow = TRUE, 
                                            override.aes = list(size = 5))) +
                
                theme_map() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5, size = 12),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = "")

ggsave("figs/sites_no_PM.png", dpi = 300, height = 10, width = 8)


test %>% 
        select(ANP, region, site_name, Buceo) %>% 
        filter(Buceo == "No especificado") %>%
        as.data.frame() %>% 
        group_by(ANP, region) %>%
        count()

test %>% 
        select(region, site_name, PM) %>% 
        filter(PM == "NO") %>%    
        ggplot() + 
        geom_sf(data = spdf_mx, 
                fill = "gray90", 
                col = NA) +
        geom_sf(data = final_mpa, 
                col = "#f03b20") +
        geom_sf(
                fill = "black", 
                alpha = .5, 
                pch = 21) +
        scale_color_brewer(palette = "Set1") +
        coord_sf(xlim = c(-118, -100), 
                 ylim = c(21, 32), 
                 expand = TRUE) +
        theme_bw(base_family = "Lato") +
        guides(color = guide_legend(nrow = 3,
                                    byrow = TRUE, 
                                    override.aes = list(size = 5))) +
        
        theme_map() +
        theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
              strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
              axis.title = element_blank(),
              strip.text.y = element_text(hjust = .5, size = 12),
              panel.grid.minor.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = NA), 
              legend.position = "")

ggsave("figs/sites_no_PM.png", dpi = 300, height = 10, width = 8)





## NON PROTECTED SITES WITH HIGH ACCESSIBILITY AND HIGH POPULARITY

test %>% 
        filter(protection_level == "Sin Protección") %>% 
        mutate(popularity = as.numeric(popularity), 
               accessibility = as.numeric(ocean_acce)) %>% 
        select(region, site_name, latitude, longitude, popularity, accessibility) %>% 
        arrange(-popularity, -accessibility) %>% 
        mutate(pop_scaled = as.vector(scales::rescale(popularity, to = c(0, 1), from = range(popularity, na.rm = TRUE, finite = TRUE))),
               acc_scaled = as.vector(scales::rescale(accessibility, to = c(0, 1), from = range(accessibility, na.rm = TRUE, finite = TRUE)))) %>% 
        ggplot(aes(x = pop_scaled, y=acc_scaled, label = site_name)) +
        geom_point() +
        geom_text()

test %>% 
        filter(protection_level == "Sin Protección") %>% 
        mutate(popularity = as.numeric(popularity), 
               accessibility = as.numeric(ocean_acce)) %>% 
        select(region, site_name, latitude, longitude, popularity, accessibility) %>% 
        arrange(-popularity, -accessibility) %>% 
        mutate(pop_scaled = as.vector(scales::rescale(popularity, to = c(0, 1), from = range(popularity, na.rm = TRUE, finite = TRUE))),
               acc_scaled = as.vector(scales::rescale(accessibility, to = c(0, 1), from = range(accessibility, na.rm = TRUE, finite = TRUE)))) %>% 
        ggplot(aes(x = pop_scaled)) +
        geom_density() 

test %>% 
        filter(protection_level == "Sin Protección") %>% 
        mutate(popularity = as.numeric(popularity), 
               accessibility = as.numeric(ocean_acce)) %>% 
        select(region, site_name, latitude, longitude, popularity, accessibility) %>% 
        arrange(-popularity, -accessibility) %>% 
        mutate(pop_scaled = as.vector(scales::rescale(popularity, to = c(0, 1), from = range(popularity, na.rm = TRUE, finite = TRUE))),
               acc_scaled = as.vector(scales::rescale(accessibility, to = c(0, 1), from = range(accessibility, na.rm = TRUE, finite = TRUE)))) %>%  
        filter(pop_scaled > .15) %>% 
        as.data.frame() %>% 
        group_by(region) %>% 
        count()
        
test %>% 
        filter(protection_level == "Sin Protección") %>% 
        mutate(popularity = as.numeric(popularity), 
               accessibility = as.numeric(ocean_acce)) %>% 
        select(region, site_name, latitude, longitude, popularity, accessibility) %>% 
        arrange(-popularity, -accessibility) %>% 
        mutate(pop_scaled = as.vector(scales::rescale(popularity, to = c(0, 1), from = range(popularity, na.rm = TRUE, finite = TRUE))),
               acc_scaled = as.vector(scales::rescale(accessibility, to = c(0, 1), from = range(accessibility, na.rm = TRUE, finite = TRUE)))) %>%  
        filter(pop_scaled > .15) %>% 
        ggplot() + 
        geom_sf(data = spdf_mx, 
                fill = "gray90", 
                col = NA) +
        geom_sf(data = final_mpa, 
                col = "#f03b20") +
        geom_sf(
                fill = "black", 
                alpha = .5, 
                pch = 21) +
        scale_color_brewer(palette = "Set1") +
        # coord_sf(xlim = c(-118, -100), 
        #          ylim = c(21, 32), 
        #          expand = TRUE) +
        theme_bw(base_family = "Lato") +
        guides(color = guide_legend(nrow = 3,
                                    byrow = TRUE, 
                                    override.aes = list(size = 5))) +
        
        theme_map() +
        theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
              strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
              axis.title = element_blank(),
              strip.text.y = element_text(hjust = .5, size = 12),
              panel.grid.minor.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = NA), 
              legend.position = "")

        
ggsave('figs/sites_not_protected_with_high_popularity.png', dpi = 300, height = 5, width = 5)

test %>% 
        filter(protection_level == "Amortiguamiento") %>% 
        mutate(popularity = as.numeric(popularity), 
               accessibility = as.numeric(ocean_acce)) %>% 
        select(region, site_name, latitude, longitude, popularity, accessibility) %>% 
        arrange(-popularity, -accessibility) %>% 
        mutate(pop_scaled = as.vector(scales::rescale(popularity, to = c(0, 1), from = range(popularity, na.rm = TRUE, finite = TRUE))),
               acc_scaled = as.vector(scales::rescale(accessibility, to = c(0, 1), from = range(accessibility, na.rm = TRUE, finite = TRUE)))) %>%  
        filter(pop_scaled > .15) %>% 
        as.data.frame() %>% 
        group_by(region) %>% 
        count()

test %>% 
        filter(protection_level == "Amortiguamiento") %>% 
        mutate(popularity = as.numeric(popularity), 
                accessibility = as.numeric(ocean_acce)) %>% 
        select(region, site_name, latitude, longitude, popularity, accessibility) %>% 
        arrange(-popularity, -accessibility) %>% 
        mutate(pop_scaled = as.vector(scales::rescale(popularity, to = c(0, 1), from = range(popularity, na.rm = TRUE, finite = TRUE))),
               acc_scaled = as.vector(scales::rescale(accessibility, to = c(0, 1), from = range(accessibility, na.rm = TRUE, finite = TRUE)))) %>%  
        filter(pop_scaled > .15) %>% 
        ggplot(aes(x=pop_scaled, y=log1p(accessibility), label = site_name)) +
        geom_point() +
        geom_text()

test %>%
        filter(protection_level == "Amortiguamiento") %>% 
        mutate(popularity = as.numeric(popularity), 
               accessibility = as.numeric(ocean_acce)) %>% 
        select(region, site_name, latitude, longitude, popularity, accessibility) %>% 
        arrange(-popularity, -accessibility) %>% 
        mutate(pop_scaled = as.vector(scales::rescale(popularity, to = c(0, 1), from = range(popularity, na.rm = TRUE, finite = TRUE))),
               acc_scaled = as.vector(scales::rescale(accessibility, to = c(0, 1), from = range(accessibility, na.rm = TRUE, finite = TRUE)))) %>%  
        filter(pop_scaled > .15) %>% 
        ggplot() + 
        geom_sf(data = spdf_mx, 
                fill = "gray90", 
                col = NA) +
        geom_sf(data = final_mpa, 
                col = "#f03b20") +
        geom_sf(
                fill = "black", 
                alpha = .5, 
                pch = 21) +
        scale_color_brewer(palette = "Set1") +
        # coord_sf(xlim = c(-118, -100), 
        #          ylim = c(21, 32), 
        #          expand = TRUE) +
        theme_bw(base_family = "Lato") +
        guides(color = guide_legend(nrow = 3,
                                    byrow = TRUE, 
                                    override.aes = list(size = 5))) +
        
        theme_map() +
        theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
              strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
              axis.title = element_blank(),
              strip.text.y = element_text(hjust = .5, size = 12),
              panel.grid.minor.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = NA), 
              legend.position = "")


ggsave('figs/sites_in_MPA_with_high_popularity.png', dpi = 300, height = 5, width = 5)   







#### Conflict and Sinergy

## MPA stats

final_mpa %>% 
        as.data.frame() %>% 
        select(ANP) %>% 
        count()

