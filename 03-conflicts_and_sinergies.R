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










########## BY AREA SUBZONES --------
(p1 <- final_mpa %>% 
         mutate(Pesca = factor(Pesca, levels = c("No especificado", "Sin manejo", "Prohibida", "Permitida"))) %>% 
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
         scale_fill_manual(values = c( "gray30", "#fee391", "firebrick", "#005a32"), aesthetics = c("color", "fill"), labels = c("No especificado", "Prohibida", "Permitida", "Sin Manejo")) +
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
                as.data.frame() %>% 
                select(ANP, Pesca, Buceo) %>% 
                group_by(Pesca) %>% 
                count() %>% 
                mutate(Pesca = factor(Pesca, levels = c("No especificado", "Sin manejo", "Prohibida", "Permitida"))) %>% 
                ggplot(aes(x= reorder(Pesca, n), y = n,  fill = Pesca)) +
                geom_col() +
                scale_fill_manual(values = c("#fee391", "gray30", "firebrick", "#005a32"), name = "Nivel de Protección") +
                theme_ipsum() +
                labs(x = "", y = "Número de sitios en AMP") +
                theme(strip.text.x = element_text(angle = 0, hjust = .5),
                      axis.text.x = element_blank(),
                      axis.title.y=element_text(face="bold", vjust =.01, family="Times New Roman", size=18),
                      legend.position = "bottom",
                      legend.text=element_text(size=20, colour="black", family="Times New Roman"),
                      legend.title = element_text(face="bold", size = 25,family="Times New Roman"),
                      plot.background = element_rect(fill="#c4c4c4", colour="#c4c4c4")))


pesca <- final_mpa %>% 
        as.data.frame() %>%        
        select(ANP, Pesca, Buceo) %>% 
        group_by(Pesca) %>% 
        count() %>% 
        mutate(Pesca = factor(Pesca, levels = c("No especificado", "Sin manejo", "Prohibida", "Permitida"))) 


pesca$Fraction = pesca$n /sum(pesca$n)
pesca$ymax=cumsum(pesca$Fraction)
pesca$ymin=c(0, head(pesca$ymax, n=-1))
pesca$labelPosition <- (pesca$ymax + pesca$ymin)/2
pesca$label <- paste0(pesca$Pesca, "\n Sitios:", pesca$n)

ggplot(pesca, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Pesca)) +
        geom_rect()+
        ggrepel::geom_label_repel(x=4.5, aes(y=labelPosition, label=label, size=6, fill=NULL))+
        theme(rect= element_rect(fill="transparent", colour="transparent"))+
        scale_fill_manual(values = c("#edb72f", "gray30", "#c91818", "#07ab4b"), name = "Nivel de Protección") +
        theme_ipsum() +
        labs(x = "", y = "Numero de subzonas en AMP") +
        theme(strip.text.x = element_text(angle = 0, hjust = .5),
              axis.text.x = element_blank(), 
              legend.position = "bottom")+
        coord_polar(theta="y") +
        xlim(c(1, 4)) +
       theme_void() +
       theme(legend.position = "none",
             plot.background = element_rect(fill = "lightblue",
                                             ))+
        ggtitle("Número de Subzonas en AMP")+
        theme(plot.title=element_text(family = "Verdana", size=30, face="bold", hjust=-1),
              #legend.text=element_text(size=20, colour="black", family="Verdana"),
              #legend.title=element_text(size=24, colour="black", family="Verdana", face="bold")
              )
        



(p3 <- test %>% 
                select(ANP, Pesca, Buceo) %>% 
                as.data.frame() %>% 
                group_by(Pesca) %>% 
                count() %>% 
                filter(!is.na(Pesca)) %>% 
                mutate(Pesca = factor(Pesca, levels = c("No especificado", "Sin manejo", "Prohibida", "Permitida"))) %>% 
                ggplot(aes(x= reorder(Pesca, n), y = n,  fill = Pesca)) +
                geom_col() +
                scale_fill_manual(values = c( "#fee391", "gray30","firebrick", "#005a32"), name = "Nivel de Protección") +
                theme_ipsum() +
                labs(x = "", y = "Numero de sitios de buceo") +
                theme(strip.text.x = element_text(angle = 0, hjust = .5),
                      axis.text.x = element_blank(), 
                      legend.position = ""))
p1/(p2 + p3)

ggsave('figs/donde_se_permite_pesca.png', dpi = 300, height = 10, width = 8)

p1+(p3 / p2)

ggsave('figs/donde_se_permite_pesca2.png', dpi = 300, height = 8, width = 12)

(p1 <- final_mpa %>% 
                mutate(Buceo = factor(Buceo, levels = c("No especificado", "Sin manejo", "Prohibido", "Permitido"))) %>% 
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
                scale_fill_manual(values = c( "gray30", "#fee391", "firebrick", "#005a32"), aesthetics = c("color", "fill"), labels = c("No especificado", "Prohibido", "Permitido", "Sin Manejo")) +
                #scale_color_manual(values = c(, "#fc4e2a", "#005a32", "black"), labels = c("No especificado", "Prohibido", "Permitido", "Sin Manejo")) +
                labs(subtitle = "Buceo")+
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
                select(ANP, Buceo, Buceo) %>% 
                as.data.frame() %>% 
                group_by(Buceo) %>% 
                count() %>% 
                mutate(Buceo = factor(Buceo, levels = c("No especificado", "Sin manejo", "Prohibido", "Permitido"))) %>% 
                ggplot(aes(x= reorder(Buceo, n), y = n,  fill = Buceo)) +
                geom_col() +
                scale_fill_manual(values = c("#fee391", "gray30", "firebrick", "#005a32"), name = "Nivel de Protección") +
                theme_ipsum() +
                labs(x = "", y = "Numero de subzonas en AMP") +
                theme(strip.text.x = element_text(angle = 0, hjust = .5),
                      axis.text.x = element_blank(), 
                      legend.position = "bottom"))



(p3 <- test %>% 
                select(ANP, Buceo, Buceo) %>% 
                as.data.frame() %>% 
                group_by(Buceo) %>% 
                count() %>% 
                filter(!is.na(Buceo)) %>% 
                mutate(Buceo = factor(Buceo, levels = c("No especificado", "Sin manejo", "Prohibido", "Permitido"))) %>% 
                ggplot(aes(x= reorder(Buceo, n), y = n,  fill = Buceo)) +
                geom_col() +
                scale_fill_manual(values = c( "#fee391", "gray30","firebrick", "#005a32"), name = "Nivel de Protección") +
                theme_ipsum() +
                labs(x = "", y = "Numero de sitios de buceo") +
                theme(strip.text.x = element_text(angle = 0, hjust = .5),
                      axis.text.x = element_blank(), 
                      legend.position = ""))
p1/(p2 + p3)


ggsave("figs/donde_se_permite_buceo.png", dpi = 300, height = 10, width = 8)

p1+(p3 / p2)

ggsave('figs/donde_se_permite_buceo2.png', dpi = 300, height = 8, width = 12)


test %>% 
        select(ANP, site_name, Buceo) %>% 
        filter(Buceo == "Prohibido")





## Map of conflict areas Diving Fisheries

final_mpa %>% 
        filter(Pesca %in% c("Permitida", "Sin manejo", "No especificado"), Buceo %in% c("No especificado", "Sin manejo", "Permitido")) %>% 
        as.data.frame() %>% 
        group_by(Pesca, Buceo) %>% 
        count() %>% 
        ungroup() %>% 
        summarise(n=sum(n))

final_mpa %>% 
        filter(Pesca == "Prohibida", Buceo %in% c("No especificado", "Sin manejo", "Permitido")) %>% 
        as.data.frame() %>% 
        group_by(Pesca, Buceo) %>% 
        count() %>% 
        ungroup() %>% 
        summarise(n=sum(n))


(p1 <- data.frame(
        Class = c("Sitio", "Sitio"),
        Type = c("Conflicto", "Sinergia"),
        value = c(247, 100)) %>%
                ggplot(aes(x=Type, y = value, fill = Type))+
                geom_col(aes(width=0.9)) +
                coord_flip()+
                scale_fill_manual(values = c("firebrick", "#558019", "gray90"), aesthetics = c("color", "fill")) +
                theme_ipsum() +
                labs(x = "", y = "Número de sitios de buceo") +
                theme(strip.text.x = element_text(angle = 0, hjust = .5),
                      legend.position = "",
                      panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                      colour = "white"),
                     # legend.text=element_text(size=20, colour="black", family="Times New Roman"),
                      axis.title.y = element_text(
                              #face = "bold",
                              vjust = .5,
                              family = "Times New Roman",
                              size = 24),
                      axis.text.x = element_text(face="bold", family= "Times New Roman",
                                                 size=15),
                      axis.text.y = element_text(family= "Times New Roman",
                                                 size=15),
                      plot.background = element_rect(fill="#c4c4c4", colour="#c4c4c4")))

(p2 <- final_mpa %>% 
                filter(Pesca %in% c("Permitida", "Sin manejo", "No especificado"), Buceo %in% c("No especificado", "Sin manejo", "Permitido")) %>% 
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


(p3 <- final_mpa %>% 
                filter(Pesca == "Prohibida", Buceo %in% c("No especificado", "Sin manejo", "Permitido")) %>% 
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

(p2 + p3) / p1

ggsave('figs/mpas_buceo_vs_pesca.png', dpi = 300, height = 4, width = 5)





specie <- c(rep("Pesca", 4) , rep("Buceo" , 4))
condition <- rep(c("No Especificado" , "Sin Manejo" , "Prohibido", "Permitido"))

data <- data.frame(specie,condition)

data <- data %>% 
        mutate(value=c(17, 41, 172, 222, 80, 41, 105, 226)) %>% 
        mutate(condition= factor(condition, levels = c("No Especificado", "Sin Manejo", "Prohibido", "Permitido")))

specie <- rep("AMP's", 2)
condition <- c("Conflicto", "Sinergia")
value <- c(247, 100)

specie <- rep("AMP's",2)
type <- c("Conflicto","Sinergia")
value <- c(247,100)

con_sin <- data.frame() %>% 
        mutate(sepcie=)
# Stacked + percent
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
        geom_bar(position="fill", stat="identity", width = 0.5)+
        coord_flip()+
        scale_fill_manual(values = c("#edb72f", "gray30", "firebrick", "#558019"), name = "Nivel de Protección") +
        theme_ipsum() +
        labs(x = "", y = "Total de Sitios") +
        theme(strip.text.x = element_text(angle = 0, hjust = .5),
              legend.position = "bottom",
              panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                              colour = "white"),
              legend.text=element_text(size=20, colour="black", family="Times New Roman"),
              axis.title.y = element_text(
                      face = "bold",
                      hjust=0.5,
                      vjust = 0.5,
                      family = "Times New Roman",
                      size = 15),
              legend.title = element_text(face="bold", size = 25,family="Times New Roman"),
              axis.text.x = element_text(face="bold", family= "Times New Roman",
                                         size=15, color= "black"),
              axis.text.y = element_text(family= "Times New Roman", hjust=0.5, color="black",
                                         size=15),
              plot.background = element_rect(fill="#c4c4c4", colour="#c4c4c4"))







