library(tidyverse)
library(BAS)
library(sf)
library(ggsflabel)
library(readxl)
library(units)
library(hrbrthemes)
source("00-all_data_wrangling.R")



### Associated biodiversity -------

sp_richness <- raster::raster("raster/sp_richness.tif")


test$richness <- raster::extract(sp_richness, test)


## Diving sites wrangling

diving_sites_to_merge <- test %>% 
        as.data.frame() %>% 
        select(id, region, locality, site_name, popularity, accessibil, ID, ANP_ID, ANP, protection_level, richness) %>% 
        mutate(popularity = as.numeric(popularity), 
               accessibility = as.numeric(accessibil)
        ) %>% 
        select(-accessibil) %>% 
        mutate(locality = str_replace(locality, "_", " "))


         


# Economic data -----------------------------------------------------------

surveyed <- read_xlsx('data/DATABASE_ECONOMIC_VALUE_SCUBA_PAPER.xlsx')


all <- read_xlsx('data/TOURISM_OPERATORS DB.xlsx') %>% 
        select(REGION, STATE, LOCALITY, OPERATOR_NAME, MPA, LAT, LONG)


economics <- surveyed %>% 
        group_by(REGION, COMPANY) %>% 
        summarise(CLIENTS = sum(CLIENTS_WEEK*4), N_BUZOS = sum(N_BUZOS_WEEK*4), N_SNORKEL = sum(N_SNORKEL_WEEK*4),
                  COST_DIVETRIP = mean(COST_DIVETRIP), COST_SNORKELTRIP = mean(COST_SNORKELTRIP)) %>% 
        rename(OPERATOR_NAME = COMPANY, CITY = REGION)




all$ID <- 1:length(all$OPERATOR_NAME)

matched_ID <- match(economics$OPERATOR_NAME, all$OPERATOR_NAME)


economics$ID <- matched_ID


economics <- economics %>% 
        filter(!is.na(ID)) %>% 
        select(ID, everything())


tourism_op <- merge(all, economics,by = c("ID", "OPERATOR_NAME"), all.x = T) %>% 
        group_by(LOCALITY) %>% 
        mutate(m_CLIENTS = mean(CLIENTS, na.rm = T), m_N_BUZOS = mean(N_BUZOS, na.rm = T), 
               m_N_SNORKEL = mean(N_SNORKEL, na.rm = T), m_COST_DIVE = mean(COST_DIVETRIP, na.rm = T), 
               m_COST_SNORKEL = mean(COST_SNORKELTRIP, na.rm = T)) %>%
        mutate(all_CLIENTS = ifelse(is.na(CLIENTS), m_CLIENTS, CLIENTS),
               all_N_BUZOS = ifelse(is.na(N_BUZOS), m_N_BUZOS, N_BUZOS), 
               all_N_SNORKEL = ifelse(is.na(N_SNORKEL), m_N_SNORKEL, N_SNORKEL),
               all_COST_DIVE = ifelse(is.na(COST_DIVETRIP), m_COST_DIVE, COST_DIVETRIP), 
               all_COST_SNORKEL = ifelse(is.na(COST_SNORKELTRIP), m_COST_SNORKEL, COST_SNORKELTRIP)) %>%
        select(OPERATOR_NAME:CITY, all_CLIENTS:all_COST_SNORKEL) %>% 
        mutate(REV_DIVING = all_N_BUZOS * all_COST_DIVE, REV_SNORKEL = all_N_SNORKEL * all_COST_SNORKEL) %>% 
        mutate(TOTAL_REV = REV_DIVING + REV_SNORKEL) 




tourism_to_merge <- tourism_op %>% 
        select(OPERATOR_NAME, LOCALITY, all_CLIENTS, TOTAL_REV, MPA) %>% 
        janitor::clean_names()




merged_test <- merge(diving_sites_to_merge, tourism_to_merge, by = "locality")

unique(merged_test$locality)


diving_data <- merged_test %>% 
        mutate(region_c = as.numeric(factor(region)),
               protection_level_c = as.numeric(factor(protection_level)),
               ) %>% 
        select(region_c, protection_level_c, popularity,
               total_rev) %>% 
        filter(!is.na(total_rev))




popularity_bas <- bas.lm(total_rev ~ .,
                         data = diving_data,
                         method = "MCMC+BAS", 
                         prior = "ZS-null", 
                         modelprior = uniform())
popularity_bas
summary(popularity_bas)

coef_popularity <- coef(popularity_bas)

#plot(coef_popularity)
# 
confint(coef_popularity)
plot(popularity_bas)




#validation <- sample_n(diving_data, 50)
validation <- merged_test %>% 
        mutate(region_c = as.numeric(factor(region)),
               protection_level_c = as.numeric(factor(protection_level)),
        ) %>% 
        mutate(protection_level_c = ifelse(protection_level_c == 3, 3, 3)) %>% 
        filter(!is.na(total_rev))

predict_1 <- predict(popularity_bas, validation, estimator = "BMA", interval = "predict", se.fit = T)
merged_test_complete <- merged_test[complete.cases(merged_test), ]

results <- data.frame(
        "ANP_ID" = validation$ANP_ID,
        "Region" = validation$region,
        "Locality" = validation$locality,
        "Protection_level" = validation$protection_level,
        "ANP" = validation$ANP,
        "Estimated revenue" = predict_1$Ybma,
        "Real revenue" = validation$total_rev,
        "Predicted_increase" = predict_1$Ybma - validation$total_rev
)


predicted_increase <- results %>% 
        group_by(Region, Locality, Protection_level, ANP, ANP_ID) %>% 
        summarise(Predicted_increase = mean(Predicted_increase, na.rm = T))



metadata <- validation %>% 
        group_by(region,ANP_ID, locality, protection_level, ANP) %>% 
        summarise(n_sitios = n_distinct(id),
                  n_operators = n_distinct(operator_name), 
                  mean_rev = mean(total_rev))


names(metadata) <- c("Region", "ANP_ID", "Locality", "Protection_level", "ANP", "n_sitios", "n_operators", "mean_rev")


amp_metadata <- final_mpa %>% 
        select(ANP_ID, Pesca) %>% 
        mutate(Area = set_units(st_area(.), km^2)) %>% 
        group_by(ANP_ID, Pesca) %>% 
        summarise(Area = sum(Area)) %>% 
        as.data.frame() %>% 
        select(-geometry) %>% 
        mutate(Area_dbl = round(as.double(Area),1)) %>% 
        select(-Area) 

full_metadata <- merge(metadata, amp_metadata, by = "ANP_ID", all.x = T)



full_results <- merge(predicted_increase, full_metadata) %>% 
        select(Region, Locality, ANP_ID, ANP, Protection_level, Area_dbl, n_sitios, n_operators, mean_rev, Predicted_increase)



summary_table <- full_results %>% 
        group_by(Region, Locality, ANP_ID, ANP, Protection_level) %>% 
        summarise(n_sitios = mean(n_sitios),
               n_operators = mean(n_operators),
               mean_rev = mean(mean_rev),
               Predicted_increase = mean(Predicted_increase),
               Area = sum(Area_dbl)) %>% 
        mutate(total_rev = mean_rev * n_operators,
               predicted_revenues = (mean_rev+Predicted_increase)*n_operators,
               value_site = total_rev / n_sitios,
               value_site_predicted = predicted_revenues / n_sitios,
               value_operator = mean_rev,
               value_operator_predicted = Predicted_increase,
               value_area = total_rev / Area,
               value_area_predicted = predicted_revenues / Area) %>% 
        select(Region:Protection_level, Area:value_area_predicted)
        
summary_table %>% 
        group_by(ANP) %>% 
        summarise(current_value = sum(total_rev),
                  predicted_value = sum(predicted_revenues)) %>% 
        ungroup() %>% 
        summarise(total_current = sum(current_value), total_predicted = sum(predicted_value))












# Data muchning -----------------------------------------------------------


summary_table <- read.csv("data/summary_table.csv")


(p1 <- summary_table %>% 
        filter(!ANP %in% c("Balandra", "Isla San Pedro Mártir", "Isla Guadalupe")) %>% 
        group_by(ANP) %>% 
        summarise(total_area = sum(Area), 
                  current_value = sum(total_rev),
                  predicted_value = sum(predicted_revenues)) %>% 
        mutate(total_area = replace_na(total_area,  9546.501)) %>% 
        ggplot(aes(x = reorder(ANP, current_value), y = current_value, fill = current_value)) +
        geom_col() +
        geom_text(aes(label = paste0(round(current_value/1000000, 1), "MDD"), vjust = .4, hjust = -.1, colour = "black")) +
        coord_flip() +
        labs(x = "AMP", y = "Ganancias totales del area en USD") +
        scale_fill_distiller(direction = 1, palette = "Spectral") +
        scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
        ylim(0, 120000000)+
        theme_ipsum() +
        theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
              strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
              axis.title = element_blank(),
              strip.text.y = element_text(hjust = .5),
              panel.grid.minor.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = NA), 
              legend.position = ""))

(p2 <- summary_table %>% 
        filter(!ANP %in% c("Balandra", "Isla San Pedro Mártir", "Isla Guadalupe")) %>% 
        group_by(ANP) %>% 
        summarise(total_area = sum(Area), 
                  current_value = sum(total_rev),
                  predicted_value = sum(predicted_revenues)) %>% 
        mutate(total_area = replace_na(total_area,  9546.501)) %>% 
        mutate(value_area = current_value / total_area) %>% 
        ggplot(aes(x = reorder(ANP, value_area), y = value_area, fill = value_area)) +
        geom_col() +
        geom_text(aes(label = paste0(round(value_area/1000, 1), "md"), vjust = .4, hjust = -.1, colour = "black")) +
        coord_flip() +
        labs(x = "AMP", y = "Valor del area en USD") +
        scale_fill_distiller(direction = 1, palette = "Spectral") +
        scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
        ylim(0, 1000000)+
        theme_ipsum() +
        theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
              strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
              axis.title = element_blank(),
              strip.text.y = element_text(hjust = .5),
              panel.grid.minor.x = element_line(color = NA),
              panel.grid.major.y = element_line(color = NA), 
              legend.position = ""))


p1/p2

ggsave("figs/current_value.png", dpi = 300, height = 8, width = 8)

(p1 <- summary_table %>% 
                filter(!ANP %in% c("Balandra", "Isla San Pedro Mártir", "Isla Guadalupe")) %>% 
                group_by(ANP) %>% 
                summarise(total_area = sum(Area), 
                          current_value = sum(total_rev),
                          predicted_value = sum(predicted_revenues)) %>% 
                mutate(total_area = replace_na(total_area,  9546.501)) %>% 
                mutate(value_area = predicted_value / total_area) %>% 
                ggplot(aes(x = reorder(ANP, predicted_value), y = predicted_value, fill = predicted_value)) +
                geom_col() +
                geom_text(aes(label = paste0(round(predicted_value/1000000, 1), "MDD"), vjust = .4, hjust = -.1, colour = "black")) +
                coord_flip() +
                labs(x = "AMP", y = "Valor modelado en USD") +
                scale_fill_distiller(direction = 1, palette = "Spectral") +
                scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
                ylim(0, 120000000) +
                theme_ipsum() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = ""))

(p2 <- summary_table %>% 
                filter(!ANP %in% c("Balandra", "Isla San Pedro Mártir", "Isla Guadalupe")) %>% 
                group_by(ANP) %>% 
                summarise(total_area = sum(Area), 
                          current_value = sum(total_rev),
                          predicted_value = sum(predicted_revenues)) %>% 
                mutate(total_area = replace_na(total_area,  9546.501)) %>% 
                mutate(value_area = predicted_value / total_area) %>% 
                ggplot(aes(x=reorder(ANP, value_area), y = value_area, fill = value_area)) +
                geom_col() +
                geom_text(aes(label = paste0(round(value_area/1000, 1), "md"), vjust = .4, hjust = -.1, colour = "black")) +
                coord_flip() +
                labs(x = "AMP", y = "Valor por area modelado en USD") +
                scale_fill_distiller(direction = 1, palette = "Spectral") +
                scale_y_continuous(labels = function(x) format(x, scientific = TRUE)) +
                ylim(0, 1000000)+
                theme_ipsum() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = ""))

p1/p2

ggsave("figs/bayesian_model2.png", dpi = 300, height = 8, width = 8)




(p1 <- summary_table %>% 
                filter(!ANP %in% c("Balandra", "Isla San Pedro Mártir", "Isla Guadalupe")) %>% 
                group_by(ANP) %>% 
                summarise(total_area = sum(Area), 
                          current_value = sum(total_rev),
                          predicted_value = sum(predicted_revenues)) %>% 
                mutate(total_area = replace_na(total_area,  9546.501)) %>% 
                mutate(perc_incr = ((predicted_value - current_value)/predicted_value)*100) %>% 
                ggplot(aes(x=reorder(ANP, perc_incr), y = perc_incr, fill = perc_incr)) +
                geom_col(position = position_dodge2()) +
                geom_text(aes(label = round(perc_incr, 1)), vjust = .4, hjust = -.1, colour = "black") +
                coord_flip() +
                labs(x = "AMP", y = "Porcentaje de incremento de ganancias") +
                scale_fill_distiller(direction = 1, palette = "Spectral") +
                #ylim(0, 120000000)+
                theme_ipsum() +
                theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
                      strip.background = element_rect(color = NA, fill = "white"),  # Make facet label background white.
                      axis.title = element_blank(),
                      strip.text.y = element_text(hjust = .5),
                      panel.grid.minor.x = element_line(color = NA),
                      panel.grid.major.y = element_line(color = NA), 
                      legend.position = ""))
### HOW much area/?


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

suggested <- st_read('shp/Suggested_No_Take.shp')

suggested %>% 
        mutate(Area = set_units(st_area(.), km^2)) %>% 
        select(id, Area) %>% 
        mutate(Area_dbl = as.double(Area)) %>% 
        mutate(rel_eez = (Area_dbl/EEZ)*100)



upgraded <- 221+46.5+91214
new <- 9546.501

(upgraded/EEZ)*100
(new/EEZ)*100
(sum(upgraded, new)/EEZ)*100



