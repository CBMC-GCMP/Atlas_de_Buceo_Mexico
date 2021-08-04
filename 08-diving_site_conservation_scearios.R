library(tidyverse)
library(fastDummies)


source('07-wrangling_diving_sites.R')
st_as_xyz = function(x) data.frame(st_coordinates(x), st_set_geometry(x, NULL))

write.csv((dive_sites), 'diving_sites_by_protection.csv', row.names = F)

toclust <- as.data.frame(dive_sites) %>% 
        select(categoria, zonificacion) %>% 
        replace(is.na(.), "no determinado") %>% 
        mutate_if(is.character,as.factor)

test_dumm <- as.data.frame(dive_sites) %>% 
        select(nombre_sitio, categoria, zonificacion, subzonificacion) %>% 
        dummy_cols(select_columns = c("categoria", "zonificacion", "subzonificacion"))

library(cluster)

gower.dist <- daisy(toclust, metric = c("gower"))

# class(gower.dist) 
## dissimilarity , dist

divisive.clust <- diana(as.matrix(gower.dist), 
                        diss = TRUE, keep.diss = TRUE)
plot(divisive.clust, main = "Divisive")




# complete
aggl.clust.c <- hclust(gower.dist, method = "complete")
plot(aggl.clust.c,
     main = "Agglomerative, complete linkages")
