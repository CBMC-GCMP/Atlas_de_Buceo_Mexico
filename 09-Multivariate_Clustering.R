library(tidyverse)
library(sf)
library(rnaturalearth)
library(ggthemes)
library(hrbrthemes)
library(patchwork)
library(fastDummies)

matrix <- read.csv('data/matrix_pca.csv')

regions <- matrix %>% 
        filter(!is.na(richness))

matrix2 <- matrix[, 3:8]

matrix2 <- matrix2 %>% 
        filter(!is.na(richness))

library(ggpubr)
library(factoextra)


# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(matrix2), 3, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster

fviz_cluster(res.km, data = matrix2,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



# Dimension reduction using PCA
res.pca <- prcomp(matrix2,  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)

ind.coord$region <- regions$region
# Data inspection
head(ind.coord)


# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)


ggscatter(
        ind.coord, x = "Dim.1", y = "Dim.2", 
        star.plot = TRUE,
        color = "cluster", 
        palette = "npg", 
        ellipse = F, 
        ellipse.type = "convex",
        shape = "region", 
        size = 1.5,  
        legend = "right", 
        ggtheme = theme_bw(),
        xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
        ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
        stat_mean(aes(color = cluster), size = 4)


fviz_pca_biplot(res.pca,
                col.var = "black",
                label = "var",
                habillage = ind.coord$cluster,
                palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                addEllipses = TRUE
) +
        stat_mean(aes(color = ind.coord$cluster), size = 4)






# Amortiguamento --------------------------------------------------------------

matrix2 <- matrix %>% 
        filter(AM == 1) %>% 
        select(-OA, -PP, -AM)

regions <- matrix2 %>% 
        filter(!is.na(richness))

matrix2 <- matrix2[, 3:5]

matrix2 <- matrix2 %>% 
        filter(!is.na(richness))


library(ggpubr)
library(factoextra)


# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(matrix2), 3, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster

fviz_cluster(res.km, data = matrix2,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



# Dimension reduction using PCA
res.pca <- prcomp(matrix2,  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)

ind.coord$region <- regions$region
# Data inspection
head(ind.coord)


# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)


ggscatter(
        ind.coord, x = "Dim.1", y = "Dim.2", 
        star.plot = TRUE,
        color = "cluster", 
        palette = "npg", 
        ellipse = F, 
        ellipse.type = "convex",
        shape = "region", 
        size = 1.5,  
        legend = "right", 
        ggtheme = theme_bw(),
        xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
        ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
        stat_mean(aes(color = cluster), size = 4)


fviz_pca_biplot(res.pca,
                col.var = "black",
                label = "var",
                habillage = ind.coord$cluster,
                palette = c("#00AFBB", "#E7B800", "#FC4E07")
) +
        stat_mean(aes(color = ind.coord$cluster), size = 4)

pca_matrix <- cbind(matrix2, res.pca$x)


pca_matrix %>% 
        ggplot(aes(x = PC1))+
        geom_density()




# Open area --------------------------------------------------------------

matrix2 <- matrix %>% 
        filter(OA == 1) %>% 
        select(-OA, -PP, -AM)

regions <- matrix2 %>% 
        filter(!is.na(richness))

matrix2 <- matrix2[, 3:5]

matrix2 <- matrix2 %>% 
        filter(!is.na(richness))


library(ggpubr)
library(factoextra)


# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(matrix2), 3, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster

fviz_cluster(res.km, data = matrix2,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



# Dimension reduction using PCA
res.pca <- prcomp(matrix2,  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)

ind.coord$region <- regions$region
# Data inspection
head(ind.coord)


# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)


ggscatter(
        ind.coord, x = "Dim.1", y = "Dim.2", 
        star.plot = TRUE,
        color = "cluster", 
        palette = "npg", 
        ellipse = F, 
        ellipse.type = "convex",
        shape = "region", 
        size = 1.5,  
        legend = "right", 
        ggtheme = theme_bw(),
        xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
        ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
        stat_mean(aes(color = cluster), size = 4)


fviz_pca_biplot(res.pca,
                col.var = "black",
                label = "var",
                habillage = ind.coord$cluster,
                palette = c("#00AFBB", "#E7B800", "#FC4E07")
)+
        stat_mean(aes(color = ind.coord$cluster), size = 4)







# Clustering with full results --------------------------------------------



full <- read.csv('data/full_results.csv')


regions <- matrix %>% 
        filter(!is.na(richness))

matrix2 <- matrix[, 3:8]

matrix2 <- matrix2 %>% 
        filter(!is.na(richness))

library(ggpubr)
library(factoextra)


# Compute k-means with k = 3
set.seed(123)
res.km <- kmeans(scale(matrix2), 3, nstart = 25)
# K-means clusters showing the group of each individuals
res.km$cluster

fviz_cluster(res.km, data = matrix2,
             palette = c("#2E9FDF", "#00AFBB", "#E7B800"), 
             geom = "point",
             ellipse.type = "convex", 
             ggtheme = theme_bw()
)



# Dimension reduction using PCA
res.pca <- prcomp(matrix2,  scale = TRUE)
# Coordinates of individuals
ind.coord <- as.data.frame(get_pca_ind(res.pca)$coord)
# Add clusters obtained using the K-means algorithm
ind.coord$cluster <- factor(res.km$cluster)

ind.coord$region <- regions$region
# Data inspection
head(ind.coord)


# Percentage of variance explained by dimensions
eigenvalue <- round(get_eigenvalue(res.pca), 1)
variance.percent <- eigenvalue$variance.percent
head(eigenvalue)


ggscatter(
        ind.coord, x = "Dim.1", y = "Dim.2", 
        star.plot = TRUE,
        color = "cluster", 
        palette = "npg", 
        ellipse = F, 
        ellipse.type = "convex",
        shape = "region", 
        size = 1.5,  
        legend = "right", 
        ggtheme = theme_bw(),
        xlab = paste0("Dim 1 (", variance.percent[1], "% )" ),
        ylab = paste0("Dim 2 (", variance.percent[2], "% )" )
) +
        stat_mean(aes(color = cluster), size = 4)


fviz_pca_biplot(res.pca,
                col.var = "black",
                label = "var",
                habillage = ind.coord$cluster,
                palette = c("#00AFBB", "#E7B800", "#FC4E07"), 
                addEllipses = TRUE
) +
        stat_mean(aes(color = ind.coord$cluster), size = 4)




