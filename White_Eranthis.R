# START -----------------------------------------------------------------------------------------------------------
Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(lubridate, purrr, dplyr, tidyr, forecast, zoo, rlang, ggplot2, tidyverse, raster,
               sp, geodata, terra, rasterVis, BiocManager, dismo, XML, jsonlite, rgdal, rJava,
               readxl, rgbif, factoextra, NbClust, cluster, caret, mice, missForest, knitr
)


# Read all lists in file
file_path <- "E:/Eranthis/White_E.xlsx"
White <- read_xlsx("White_E.xlsx")

# Create a distance matrix
dist_matrix <- dist(White)

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc, main = "Hierarchical Clustering Dendrogram", sub = NULL, xlab = NULL, cex = 0.8, labels = White$Species)
