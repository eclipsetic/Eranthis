Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(lubridate, purrr, dplyr, tidyr, forecast, zoo, rlang, ggplot2, tidyverse, raster,
               sp, geodata, terra, rasterVis, BiocManager, dismo, XML, jsonlite, rgdal, rJava,
               readxl, rgbif, factoextra, NbClust, cluster, openxlsx, caret, mice, missForest, knitr, htmltools,
               FactoMineR, missMDA, pcaMethods, caret, ggfortify, gridExtra, hrbrthemes, corrplot, mice,
               caTools, vegan, pvclust, openxlsx
)
#Empty Global Enviroment
rm(list = ls())

SP1 <- read_xlsx("E:/Eranthis/Species/SP2.xlsx")
SP2 <- read_xlsx("E:/Eranthis/Species/Species_list.xlsx")


matching_rows <- inner_join(SP1, SP2, by = c("Species" = "Species"))
write.xlsx(matching_rows, "matching_rows.xlsx", rowNames = FALSE)
