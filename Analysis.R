# START -----------------------------------------------------------------------------------------------------------
Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(lubridate, purrr, dplyr, tidyr, forecast, zoo, rlang, ggplot2, tidyverse, raster,
  sp, geodata, terra, rasterVis, BiocManager, dismo, XML, jsonlite, rgdal, rJava,
  readxl, rgbif, factoextra, NbClust, cluster, openxlsx, caret, mice, missForest, knitr, htmltools,
  FactoMineR, missMDA, pcaMethods, caret, ggfortify, gridExtra, hrbrthemes, corrplot, mice
)
#Empty Global Enviroment
rm(list = ls())

# Read all lists in file
file_path <- "E:/Eranthis/Eranthis_morph.xlsx"

all_sheets <- excel_sheets(file_path)

for (sheet_name in all_sheets) {
 var_name <- make.names(sheet_name)
 assign(var_name, read_excel(file_path, sheet = sheet_name))
}

# Downloading... --------------------------------------------------------------------------------------------------

dir.create("E:/Eranthis/maps")
bioclim <- getData(name="worldclim", download=FALSE, path = "E:/Eranthis/maps", res= 2.5, var="bio")


# DataFrame -------------------------------------------------------------------------------------------------------

#Spatial Point from GBIF
dir.create("E:/Eranthis/gbif")
species_list <- read_xlsx("E:/Eranthis/gbif/PLANT_LIST.xlsx")$species

occ_data_list <- list()

for (species_name in species_list) {
  print(paste("Retrieving data for:", species_name))
  current_occ_data <- gbif(genus = "Eranthis", species = species_name, ext = NULL, args = NULL, 
                           geo = TRUE, sp = TRUE, removeZeros = FALSE, download = TRUE)
  occ_data_list[[species_name]] <- current_occ_data
}


# Путь к папке, где будут сохранены карты
output_folder <- "E:/Eranthis/gbif/maps"
dir.create(output_folder, showWarnings = FALSE)

# Функция для получения данных GBIF для указанного вида
get_gbif_data <- function(genus, species) {
  gbif_data <- gbif(genus = genus, species = species, geo = TRUE)
  return(gbif_data)
}

# Цикл по каждому виду и создание карты
for (species_name in species_list) {
  print(paste("Retrieving data for:", species_name))
  
  # Получение данных GBIF
  gbif_data <- get_gbif_data(genus = "Eranthis", species = species_name)
  
  # Проверка наличия столбцов latitude и longitude
  if ("latitude" %in% colnames(gbif_data) && "longitude" %in% colnames(gbif_data)) {
    # Создание карты с точками
    map <- leaflet(data = gbif_data) %>%
      addTiles() %>%
      addMarkers(
        lng = ~longitude,
        lat = ~latitude,
        popup = ~paste("Genus: Eranthis", "<br>Species: ", species_name)
      )
    
    # Сохранение карты в файл
    map_file_path <- file.path(output_folder, paste0("map_", gsub(" ", "_", species_name), ".html"))
    saveWidget(map, map_file_path, selfcontained = TRUE)
    
    print(paste("Map saved for:", species_name))
  } else {
    print(paste("Error: latitude or longitude column not found for species", species_name))
  }
}


# Analysis --------------------------------------------------------------------------------------------------------

#Making one table from all

all_dataframes <- list("E.sibirica", "E.tanhoensis", "E.sibirica_x_E.tanhoensis", "E.krasnoborovii", "E.sineli",
                       "E.stellata", "E.stellata.Korea.", "E.pinnatifida", "E.pungdoensis", "E.albiflora",
                       "E.lobulata", "E.byunsanensis")


# Table with MICE ---------------------------------------------------------------------------------------------------
imputed_dataframes <- list()

for (df_name in all_dataframes) {
  df <- get(df_name)
  df <- df[, -c(1,3,45,46,47)]
  df[, 2:42] <- lapply(df[, 2:42], as.numeric)
  df$Species <- df_name
  imp <- mice(data = df, method = 'cart', m = 5, seed=500)
  completed_df <- complete(imp)
  imputed_dataframes[[df_name]] <- completed_df
}

combined_data <- do.call(rbind, imputed_dataframes)

#Making One table
tables <- lapply(all_dataframes, get)
tables <- lapply(tables, as.data.frame)
tables <- lapply(tables, function(tbl) mutate_all(tbl, as.character))

tables <- lapply(seq_along(all_dataframes), function(i) {
  df <- get(all_dataframes[[i]])
  df <- as.data.frame(mutate_all(df, as.character))
  df <- df[complete.cases(df$Species), ]
  df$Species <- all_dataframes[[i]]  # Здесь изменено
  return(df)
})

combined_data <- bind_rows(tables)
combined_data <- combined_data[, -c(1,3,45,46,47,48) ]
combined_data <- combined_data %>%
  mutate_at(vars(2:42), ~as.numeric(.))

imp <- mice(data = combined_data, method = 'rf', m = 1, seed=500)
combined_data <- complete(imp)
# MLR -------------------------------------------------------------------------------------------------------------

#1,3,15,17,19,23,29,30,31
model_fr <- combined_data[ , c(1,3,5,7,9,13,15,17,19,21,23,29,30,31)]
model_fr <- na.omit(model_fr)

model_fl <- combined_data[ , c(1,2,4,6,8,12,14,16,18,20,22,24,25,26,27,28)]
model_fl <- na.omit(model_fl)


model <- lm(Species ~ . - Species, data = model_fr)
hist(residuals(model), col = "steelblue")
plot(fitted(model), residuals(model))

summary(model)
# Create a distance matrix
dist_matrix <- dist(model_fr)

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc, main = "Hierarchical Clustering Dendrogram", sub = NULL, xlab = NULL, cex = 0.8, labels = model_fr$Species)


# PCA -------------------------------------------------------------------------------------------------------------


res.pca <- PCA(model_fr, quali.sup=c(1), graph =  FALSE)
fviz_pca_biplot(res.pca, label = "var", habillage = 1, addEllipses = TRUE, pointsize = 2,
                ellipse.level = 0.95, geom.ind = "point", ggtheme = theme_minimal()) +
  scale_color_brewer(palette = "Set1")


pca_res <- prcomp(model_fr[,2:11], scale. = TRUE)
autoplot(pca_res, data = model_fr, colour = 'Species',  loadings = TRUE, loadings.colour = 'blue',
         loadings.label = TRUE, loadings.label.size = 3, addEllipses=TRUE)


# Plotting --------------------------------------------------------------------------------------------------------
target_species <- c('E.sibirica','E.krasnoborovii')

# 'E.sibirica', 'E.tanhoensis', 'E.sibirica_x_E.tanhoensis'                                                                                       
# 'E.sibirica', 'E.tanhoensis', 'E.sibirica_x_E.tanhoensis', 'E.krasnoborovii', 'E.sineli'                                                         
# 'E.sineli', 'E.stellata', 'E.stellata.Korea.'                                                                                                 
# 'E.stellata', 'E.stellata.Korea.'                                                                                                            
# 'E.albiflora', 'E.lobulata'                                                                                                          
# 'E.pinnatifida', 'E.byunsanensis', 'E.pungdoensis'                                                                                    
# 'E.byunsanensis', 'E.pungdoensis'                                                                                                     
# 'E.albiflora', 'E.lobulata', 'E.sibirica', 'E.tanhoensis', 'E.stellata', 'E.pinnatifida', 'E.byunsanensis'

filtered_data <- model_fr[model_fr$Species %in% target_species, ]
res.pca <- PCA(filtered_data, quali.sup = 1, graph = FALSE, ncp = 2)
fviz_pca_biplot(res.pca, label = "var", habillage = 1, col.var = "black",
                addEllipses = TRUE, pointsize = 3, ellipse.level = 0.95,
                mean.point = FALSE, ellipse.alpha = 0, repel = TRUE) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

var <- get_pca_var(res.pca)
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$contrib, is.corr=FALSE)   
#Target species
plots <- list()

for (species_level in target_species) {
    filtered_data <- subset(model_fr, Species == species_level)
    if (nrow(filtered_data) > 0) {
    res.pca <- PCA(filtered_data, quali.sup = 1, graph = FALSE)
    plot <- fviz_pca_biplot(res.pca, label = "var",
                            addEllipses = TRUE, pointsize = 2, ellipse.level = 0.95, geom.ind = "point", 
                            pointshape = 16, alpha.ind = 0.5) +
      scale_color_brewer(palette = "Set1") +
      theme_minimal() +
      ggtitle(paste("Species:", species_level))
      plots[[length(plots) + 1]] <- plot
    }
}

grid.arrange(grobs = plots)



# E. pinnatifida --------------------------------------------------------------------------------------------------

E.pinnatifida$Species <- paste (E.pinnatifida$Species, E.pinnatifida$`Sample ID`, sep=" _ ")
E.pinnatifida_CD <- E.pinnatifida[, -c(1,3,34:47)]
imp <- mice(data = E.pinnatifida_CD, method = 'logreg.boot', m = 5, seed=500)
completed_df <- complete(imp)
res.pca <- PCA(completed_df, quali.sup = 1)
fviz_pca_biplot(res.pca, label = "var", habillage = 1, col.var = "black",
                addEllipses = TRUE, pointsize = 3, ellipse.level = 0.95,
                mean.point = FALSE, ellipse.alpha = 0, repel = TRUE) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()







