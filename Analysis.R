# START -----------------------------------------------------------------------------------------------------------
Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(lubridate, purrr, dplyr, tidyr, forecast, zoo, rlang, ggplot2, tidyverse, raster,
  sp, geodata, terra, rasterVis, BiocManager, dismo, XML, jsonlite, rgdal, rJava,
  readxl, rgbif, factoextra, NbClust, cluster, openxlsx, caret, mice, missForest, knitr, htmltools,
  FactoMineR, missMDA, pcaMethods, caret
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
bioclim <- getData(name="worldclim", download=TRUE, path = "E:/Eranthis/maps", res= 2.5, var="bio")


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


#Making One table
tables <- lapply(all_dataframes, get)
tables <- lapply(tables, as.data.frame)
tables <- lapply(tables, function(tbl) mutate_all(tbl, as.character))

tables <- lapply(seq_along(all_dataframes), function(i) {
  df <- get(all_dataframes[[i]])  
  df <- mutate(df, Species = as.numeric(df$Species))
  df
})

combined_data <- bind_rows(tables)
combined_data <- combined_data[-c(103:107), -c(1,3,48) ]

#Species to Numbers
combined_data$Species <- factor(combined_data$Species, levels = unique(combined_data$Species))
combined_data$Species <- as.numeric(factor(combined_data$Species))

# MLR -------------------------------------------------------------------------------------------------------------
combined_data <- combined_data %>%
  mutate_at(vars(2:45), ~as.numeric(.))

model_fr <- combined_data[ , c(1,3,5,7,9,13,15,17,19,21,23)]
model_fl <- combined_data[ , c(1,)]

model_fr <- na.omit(model_fr)

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

model_fr$Species <- factor(model_fr$Species, levels = unique(model_fr$Species))
model_fr$Species <- as.numeric(factor(model_fr$Species))

model_fr$Species <- as.factor(model_fr[, 1])

res.pca <- PCA(model_fr[2:11], graph = FALSE)
print(summary(res.pca))
fviz(res.pca, "var", habillage = model_fr[, 1])
 