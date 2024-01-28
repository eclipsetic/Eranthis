# START -----------------------------------------------------------------------------------------------------------
Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(lubridate, purrr, dplyr, tidyr, forecast, zoo, rlang, ggplot2, tidyverse, raster,
  sp, geodata, terra, rasterVis, BiocManager, devtools, dismo, XML, jsonlite, rgdal, rJava,
  readxl, rgbif, leaflet, factoextra, NbClust, cluster, Hmisc, caret, mice, missForest
)


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

tables <- lapply(all_dataframes, get)
tables <- lapply(tables, function(tbl) mutate_all(tbl, as.character))
combined_data <- bind_rows(tables)

# Claster

data_for_clustering <- combined_data %>% dplyr::select(Species, PHfl)

data_for_clustering <- na.omit(data_for_clustering)
data_for_clustering$PHfl <- as.numeric(data_for_clustering$PHfl)
# Create a distance matrix
dist_matrix <- dist(imputed_data)

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc, main = "Hierarchical Clustering Dendrogram", sub = NULL, xlab = NULL, cex = 0.8)




# Выберем только нужные столбцы для кластеризации
data_for_clustering <- combined_data %>% dplyr::select(Species, PHfl)
data_for_clustering$PHfl <- as.numeric(data_for_clustering$PHfl)
data_for_clustering <- data_for_clustering[complete.cases(data_for_clustering), ]
data_for_clustering$PHfl <- as.numeric(as.character(data_for_clustering$PHfl), na.rm = TRUE)
# Кластеризация методом k-средних (k-means)
kmeans_result <- kmeans(data_for_clustering, centers = 3)

# Визуализация результатов
plot(data_for_clustering, col = kmeans_result$cluster, main = "K-means Clustering")

# Вывод информации о кластерах
cat("Cluster Centers:\n")
print(kmeans_result$centers)

# Вывод результатов кластеризации
combined_data$cluster <- kmeans_result$cluster