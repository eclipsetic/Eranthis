# START -----------------------------------------------------------------------------------------------------------
Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(lubridate, purrr, dplyr, tidyr, forecast, zoo, rlang, ggplot2, tidyverse, raster,
  sp, geodata, terra, rasterVis, BiocManager, dismo, XML, jsonlite, rgdal, rJava,
  readxl, rgbif, factoextra, NbClust, cluster, openxlsx, caret, mice, missForest, knitr, FactoMineR
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

# For Making Means instead of NA
for (df_name in all_dataframes) {
  df <- get(df_name)
  df <- lapply(df, function(x) {
    ifelse(is.na(x), mean(x, na.rm = TRUE), x)
  })
  assign(df_name, df, envir = .GlobalEnv)
}

#Making One table
tables <- lapply(all_dataframes, get)
tables <- lapply(tables, as.data.frame)
tables <- lapply(tables, function(tbl) mutate_all(tbl, as.character))
combined_data <- bind_rows(tables)
write.xlsx(combined_data, "combined_data.xlsx", rowNames = FALSE)
combined_data <- read.xlsx("combined_data.xlsx")

#MAking Means to One
combined_data <- combined_data %>%
  mutate_at(vars(2:45), as.numeric)

combined_data <- combined_data %>%
  group_by(Species) %>%
  summarize(across(everything(), ~ mean(., na.rm = TRUE)))

# Create a distance matrix
dist_matrix <- dist(combined_data)

# Perform hierarchical clustering
hc <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(hc, main = "Hierarchical Clustering Dendrogram", sub = NULL, xlab = NULL, cex = 0.8, labels = combined_data$Species)


# PCA -------------------------------------------------------------------------------------------------------------

combined_data <- combined_data %>%
  mutate_at(vars(2:45), as.numeric)

combined_data_factorized <- lapply(combined_data, function(x) {
  if(is.character(x)) {
    as.factor(x)
  } else {
    x
  }
})
combined_data_factorized <- as.data.frame(combined_data_factorized)

pca_result <- prcomp(combined_data_factorized, scale. = TRUE, na.fail)


PCA(combined_data, scale.unit = TRUE, ncp = 3, graph = TRUE)
