# START -----------------------------------------------------------------------------------------------------------
Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(lubridate, purrr, dplyr, tidyr, forecast, zoo, rlang, ggplot2, tidyverse, raster,
  sp, geodata, terra, rasterVis, BiocManager, dismo, XML, jsonlite, rJava,
  readxl, factoextra, NbClust, cluster, openxlsx, caret, mice, missForest, knitr, htmltools,
  FactoMineR, missMDA, pcaMethods, caret, ggfortify, gridExtra, hrbrthemes, corrplot, caTools, 
  vegan, pvclust, ClassDiscovery
)
# rgdal, rgbif
pacman::p_load(dplyr, factoextra, fastICA, ggplot2, ggpubr, NMF,  party, psych, randomForest,
  reshape2, Rtsne, shipunov, tidyverse, tseries, umap, vegan)
           
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

imp <- mice(data = combined_data, method = 'rf', m = 1, seed=500)
combined_data <- complete(imp)


#Making One table
tables <- lapply(all_dataframes, get)
tables <- lapply(tables, as.data.frame)
tables <- lapply(tables, function(tbl) mutate_all(tbl, as.character))

tables <- lapply(seq_along(all_dataframes), function(i) {
  df <- get(all_dataframes[[i]])
  df <- as.data.frame(mutate_all(df, as.character))
  df <- df[complete.cases(df$Species), ]
  df$Species <- all_dataframes[[i]]
  return(df)
})

combined_data <- bind_rows(tables)
combined_data <- combined_data[, -c(1,3,34:44,48) ]
combined_data <- combined_data %>%
  mutate_at(vars(2:34), ~as.numeric(.))


# MLR -------------------------------------------------------------------------------------------------------------

#1,3,15,17,19,23,29,30,31
model_fr <- combined_data[ , c(1,3,5,7,9,13,15,17,19,21,23,29,30,31,33,34)]

filter <- apply(model_fr[, -1], 1, function(row) !all(is.na(row)))
model_fr <- model_fr[filter, ]
model_fr<- na.omit(model_fr)
table(model_fr)

model_fl <- combined_data[ , c(1,2,4,6,8,12,14,16,18,20,22,24,25,26,27,28,32)]

filter <- apply(model_fl[, -1], 1, function(row) !all(is.na(row)))
model_fl <- model_fl[filter, ]
model_fl <- na.omit(model_fl)
table(model_fl)

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

data_plot <- reshape2::melt(combined_data, id.vars = "Species")
target_species <- c('E.sibirica', 'E.tanhoensis', 'E.sibirica_x_E.tanhoensis')
data_plot_target <- data_plot[data_plot$Species %in% target_species, ]

ggplot(data_plot_target, aes(x = Species, y = value, fill = variable)) +
  geom_boxplot(data = subset(data_plot_target, variable %in% c("PHfl", "PHfr")),
               position = position_dodge(width = 0.8), width = 0.5) +
  scale_fill_manual(values = c("PHfl" = "slateblue1", "PHfr" = "tomato"), name = "Данные") +
  theme_minimal()

# Multiple Linear Regression Modelling ----------------------------------------------------------------------------
combined_data <- combined_data %>%
  mutate_at(vars(2:42), ~as.numeric(.))

set.seed(123)
split = sample.split(combined_data$PHfl, SplitRatio = 0.5)
training_set <- subset(combined_data, split == TRUE)
test_set <- subset(combined_data, split == FALSE)

sapply( lapply(training_set, unique), length)
training_set <- training_set[, colSums(is.na(training_set)) != nrow(training_set)]
values_count <- sapply(lapply(training_set, unique), length)

regressor = lm.fit(formula = PHfl ~ ., data = training_set)
y_pred = predict(regressor, newdata = test_set)
summary(y_pred)
head(cbind(y_pred$PHfl,testing))


# columns_to_impute <- c("Species", "PHfl", "PHfr", "BLSLfl", "BLSLfr", "BLSWfl", "BLSWfr", "BLSDfl", "BLSDfr", 
#                      "BSLNfl", "BSLNfr", "BSTNfl", "BSTNfr", "CLSLfl", "CLSLfr", "CLSWfl", "CLSWfr", "CLSDfl", 
#                       "CLSDfr", "CSLNfl", "CSLNfr", "CSTNfl", "CSTNfr", "SN", "SL", "SW", "PN", "PL", "FN", 
#                       "FL", "StL", "LCfl", "BLA", "CLA", "FP", "FSP", "PS", "SP", "AAdLC", "AAbLC", "MLC", 
#                       "SC")



mising_value <- function(data_p, nx){
  pr_names<-c("y")
  for(i in 2:ncol(data_p)) pr_names<-c(pr_names, paste("x", i-1, sep="")) 
  colnames(data_p)<-pr_names
  f<-paste(pr_names[1], "~", sep="")
  for(i in 2:length(pr_names)) f<-paste(f, "+", pr_names[i], sep="")  
  f<-as.formula(f)
  fit <- lm(f, data=data_p)
  newdata=data.frame(x=nx[1])
  for(i in 2:length(nx)) newdata<-cbind(newdata, c(nx[i]))
  colnames(newdata)<-pr_names[2:length(pr_names)]
  predict(fit, newdata)[[1]]
}

pop <- model_fl[,1]
data <- model_fl[,-c(1)]
data_rep <- data


for (i in 1:ncol(data)) {
  for (j in 1:nrow(data)) {
    if (is.na(data[j, i])) {
      yd <- data[, i]
      nx <- as.numeric(data[j, ])
      nx <- nx[!is.na(nx)]
      na_index <- which(!is.na(data[j, , drop = FALSE]))
      data_p <- data[, na_index]
      data_p <- cbind(y = yd, data_p)
      data_p <- data_p[rowSums(is.na(data_p)) == 0, ]
      data_rep[j,i] <- mising_value(data_p, nx)
    }
  }
}


data_rep<-cbind(pop=pop, data_rep)

data<-data_rep

row_n<-data.frame(point=data[,1])

data<-data[,-1]

data_am<-data.frame(dep=data[,12], point=row_n)

fit <- aov(dep ~ point, data=data_am)

summary(fit)

a<-3.019

1-1/a

boxplot(dep ~ point, data=data_am)

for(i in 2:ncol(data)){
  data[,i]<-(data[,i]-mean(data[,i]))/sd(data[,i])
}

fit<-prcomp(data[,2:12])
fviz_pca_biplot(fit, habillage=row_n[,1], addEllipses=T, pointsize = 6)



data_rep <- data_rep %>% rename(Species = pop)
target_species <- c('E.sibirica', 'E.tanhoensis', 'E.sibirica_x_E.tanhoensis')

# 'E.sibirica', 'E.tanhoensis', 'E.sibirica_x_E.tanhoensis'                                                                                       
# 'E.sibirica', 'E.tanhoensis', 'E.sibirica_x_E.tanhoensis', 'E.krasnoborovii', 'E.sineli'                                                         
# 'E.sineli', 'E.stellata', 'E.stellata.Korea.'                                                                                                 
# 'E.stellata', 'E.stellata.Korea.'                                                                                                            
# 'E.albiflora', 'E.lobulata'                                                                                                          
# 'E.pinnatifida', 'E.byunsanensis', 'E.pungdoensis'                                                                                    
# 'E.byunsanensis', 'E.pungdoensis'                                                                                                     
# 'E.albiflora', 'E.lobulata', 'E.sibirica', 'E.tanhoensis', 'E.stellata', 'E.pinnatifida', 'E.byunsanensis'

filtered_data <- data_rep[data_rep$Species %in% target_species, ]
res.pca <- PCA(model_fl, quali.sup = 1, graph = FALSE, ncp = 5)
fviz_pca_biplot(res.pca, label = "var", habillage = 1, col.var = "black",
                addEllipses = TRUE, pointsize = 3, ellipse.level = 0.95,
                mean.point = FALSE, ellipse.alpha = 0, repel = TRUE) +
  scale_color_brewer(palette = "Set1") +
  theme_minimal()

fviz_pca_ind(res.pca, label = "var", habillage = 1, col.var = "black",
                addEllipses = TRUE, pointsize = 3, ellipse.level = 0.95,
                mean.point = FALSE, ellipse.alpha = 0, repel = TRUE) 

fviz_pca_var(res.pca, col.var="black")+
  theme_minimal() 

fviz_eig(res.pca)
get_pca(res.pca)$contrib
fviz_pca_contrib(res.pca, choice = c("var"))
res.pca$eig

                 