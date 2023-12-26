# START -----------------------------------------------------------------------------------------------------------
Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(readxl, ggplot2)
setwd("E:/Eranthis")
fenolic <- read_excel("fenolic.xlsx")
