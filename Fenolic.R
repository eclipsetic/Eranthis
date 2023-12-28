# START -----------------------------------------------------------------------------------------------------------
Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(readxl, ggplot2)
setwd("E:/Eranthis")
fenolic <- read_excel("fenolic.xlsx")

ggplot(fenolic) +
  geom_bar( aes(x=name, y=leaves), stat="identity", fill="skyblue", alpha=0.7)

data <- data.frame(
  name=letters[1:5],
  value=sample(seq(4,15),5),
  sd=c(1,0.2,3,2,4)
)
