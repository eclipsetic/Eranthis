# START -----------------------------------------------------------------------------------------------------------
Sys.setenv(LANG = "en_US.UTF-8")
#Library
library(pacman)
pacman::p_load(readxl, ggplot2, scales, extrafont, stringr)
setwd("E:/Eranthis")
fenolic <- read_excel("fenolic.xlsx")

fenolic$leaves <- as.numeric(as.character(fenolic$leaves))
fenolic$flowers <- as.numeric(as.character(fenolic$flowers))

fenolic_long <- tidyr::gather(fenolic, key = "variable", value = "value", -name)

# Создание графика с разделением по оси x
ggplot(fenolic_long, aes(x = name, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Leaves and Flowers Comparison", x = "Name", y = "Count") +
  scale_fill_manual(values = c("leaves" = "green", "flowers" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(. ~ variable, scales = "free_x", space = "free_x") +
  scale_y_continuous(labels = comma)

ggplot(fenolic_long, aes(x = name, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Peak area, ion count", y = "Сompound") +
  scale_fill_manual(values = c("leaves" = "green", "flowers" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma, breaks = seq(0, max(fenolic_long$value), by = 50000000))

ggplot(fenolic_long, aes(x = factor(name, levels = unique(name)), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Peak area, ion count", y = "Сompound") +
  scale_fill_manual(values = c("leaves" = "green", "flowers" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = comma, breaks = seq(0, max(fenolic_long$value), by = 50000000))

font <- "Palatino Linotype"

fenolic_long$variable_label <- str_to_title(fenolic_long$variable)


ggplot(fenolic_long, aes(x = factor(name, levels = unique(name)), y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge", size = 1.5) +
  labs(x = "Сompound", y = "Peak area, ion count",
       fill = "Variable:") +
  scale_fill_manual(values = c("leaves" = "green", "flowers" = "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, family = font, face = "bold"),
        text = element_text(family = font),
        axis.title = element_text(family = font, face = "bold"),
        plot.title = element_text(family = font, face = "bold"),
        legend.text = element_text(family = font),
        legend.title = element_text(family = font, face = "bold"),
        legend.position = "right") +
  scale_y_continuous(labels = comma, breaks = seq(0, max(fenolic_long$value), by = 50000000))

