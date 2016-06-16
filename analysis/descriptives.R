library(ggplot2)
library(dplyr)
library(purrr)
library(fuehrerschein)
library(tidyr)

refactored <- readr::read_rds("data/refactored.rds")

labels <- refactored %>%
  attributes() %>%
  .[c("names", "variable.labels")] %>%
  bind_cols()

# Descriptives

# Alter -----
refactored %>%
  filter(!is.na(v3)) %>%
  ggplot(aes(v3)) +
  geom_bar() +
  scale_x_discrete(drop = F) +
  labs(title = find_label(labels, "v3"),
       x = NULL, y = "Anzahl der Befragten") +
  theme_bw()


# Map function over all factors ----
refactored %>%
  select(v3, v4, v8:v10, v12) %>%
  plot_factors(.labels = labels)

