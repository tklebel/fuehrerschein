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

# Alter und Internet ------
with(refactored, cor(as.numeric(v3), as.numeric(v11b), use = "complete.obs"))

pdata <- refactored %>%
  mutate(alter = case_when(as.numeric(.$v3) < 5 ~ "unter 20",
                           as.numeric(.$v3) > 6 ~ "30 oder älter",
                           TRUE ~ "20-29"),
         alter = factor(alter, levels = c("unter 20", "20-29", "30 oder älter"))) %>%
  select(alter, v3, dplyr::contains("v11")) %>%
  gather(var, wert, -alter, -v3) %>%
  mutate(var = recode(var, "v11a" = "beruflich", "v11b" = "privat"))

ggplot(pdata, aes(v3, wert)) +
  geom_boxplot() +
  geom_jitter(width = .2, height = .1) +
  facet_wrap(~var)

ggplot(pdata, aes(alter, wert)) +
  geom_boxplot(fill = "firebrick3") +
  facet_wrap(~var) +
  theme_bw() +
  labs(x = NULL,
       y = "Stunden",
       title = "Online verbrachte Zeit")


# Sekundäre Gründe #########
summarised <- refactored %>%
  select(starts_with("v7"), -v7q) %>%
  gather(var, wert) %>%
  group_by(var) %>%
  summarise(mean = mean(wert, na.rm = T)) %>%
  filter(var != "v7a") %>%
  arrange(mean) %>%
  rename(names = var) %>%
  left_join(labels)


# plot it
pdata <- refactored %>%
  select(starts_with("v7"), -v7q) %>%
  gather(var, wert) %>%
  group_by(var) %>%
  filter(var != "v7a", !is.na(wert)) %>%
  rename(names = var) %>%
  left_join(labels) %>%
  mutate(langname = stringr::str_wrap(variable.labels, 50),
         median = median(wert))


p_labels <- pdata %>%
  mutate(n = n()) %>%
  ungroup() %>%
  select(n, langname, median) %>%
  distinct() %>%
  mutate(label = paste0("n = ", n)) %>%
  arrange(desc(median), langname)


rev_median <- function(x) {
  res <- median(x)
  -res
}

ggplot(pdata, aes(reorder(langname, wert, FUN = "rev_median"), wert)) +
  coord_flip() +
  geom_boxplot() +
  annotate("text", x = 1:15, y = 0, label = p_labels$label, fontface = "italic",
           family = "mono") +
  geom_jitter(width = .4, height = .2, alpha = .5, colour = "firebrick2") +
  theme_bw() +
  scale_y_continuous(limits = c(0, NA), breaks = seq(1, 15, 2)) +
  labs(x = NULL,
       y = "Vergebener Rang",
       title = "Mittlere Bedeutung der einzelnen Gründe")
