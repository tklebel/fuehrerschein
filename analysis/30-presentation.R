library(ggplot2)
library(dplyr)
library(purrr)
library(fuehrerschein)
library(tidyr)
library(ggthemes)

refactored <- readr::read_rds("data/refactored.rds")

labels <- refactored %>%
  attributes() %>%
  .[c("names", "variable.labels")] %>%
  bind_cols()

# Descriptives

# Alter -----
alter <- ggplot(refactored, aes(v3)) +
  geom_bar(fill = "firebrick3") +
  scale_x_discrete(drop = F) +
  labs(title = find_label(labels, "v3"),
       x = NULL, y = "Anzahl der Befragten",
       fill = NULL) +
  theme_fivethirtyeight()

alter_geschlecht <- ggplot(refactored, aes(v3, fill = v4)) +
  geom_bar(position = "dodge") +
  scale_x_discrete(drop = F) +
  labs(title = find_label(labels, "v3"),
       x = NULL, y = "Anzahl der Befragten",
       fill = NULL) +
  theme_fivethirtyeight() +
  scale_fill_brewer(palette = "Set1")

ggsave("graphs/alter.png", alter, width = 7)
ggsave("graphs/alter_geschlecht.png", alter_geschlecht, width = 7)

# Geschlecht -----
ggplot(refactored, aes(v4)) +
  geom_bar()

table(refactored$v4)/unlist(tally(refactored))

# Wann Führerschein machen? ------


# Hat Ehegatte/Partner Auto? ------


# Bildung -----


# Beschäftigung ----

# Alter und Internet ------
with(refactored, cor(as.numeric(v3), as.numeric(v11b), use = "complete.obs"))

pdata <- refactored %>%
  select(alter, v3, dplyr::contains("v11")) %>%
  gather(var, wert, -alter, -v3) %>%
  mutate(var = recode(var, "v11a" = "beruflich", "v11b" = "privat"))

# ggplot(pdata, aes(v3, wert)) +
#   geom_boxplot() +
#   geom_jitter(width = .2, height = .1) +
#   facet_wrap(~var)

online <- ggplot(pdata, aes(alter, wert)) +
  geom_boxplot(fill = "firebrick3") +
  facet_wrap(~var) +
  theme_fivethirtyeight() +
  labs(x = NULL,
       y = "Stunden",
       title = "Online verbrachte Zeit")

ggsave("graphs/online.png", online)


# Hauptgründe -----
pdata <- refactored %>%
  count(v6) %>%
  filter(!is.na(v6)) %>%
  mutate(perc = n/sum(n),
         v6 = stringr::str_wrap(v6, width = 50))

hauptgründe <- ggplot(pdata, aes(reorder(v6, perc), perc)) +
  geom_bar(stat = "identity", fill = "firebrick3") +
  theme_fivethirtyeight() +
  coord_flip() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = "Anteil der Befragten",
       title = "Hauptgrund, keinen Führerschein zu haben",
       caption = paste0("N = ", sum(pdata$n)))

ggsave("graphs/hauptgründe.png", hauptgründe, width = 9)

# Für die folgende Grafik wurden zwecks der Übersichtlichkeit jene Gründe
# ausgewählt, die von mehr als fünf
# Befragten genannt wurden.

pdata <- refactored %>%
  group_by(v4) %>%
  count(v6) %>%
  mutate(perc = n/sum(n),
         v6 = stringr::str_wrap(v6, width = 30)) %>%
  group_by(v6) %>%
  filter(sum(n) > 5) %>%
  group_by(v4)

# Überprüfen: ist das wirklich so schön kongruent zwischen Frauen und Männern?
# Jap
hauptgründe_gender <- ggplot(pdata, aes(v4, perc, fill = reorder(v6, 1 - perc))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = "Anteil der Befragten",
       fill = NULL,
       title = "Hauptgründe, keinen Führerschein zu haben",
       caption = paste0("N = ", sum(pdata$n))) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

ggsave("graphs/hauptgründe_gender.png", hauptgründe_gender, scale = 1.2, width = 7)

# hauptgründe nach beschäftigung
pdata <- refactored %>%
  group_by(v12) %>%
  count(v6) %>%
  mutate(perc = n/sum(n),
         v6 = stringr::str_wrap(v6, width = 30)) %>%
  group_by(v6) %>%
  filter(sum(n) > 5) %>%
  group_by(v12)

# Überprüfen: ist das wirklich so schön kongruent zwischen Frauen und Männern?
# Jap

hauptgründe_beschäftigung <- ggplot(pdata, aes(v12, perc, fill = reorder(v6, 1 - perc))) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_fivethirtyeight() +
  scale_y_continuous(labels = scales::percent) +
  labs(x = NULL,
       y = "Anteil der Befragten",
       fill = NULL,
       title = "Hauptgründe, keinen Führerschein zu haben",
       caption = paste0("N = ", sum(pdata$n))) +
  scale_fill_brewer(palette = "Set1") +
  theme(legend.position = "bottom")

# Sekundäre Gründe -----
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

sek_gründe <- ggplot(pdata, aes(reorder(langname, wert, FUN = "rev_median"), wert)) +
  coord_flip() +
  geom_boxplot() +
  annotate("text", x = c(1:15) + .12, y = 0, label = p_labels$label, fontface = "italic",
           family = "mono") +
  geom_jitter(width = .4, height = .2, alpha = .5, colour = "firebrick2") +
  theme_fivethirtyeight() +
  scale_y_continuous(limits = c(0, NA), breaks = seq(1, 15, 2)) +
  labs(x = NULL,
       y = "Vergebener Rang",
       title = "Mittlere Bedeutung der einzelnen Gründe")

ggsave("graphs/sekundäre_gründe.png", sek_gründe, width = 12, height = 9)

# Sonstige Gründe -----
