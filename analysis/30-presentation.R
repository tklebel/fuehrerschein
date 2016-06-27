library(ggplot2)
library(dplyr)
library(purrr)
library(fuehrerschein)
library(tidyr)
library(ggthemes)
library(crosstabr)

refactored <- readr::read_rds("data/refactored.rds")

labels <- refactored %>%
  attributes() %>%
  .[c("names", "variable.labels")] %>%
  bind_cols()

# Descriptives

# Alter -----
pdata <- refactored %>%
  count(v3) %>%
  mutate(perc = n/sum(n))

alter <- ggplot(pdata, aes(v3, perc)) +
  geom_bar(fill = "firebrick3", stat = "identity") +
  scale_x_discrete(drop = F) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = find_label(labels, "v3"),
       x = NULL, y = "Anzahl der Befragten",
       fill = NULL) +
  theme_fivethirtyeight()

pdata <- refactored %>%
  group_by(v4) %>%
  count(v3) %>%
  mutate(perc = n/sum(n))

alter_geschlecht <- ggplot(pdata, aes(v3, perc, fill = v4)) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_discrete(drop = F) +
  scale_y_continuous(labels = scales::percent) +
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
pdata <- refactored %>%
  count(v8) %>%
  mutate(perc = n/sum(n))

führerschein <- ggplot(pdata, aes(v8, perc)) +
  geom_bar(fill = "firebrick3", stat = "identity") +
  scale_x_discrete(drop = F) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = stringr::str_replace(find_label(labels, "v8"), "vor", "vor,"),
       x = NULL, y = "Anzahl der Befragten",
       fill = NULL,
       caption = paste0("N = ", sum(pdata$n))) +
  theme_fivethirtyeight()

ggsave("graphs/fuehrerschein.png", führerschein, width = 7, height = 7)

# Gruppiert nach Geschlecht
pdata <- refactored %>%
  group_by(v4) %>%
  count(v8) %>%
  mutate(perc = n/sum(n))

ggplot(pdata, aes(v8, perc, fill = v4)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(drop = F) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = find_label(labels, "v8"),
       x = NULL, y = "Anzahl der Befragten",
       fill = NULL,
       caption = paste0("N = ", sum(pdata$n))) +
  theme_fivethirtyeight()

# Gruppiert nach Alter
pdata <- refactored %>%
  group_by(alter) %>%
  count(v8) %>%
  mutate(perc = n/sum(n))

führerschein_gender <- ggplot(pdata, aes(v8, perc, fill = alter)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_x_discrete(drop = F) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = stringr::str_replace(find_label(labels, "v8"), "vor", "vor,"),
       x = NULL, y = "Anzahl der Befragten",
       fill = NULL,
       caption = paste0("N = ", sum(pdata$n))) +
  theme_fivethirtyeight()

ggsave("graphs/fuehrerschein_gender.png", führerschein_gender, width = 7, height = 7)

# Kreuztabelle dazu
refactored %>%
  crosstab(v8 ~ alter) %>%
  add_stats(fisher.test)

# gruppiert danach, ob Partner führerschein hat
refactored %>%
  mutate(v9 = factor(v9, levels = c("Ja", "Nein", "TNZ"),
                     labels = c("Ja", "Nein", "Trifft nicht zu"))) %>%
  mutate(v9 = recode(v9, "Trifft nicht zu" = NA_character_, .default = levels(v9))) %>%
  crosstab(v8 ~ v9) %>%
  add_stats(fisher.test)

  # group_by(v9) %>%
  # count(v8) %>%
  # filter(!is.na(v9)) %>%
  # mutate(perc = n/sum(n))



# Hat Ehegatte/Partner Auto? ------
pdata <- refactored %>%
  count(v9) %>%
  mutate(v9 = factor(v9, levels = c("Ja", "Nein", "TNZ"),
                     labels = c("Ja", "Nein", "Trifft nicht zu")),
         perc = n/sum(n))

partner <- ggplot(pdata, aes(v9, perc)) +
  geom_bar(stat = "identity", fill = "firebrick3") +
  scale_x_discrete(drop = F) +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_brewer(palette = "Set1") +
  labs(title = stringr::str_wrap(find_label(labels, "v9"), 50),
       x = NULL, y = "Anzahl der Befragten",
       fill = NULL,
       caption = paste0("N = ", sum(pdata$n))) +
  theme_fivethirtyeight()

ggsave("graphs/partner.png", partner, width = 7, height = 7)

# Bildung -----
pdata <- refactored %>%
  count(v10) %>%
  mutate(v10 = factor(v10, levels = c("Pflichtschulabschluss",
                                    "Lehrabschluss",
                                    "Berufsbildende Schule",
                                    "Matura",
                                  "FH-Abschluss",
                                  "Universitätsabschluss")),
         perc = n/sum(n)) %>%
  filter(!is.na(v10))

bildung <- ggplot(pdata, aes(v10, perc)) +
  geom_bar(fill = "firebrick3", stat = "identity") +
  scale_x_discrete(drop = F) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = stringr::str_wrap(find_label(labels, "v10"), 50),
       x = NULL, y = "Anzahl der Befragten",
       fill = NULL,
       caption = paste0("N = ", sum(pdata$n))) +
  theme_fivethirtyeight()

ggsave("graphs/bildung.png", bildung, width = 9, height = 7)


# Beschäftigung ----
pdata <- refactored %>%
  count(v12) %>%
  mutate(perc = n/sum(n),
         v12 = stringr::str_wrap(v12, 20))

arbeit <- ggplot(pdata, aes(v12, perc)) +
  geom_bar(fill = "firebrick3", stat = "identity") +
  scale_x_discrete(drop = F) +
  scale_y_continuous(labels = scales::percent) +
  labs(title = stringr::str_wrap(find_label(labels, "v12"), 50),
       x = NULL, y = "Anzahl der Befragten",
       fill = NULL,
       caption = paste0("N = ", sum(pdata$n))) +
  theme_fivethirtyeight()

ggsave("graphs/arbeit.png", arbeit, width = 9, height = 7)

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
  group_by(v6, v4) %>%
  filter(sum(n) > 4) %>%
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

ggsave("graphs/hauptgründe_gender.png", hauptgründe_gender, scale = 1, width = 7)

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
refactored$v6andere %>% unique()

# Scheinkausalität-----
with(refactored, cor.test(as.numeric(v8), v11b,
                     method = "pearson"))
