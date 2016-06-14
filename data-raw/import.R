library(readxl)
library(dplyr)

# Import data and labels
raw_dat <- read_excel("data-raw/rohdaten.xlsx")
labels <- read_excel("data-raw/rohdaten.xlsx", sheet = "labels")

# Set attribute for better display
attr(raw_dat, "variable.labels") <- labels[["langname"]]

# Convert to correct type
correct_type <- raw_dat %>%
  mutate_if(is.character, as.factor) %>%
  mutate_at("Anmerkung", as.character)

# Create proper factors for later display
refactored <- correct_type %>%
  mutate(v3 = factor(v3, levels = c("16", "17", "18", "19", "20-24", "25-29",
                                    "30-34", "35-39", "40-44", "45 u. älter")),
         v6 = factor(v6, levels = c("Ich nutze die Möglichkeiten des Internets intensiv und bin daher nicht auf ein Auto angewiesen",
                                    "Ich nutze Mitfahrgelegenheiten",
                                    "Ich halte die negativen Umweltfolgen für zu gravierend",
                                    "Ich halte die negativen sozialen Folgen des Autofahrens für zu gravierend",
                                    "Ich habe Angst vorm Autofahren",
                                    "Ich habe keine Lust Auto zu fahren",
                                    "Ich bin gerade dabei den Führerschein zu machen",
                                    "Der Erwerb und die Instandhaltung/Betrieb eines Autos sind für mich zu kostenintensiv",
                                    "Ich bevorzuge es zu Fuß zu gehen",
                                    "Ich bevorzuge es mit dem Fahrrad zu fahren",
                                    "Ich bevorzuge die Nutzung öffentlicher Verkehrsmittel",
                                    "Ich bin derzeit zu beschäftigt bzw. habe zu wenig Zeit, um einen Führerschein zu machen",
                                    "Invalidität, gesundheitliche Probleme, Sehschwäche",
                                    "Rechtliche Umstände",
                                    "Andere Gründe")),
         v10 = factor(v10, levels = c("Universitätsabschluss",
                                      "FH-Abschluss",
                                      "Matura",
                                      "Berufsbildende Schule",
                                      "Lehrabschluss",
                                      "Pflichtschulabschluss")))

devtools::use_data(refactored)
