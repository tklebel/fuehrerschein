# Analyse der Befragung zum Führerschein

Code und Daten dieser Analyse sind ein Übungsbeispiel aus dem Kurs 
"Technik – Ethik – Politik: Herausforderungen einer nachhaltigen
Technikgestaltung".

## Verwendung des Repositories

Die Struktur des Pakets ist nicht ideal, daher sind zwei Schritte notwendig:

- einerseits die Dateien herunterzuladen (z.b. über `git clone`)
- r-paket installieren, um Zugriff auf den Code zu haben.

### Herunterladen der Daten

Entweder über git:

```
git clone git@github.com:tklebel/fuehrerschein.git
```

oder einfach als Download von [GitHub](https://github.com/tklebel/fuehrerschein).

### Installation der Software

```r
# install.packages("devtools")
devtools::install_github("tklebel/fuehrerschein")
```

## Analyse der Daten

Die Daten wurden in die Datei `rohdaten.xlsx` im Ordner `data-raw` eingegeben.
Danach wurden die Daten nach R mit der Datein `01-import.R` im Ordner `analysis`
importiert. Weitere Auswertungen finden sich in den anderen Dateien desselben
Ordners.

