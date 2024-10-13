# Charger les bibliothèques nécessaires
library(dplyr)

# Lire les fichiers CSV
donnee <- read.csv("C:/Users/gayod/OneDrive/Bureau/projetv2/donnee.csv", sep=",", header=TRUE, stringsAsFactors=FALSE)
adresses <- read.csv("C:/Users/gayod/OneDrive/Bureau/projetv2/adresses-03.csv", sep=";", header=TRUE, stringsAsFactors=FALSE)

# Fusionner les données sur Identifiant__BAN de donnee et id de adresses
merged_data <- donnee %>%
  left_join(adresses %>% select(id, x, y, lon, lat), 
            by = c("Identifiant__BAN" = "id"))

# Remplacer les informations géographiques manquantes par NA
merged_data <- merged_data %>%
  mutate(across(c(x, y, lon, lat), ~ ifelse(is.na(.), NA, .)))

# Sauvegarder les données fusionnées dans un nouveau fichier CSV
write.csv2(merged_data, "C:/Users/gayod/OneDrive/Bureau/projetv2/merged_donnee.csv", row.names = FALSE)
