library(httr)
library(jsonlite)
library(dplyr)

# URL de base
base_url <- "https://data.ademe.fr/data-fair/api/v1/datasets/dpe-v2-logements-existants/lines"

# Définir les périodes trimestrielles
date_ranges <- list(
  # 2015
  "2015-01-01 TO 2015-03-31",
  "2015-04-01 TO 2015-06-30",
  "2015-07-01 TO 2015-09-30",
  "2015-10-01 TO 2015-12-31",
  # 2016
  "2016-01-01 TO 2016-03-31",
  "2016-04-01 TO 2016-06-30",
  "2016-07-01 TO 2016-09-30",
  "2016-10-01 TO 2016-12-31",
  # 2017
  "2017-01-01 TO 2017-03-31",
  "2017-04-01 TO 2017-06-30",
  "2017-07-01 TO 2017-09-30",
  "2017-10-01 TO 2017-12-31",
  # 2018
  "2018-01-01 TO 2018-03-31",
  "2018-04-01 TO 2018-06-30",
  "2018-07-01 TO 2018-09-30",
  "2018-10-01 TO 2018-12-31",
  # 2019
  "2019-01-01 TO 2019-03-31",
  "2019-04-01 TO 2019-06-30",
  "2019-07-01 TO 2019-09-30",
  "2019-10-01 TO 2019-12-31",
  # 2020
  "2020-01-01 TO 2020-03-31",
  "2020-04-01 TO 2020-06-30",
  "2020-07-01 TO 2020-09-30",
  "2020-10-01 TO 2020-12-31",
  # 2021
  "2021-01-01 TO 2021-03-31",
  "2021-04-01 TO 2021-06-30",
  "2021-07-01 TO 2021-09-30",
  "2021-10-01 TO 2021-12-31",
  # 2022
  "2022-01-01 TO 2022-03-31",
  "2022-04-01 TO 2022-06-30",
  "2022-07-01 TO 2022-09-30",
  "2022-10-01 TO 2022-12-31",
  # 2023
  "2023-01-01 TO 2023-03-31",
  "2023-04-01 TO 2023-06-30",
  "2023-07-01 TO 2023-09-30",
  "2023-10-01 TO 2023-12-31",
  # 2024
  "2024-01-01 TO 2024-03-31",
  "2024-04-01 TO 2024-06-30",
  "2024-07-01 TO 2024-09-30",
  "2024-10-01 TO 2024-12-31",
  # 2025
  "2025-01-01 TO 2025-03-31",
  "2025-04-01 TO 2025-06-30",
  "2025-07-01 TO 2025-09-30",
  "2025-10-01 TO 2025-12-31"
)

# Créer un DataFrame vide pour stocker toutes les données
all_data <- data.frame()

# Boucle pour récupérer les pages et ajouter les données
for (date_range in date_ranges) {
  page <- 1
  repeat {
    # Paramètres de la requête pour chaque page
    params <- list(
      size = 1000,  # Taille de page
      select = "N°DPE,Code_postal_(BAN),Etiquette_DPE,Date_réception_DPE,Etiquette_GES,Surface_habitable_logement,Type_énergie_principale_chauffage,Date_établissement_DPE,Année_construction,Type_bâtiment,Identifiant__BAN",
      q = "03*",
      q_fields = "Code_postal_(BAN)",
      qs = paste0("Date_réception_DPE:[", date_range, "]"),
      page = page
    )
    
    # Effectuer la requête GET
    response <- GET(base_url, query = params)
    
    # Vérifier le statut de la réponse
    status_code <- status_code(response)
    
    # Si la requête est réussie, traiter les résultats
    if (status_code == 200) {
      raw_content <- content(response, as = "text", encoding = "UTF-8")
      content <- fromJSON(raw_content)
      
      if (!is.null(content$results)) {
        df <- as.data.frame(content$results)
        all_data <- bind_rows(all_data, df)  # Ajouter les résultats au DataFrame
        
        # Si moins de résultats que la taille de page, arrêter (dernière page)
        if (nrow(df) < params$size) {
          break
        }
      } else {
        break  # Arrêter si aucun résultat
      }
    } else {
      cat("Erreur lors de la récupération de la page :", page, "pour la période", date_range, "Statut :", status_code, "\n")
      break
    }
    
    # Affichage d'avancement
    cat("Page", page, "pour la période", date_range, "traitée.\n")
    
    # Pause de 1 seconde entre les requêtes pour respecter les limites de l'API
    Sys.sleep(1)
    
    page <- page + 1
  }
}

# Exporter les résultats au format CSV
write.csv(all_data, "toutes_les_donnees_03.csv", row.names = FALSE)
cat("Les données ont été exportées dans le fichier 'toutes_les_donnees_03.csv'.\n")
