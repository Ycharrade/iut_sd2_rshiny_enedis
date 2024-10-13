
# Projet Allier - Application RShiny Enedis

## Description
Ce projet a été développé dans le cadre d'une étude des données de logements du département de l'Allier. L'application est construite en R avec Shiny et permet d'explorer des informations sur les logements, telles que la surface habitable, les étiquettes GES, et les coordonnées géographiques (longitude et latitude).

### Fonctionnalités principales
- **Récupération de données via API** : Les données sont récupérées dynamiquement à partir d'une API dédiée.
- **Visualisation des données** : Plusieurs types de graphiques (histogrammes, boxplots, scatter plots) sont disponibles pour visualiser les informations sur les logements.
- **Carte interactive** : Les logements sont géolocalisés et affichés sur une carte en fonction de leur étiquette GES.
- **Tableau interactif** : Un tableau interactif permet de filtrer et d'explorer les 100 premières lignes des données récupérées.

## Installation

### Prérequis
Assurez-vous d'avoir R installé sur votre machine, ainsi que les packages suivants :
- `shiny`
- `shinydashboard`
- `ggplot2`
- `DT`
- `shinyjs`
- `shinythemes`
- `dplyr`
- `leaflet`
- `plotly`
- `rsconnect`

### Étapes d'installation
1. Clonez ce repository :
   \`\`\`bash
   git clone https://github.com/nom_utilisateur/iut_sd2_rshiny_enedis.git
   \`\`\`

2. Ouvrez le fichier RShiny dans RStudio ou votre éditeur préféré.

3. Installez les packages nécessaires si vous ne les avez pas encore :
   \`\`\`R
   install.packages(c("shiny", "shinydashboard", "ggplot2", "DT", "shinyjs", "shinythemes", "dplyr", "leaflet", "plotly", "rsconnect"))
   \`\`\`

4. Lancez l'application :
   \`\`\`R
   shiny::runApp("app.R")
   \`\`\`

## Architecture du projet
Le projet est divisé en deux parties principales :
1. **Récupération des données** : Le script `code_recup_donnee_allier_api.R` s'occupe de récupérer les données via l'API.
2. **Visualisation** : Le script `App.R` contient toute la logique pour afficher les données, les graphiques et la carte.

## Auteur
- **Yoann Charrade** : Gestion des données, récupération via API, intégration des coordonnées pour la carte.
- **Loic Cotte** : Conception des graphiques et mise en page de l'interface utilisateur (CSS, graphiques Shiny).

## Licence
Ce projet est sous licence MIT. Vous pouvez librement l'utiliser et le modifier.
