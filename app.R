# Liste des packages nécessaires
packages <- c(
  "shiny", "shinydashboard", "ggplot2", "DT", 
  "shinyjs", "shinythemes", "dplyr", 
  "leaflet", "plotly", "rsconnect"
)

# Fonction pour installer les packages manquants
install_if_missing <- function(pkg) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# Installer et charger les packages
lapply(packages, install_if_missing)

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)
library(shinyjs)
library(shinythemes)
library(dplyr)
library(leaflet)
library(plotly)
library(rsconnect)
setwd(getwd())


# Charger les données à partir du fichier CSV
df_donnees <- read.csv("merged_donnee.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)

# Convertir les dates et les colonnes numériques si nécessaire
df_donnees$Date_établissement_DPE <- as.Date(df_donnees$Date_établissement_DPE, format = "%Y-%m-%d")
df_donnees$Date_réception_DPE <- as.Date(df_donnees$Date_réception_DPE, format = "%Y-%m-%d")
df_donnees$Surface_habitable_logement <- as.numeric(df_donnees$Surface_habitable_logement)

# Convertir certaines colonnes en facteurs
df_donnees$Etiquette_GES <- as.factor(df_donnees$Etiquette_GES)
df_donnees$Etiquette_DPE <- as.factor(df_donnees$Etiquette_DPE)
df_donnees$Type_bâtiment <- as.factor(df_donnees$Type_bâtiment)
df_donnees$Type_énergie_principale_chauffage <- as.factor(df_donnees$Type_énergie_principale_chauffage)

# Chargement et filtrage des données géographiques
df_donnees_geo <- read.csv("merged_donnee.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
df_donnees_geo$lon <- as.numeric(gsub(",", ".", df_donnees_geo$lon))
df_donnees_geo$lat <- as.numeric(gsub(",", ".", df_donnees_geo$lat))
df_donnees_filtered <- df_donnees_geo[!is.na(df_donnees_geo$lon) & !is.na(df_donnees_geo$lat) &
                                        df_donnees_geo$lon >= -180 & df_donnees_geo$lon <= 180 &
                                        df_donnees_geo$lat >= -90 & df_donnees_geo$lat <= 90, ]
df_donnees_filtered <- head(df_donnees_filtered, 5000)

# Palette de couleurs pour les étiquettes DPE
palette_DPE <- colorFactor(
  palette = c("#2ECC40", "#9ACD32", "#FFFF00", "#FFD700", "#FFA500", "#FF4500", "#FF0000"),
  domain = df_donnees_filtered$Etiquette_DPE
)

# Fonction pour filtrer les choix de variables
filtered_choices <- function() {
  variables_to_hide <- c("x", "y", "identifiant_ban", "lon", "lat", "X_score")
  setdiff(names(df_donnees), variables_to_hide)
}

# Fonction générique pour le téléchargement des données
downloadDataHandler <- function(data, filename_prefix) {
  downloadHandler(
    filename = function() {
      paste0(filename_prefix, "-", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
}

# Fonction générique pour le téléchargement des graphiques
downloadPlotHandler <- function(plot_func, filename_prefix) {
  downloadHandler(
    filename = function() {
      paste0(filename_prefix, "-", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_func(), device = "png", width = 10, height = 6)
    }
  )
}

# UI pour l'application Shiny
ui <- fluidPage(
  useShinyjs(),
  
  # CSS pour le sélecteur de thème et autres éléments
  tags$head(tags$style(HTML("
    .theme-selector {
      position: fixed;
      bottom: 10px;
      left: 10px;
      z-index: 1000;
      background-color: white;
      padding: 10px;
      border-radius: 5px;
      box-shadow: 0 2px 10px rgba(0, 0, 0, 0.2);
    }
    .login-panel {
      margin-top: 100px;
      text-align: center;
      background-color: #f9f9f9;
      padding: 30px;
      border-radius: 10px;
      box-shadow: 0px 0px 20px rgba(0, 0, 0, 0.1);
      max-width: 400px;
      margin-left: auto;
      margin-right: auto;
    }
    .login-title {
      margin-bottom: 20px;
      font-size: 24px;
      font-weight: bold;
    }
    .info-tooltip {
      cursor: pointer;
      color: #007bff;
      text-decoration: underline;
    }
    .context-image {
      max-width: 100%;
      height: auto;
      margin-bottom: 20px;
    }
    .data-table-container {
      border: 1px solid #ddd;
      border-radius: 5px;
      padding: 20px;
      background-color: #f9f9f9;
      box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      position: relative;
    }
    .background-image {
      position: absolute;
      top: 0;
      left: 0;
      width: 100%;
      height: 100%;
      z-index: -1;
      opacity: 0.1;
    }
    /* Votre CSS personnalisé ici */
    body {
      font-family: 'Arial', sans-serif;
      background-color: #f0f0f0;
    }
    .navbar {
      background-color: #2c3e50;
    }
    .navbar-default .navbar-nav > li > a {
      color: #ecf0f1;
    }
    .box {
      border-radius: 0;
      box-shadow: 0 2px 5px rgba(0,0,0,0.1);
    }
    /* Ajoutez d'autres styles selon vos besoins */
  ")),
            tags$link(rel = "stylesheet", type = "text/css", href = "https://cdn.datatables.net/1.10.24/css/jquery.dataTables.min.css"),
            tags$script(src = "https://cdn.datatables.net/1.10.24/js/jquery.dataTables.min.js"),
            tags$script(src = "https://cdn.datatables.net/buttons/1.7.0/js/dataTables.buttons.min.js"),
            tags$script(src = "https://cdn.datatables.net/buttons/1.7.0/js/buttons.html5.min.js"),
            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/jszip/3.1.3/jszip.min.js"),
            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.53/pdfmake.min.js"),
            tags$script(src = "https://cdnjs.cloudflare.com/ajax/libs/pdfmake/0.1.53/vfs_fonts.js"),
            tags$script(src = "https://cdn.datatables.net/buttons/1.7.0/js/buttons.print.min.js"),
            tags$script(src = "https://cdn.datatables.net/plug-ins/1.10.24/i18n/French.json")
  ),
  
  # Sélecteur de thème
  div(class = "theme-selector", shinythemes::themeSelector()),
  
  # Panneau de connexion
  uiOutput("loginPanel"),
  
  # Contenu principal du dashboard
  uiOutput("mainUI")
)

# Server
server <- function(input, output, session) {
  
  # Stockage de l'état de la connexion
  user_logged_in <- reactiveVal(FALSE)#Changer d'opérateur booléen pr se connecter directement
  
  # Identifiants d'accès
  valid_user <- "User01"
  valid_password <- "Password01"
  
  # Fonction de connexion
  observeEvent(input$login, {
    if (input$username == valid_user && input$password == valid_password) {
      user_logged_in(TRUE)
      shinyjs::hide("loginPanel")  # Cacher le panneau de connexion
    } else {
      shinyjs::alert("Identifiants invalides !")
    }
  })
  
  # Afficher le panneau de connexion si l'utilisateur n'est pas connecté
  output$loginPanel <- renderUI({
    if (!user_logged_in()) {
      fluidRow(
        column(12,
               div(class = "login-panel",
                   h2(class = "login-title", "Connexion"),
                   textInput("username", "Nom d'utilisateur"),
                   passwordInput("password", "Mot de passe"),
                   actionButton("login", "Se connecter"),
                   tags$br(),
                   span(class = "info-tooltip", title = paste("Identifiants: ", valid_user, "/", valid_password), 
                        "Informations d'identification")
               )
        )
      )
    }
  })
  
  # Contenu principal de l'application après connexion
  output$mainUI <- renderUI({
    req(user_logged_in())  # Nécessite que l'utilisateur soit connecté
    navbarPage(
      title = "Données DPE Dashboard",
      id = "nav",
      tabPanel("Accueil", 
               fluidRow(
                 column(8,
                        box(
                          title = "Bienvenue sur le Dashboard DPE",
                          status = "primary",
                          solidHeader = TRUE,
                          width = NULL,
                          p("Ce dashboard interactif vous permet d'explorer les données du Diagnostic de Performance Énergétique (DPE) des logements dans l'Allier."),
                          p("Utilisez les différents onglets pour visualiser et analyser les données sous différents angles :"),
                          tags$ul(
                            tags$li("Nuage de points : Explorez les relations entre différentes variables"),
                            tags$li("Boîte à moustaches : Comparez la distribution des variables"),
                            tags$li("Histogrammes : Visualisez la distribution des variables"),
                            tags$li("Carte : Explorez la répartition géographique des DPE"),
                            tags$li("Graphiques personnalisés : Créez vos propres visualisations")
                          ),
                          p("Pour commencer, sélectionnez un onglet dans le menu de gauche.")
                        )
                 ),
                 column(4,
                        box(
                          title = "Département de l'Allier",
                          status = "primary",
                          solidHeader = TRUE,
                          width = NULL,
                          img(src = "allier.png", width = "100%", alt = "Carte de l'Allier")
                        )
                 )
               ),
               fluidRow(
                 column(6,
                        valueBoxOutput("total_logements", width = NULL)
                 ),
                 column(6,
                        valueBoxOutput("moyenne_consommation", width = NULL)
                 )
               ),
               fluidRow(
                 column(12,
                        box(
                          
                          status = "primary",
                          solidHeader = FALSE,
                          width = NULL,
                          img(src = "sioule.jpeg", width = "100%", alt = "Photo sioule")
                        )
                 )
               )
      ),
      tabPanel("Contexte", 
               h2("Contexte du Projet"),
               img(src = "Enedis.png", class = "context-image", width = "500px", height = "auto"),
               p("Les données DPE fournissent des informations sur la consommation énergétique et les émissions de gaz à effet de serre des bâtiments."),
               p("Ces informations proviennent de ENEDIS, qui gère la distribution d'électricité en France. La performance énergétique des bâtiments est essentielle pour la transition énergétique."),
               p("Cette application vise à aider à comprendre et analyser ces données."),
               
               # Ajout de la table de données ici
               h3("Table des Données"),
               div(class = "data-table-container",
                   DTOutput("dataTable"),
                   
               )
      ),
      tabPanel("Dashboard", 
               dashboardPage(
                 dashboardHeader(title = "DPE Dashboard"),
                 dashboardSidebar(
                   sidebarMenu(
                     menuItem("Accueil", tabName = "home", icon = icon("home")),
                     menuItem("Nuage de points", tabName = "scatter", icon = icon("chart-area")),
                     menuItem("Boîte à moustaches", tabName = "boxplot", icon = icon("stats", lib = "glyphicon")),
                     menuItem("Histogrammes", tabName = "histogram", icon = icon("bar-chart")),
                     menuItem("Carte", tabName = "map", icon = icon("map")),
                     menuItem("Graphiques personnalisés", tabName = "custom_graphs", icon = icon("chart-line"))
                   )
                 ),
                 dashboardBody(
                   tags$head(tags$style(HTML('
              .content-wrapper, .right-side {
                background-color: #f4f4f4;
              }
              .box {
                border-top: 3px solid #3c8dbc;
              }
            '))),
                   tabItems(
                     tabItem(tabName = "home",
                             fluidRow(
                               valueBoxOutput("total_logements"),
                               valueBoxOutput("avg_surface"),
                               valueBoxOutput("count_GES_A"),
                               valueBoxOutput("count_GES_B")
                             ),
                             fluidRow(
                               box(
                                 title = "Répartition des étiquettes DPE",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 plotOutput("dpe_distribution")
                                 
                                 
                               ),
                               box(
                                 title = "Évolution des DPE par année",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 plotOutput("dpe_evolution")
                                 
                                 
                               )
                             )
                     ),
                     tabItem(tabName = "scatter",
                             fluidRow(
                               column(width = 9,
                                      box(width = NULL, status = "primary",
                                          title = "Nuage de points",
                                          selectInput("x_var_scatter", "Variable X:", choices = names(df_donnees)),
                                          selectInput("y_var_scatter", "Variable Y:", choices = names(df_donnees)),
                                          selectInput("color_var_scatter", "Couleur:", choices = c("Aucune", names(df_donnees))),
                                          actionButton("suggest_scatter", "Suggérer des variables"),
                                          plotOutput("scatterPlot"),
                                          downloadButton("downloadScatterPlot", "Télécharger le graphique"),
                                          downloadButton("downloadScatterData", "Télécharger les données")
                                          
                                      )
                               ),
                               column(width = 3,
                                      valueBoxOutput("r_squared_scatter", width = NULL),
                                      valueBoxOutput("correlation_scatter", width = NULL)
                               )
                             )
                     ),
                     tabItem(tabName = "boxplot",
                             fluidRow(
                               column(width = 9,
                                      box(width = NULL, status = "primary",
                                          title = "Boîte à moustaches",
                                          selectInput("x_var_box", "Variable X:", choices = names(df_donnees)),
                                          selectInput("y_var_box", "Variable Y:", choices = names(df_donnees)),
                                          selectInput("color_var_box", "Couleur:", choices = c("Aucune", names(df_donnees))),
                                          actionButton("suggest_boxplot", "Suggérer des variables"),
                                          plotOutput("boxPlot"),
                                          downloadButton("downloadBoxPlot", "Télécharger le graphique"),
                                          downloadButton("downloadBoxData", "Télécharger les données")
                                      )
                               ),
                               column(width = 3,
                                      valueBoxOutput("median_box", width = NULL),
                                      valueBoxOutput("iqr_box", width = NULL)
                               )
                             )
                     ),
                     tabItem(tabName = "histogram",
                             fluidRow(
                               column(width = 6,
                                      box(width = NULL,
                                          title = "Histogramme 1",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          selectInput("histo1_type", "Type d'histogramme:", 
                                                      choices = c("Distribution" = "dist", "Fréquence" = "freq")),
                                          selectInput("histo1_var", "Variable:", choices = names(df_donnees)),
                                          plotlyOutput("histoPlot1"),
                                          
                                          downloadButton("downloadHistoPlot1Data", "Télécharger les données"),
                                          downloadButton("downloadHistoPlot1", "Télécharger le graphique (PNG)")
                                      )
                               ),
                               column(width = 6,
                                      box(width = NULL,
                                          title = "Histogramme 2",
                                          status = "primary",
                                          solidHeader = TRUE,
                                          selectInput("histo2_type", "Type d'histogramme:", 
                                                      choices = c("Distribution" = "dist", "Fréquence" = "freq")),
                                          selectInput("histo2_var", "Variable:", choices = names(df_donnees)),
                                          plotlyOutput("histoPlot2"),
                                          
                                          downloadButton("downloadHistoPlot2Data", "Télécharger les données"),
                                          downloadButton("downloadHistoPlot2", "Télécharger le graphique (PNG)")
                                      )
                               )
                             )
                     ),
                     tabItem(tabName = "map",
                             fluidRow(
                               box(width = 12, status = "primary",
                                   title = "Carte des DPE",
                                   leafletOutput("map", height = 600),
                                   absolutePanel(top = 10, right = 10,
                                                 selectInput("map_color", "Colorer par:", 
                                                             choices = c("Etiquette_DPE", "Etiquette_GES", "Type_bâtiment")),
                                                 checkboxInput("cluster_map", "Grouper les marqueurs", value = FALSE),
                                                 downloadButton("downloadMap", "Télécharger la carte")
                                   )
                               )
                             )
                     ),
                     tabItem(tabName = "custom_graphs",
                             fluidRow(
                               box(
                                 title = "Créer un graphique personnalisé",
                                 status = "primary",
                                 solidHeader = TRUE,
                                 width = 12,
                                 selectInput("graph_type", "Type de graphique:",
                                             choices = c("Secteur" = "pie", "Histogramme" = "histogram", 
                                                         "Boîte à moustaches" = "boxplot", 
                                                         "Nuage de points" = "scatter")),
                                 uiOutput("dynamic_inputs"),
                                 dateRangeInput("date_range", "Période:", start = min(df_donnees$Date_établissement_DPE), end = max(df_donnees$Date_établissement_DPE)),
                                 actionButton("add_graph", "Ajouter un graphique"),
                                 actionButton("reset_graphs", "Réinitialiser tous les graphiques")
                               )
                             ),
                             uiOutput("custom_graphs")
                     )
                   )
                 )
               )
      )
    )
  })
  
  # KPI
  output$total_logements <- renderValueBox({
    valueBox(
      value = nrow(df_donnees),
      subtitle = "Total de logements",
      icon = icon("home"),
      color = "blue"
    )
  })
  
  output$avg_surface <- renderValueBox({
    valueBox(
      value = round(mean(df_donnees$Surface_habitable_logement, na.rm = TRUE), 2),
      subtitle = "Surface habitable moyenne (m²)",
      icon = icon("ruler"),
      color = "teal"
    )
  })
  
  output$count_GES_A <- renderValueBox({
    valueBox(
      value = sum(df_donnees$Etiquette_DPE == "A"),
      subtitle = "Logements avec DPE A",
      icon = icon("leaf"),
      color = "green"
    )
  })
  
  output$count_GES_B <- renderValueBox({
    valueBox(
      value = sum(df_donnees$Etiquette_DPE == "B"),
      subtitle = "Logements avec DPE B",
      icon = icon("leaf"),
      color = "orange"
    )
  })
  
  # Graphique GES
  output$graph_ges <- renderPlot({
    req(input$ges_filter)
    ggplot(df_donnees[df_donnees$Etiquette_GES == input$ges_filter, ], aes(x = Année_construction)) +
      geom_bar(fill = "steelblue") +
      labs(title = "Répartition des logements par année de construction", x = "Année de construction", y = "Nombre de logements") +
      theme_minimal()
  })
  
  # Table de données réactive
  output$dataTable <- renderDT({
    datatable(
      head(df_donnees, 100), # Limite à 100 lignes pour l'exemple
      options = list(
        scrollX = TRUE, 
        scrollY = "400px", # Hauteur fixe avec défilement vertical
        pageLength = 10,   # 10 lignes par page
        dom = 'Bfrtip',    # Ajoute des boutons pour l'export
        buttons = list(
          list(extend = 'copy', text = 'Copier'),
          list(extend = 'csv', text = 'CSV'),
          list(extend = 'excel', text = 'Excel'),
          list(extend = 'pdf', text = 'PDF'),
          list(extend = 'print', text = 'Imprimer')
        ),
        language = list(
          url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/French.json'
        )
      ),
      extensions = 'Buttons',
      class = 'cell-border stripe hover'
    )
  })
  
  # Graphique de nuage de points avec régression linéaire
  output$scatterPlot <- renderPlot({
    req(input$x_var, input$y_var)
    ggplot(df_donnees, aes_string(x = input$x_var, y = input$y_var)) +
      geom_point() +
      geom_smooth(method = "lm", col = "red") +
      labs(x = input$x_var, y = input$y_var) +
      ggtitle(paste("Nuage de points de", input$x_var, "et", input$y_var)) +
      theme_minimal()
  })
  
  # Fonction pour télécharger le graphique en PNG
  output$downloadPlot <- downloadHandler(
    filename = function() {
      paste("scatterplot-", input$x_var, "-", input$y_var, ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(
        ggplot(df_donnees, aes_string(x = input$x_var, y = input$y_var)) +
          geom_point() +
          geom_smooth(method = "lm", col = "red") +
          labs(x = input$x_var, y = input$y_var) +
          ggtitle(paste("Nuage de points de", input$x_var, "et", input$y_var)) +
          theme_minimal()
      )
      dev.off()
    }
  )
  
  # Carte géographique
  output$map <- renderLeaflet({
    if (nrow(df_donnees_filtered) > 0) {
      leaflet(df_donnees_filtered) %>%
        addTiles() %>%
        addCircleMarkers(~lon, ~lat, 
                         color = ~palette_DPE(Etiquette_DPE),  # Appliquer la couleur en fonction de DPE
                         popup = ~paste(
                           "<strong>Type de bâtiment :</strong>", Type_bâtiment, "<br>",
                           "<strong>Étiquette DPE :</strong>", Etiquette_DPE, "<br>",
                           "<strong>Étiquette GES :</strong>", Etiquette_GES, "<br>",
                           "<strong>Année de construction :</strong>", Année_construction, "<br>",
                           "<strong>Surface habitable :</strong>", Surface_habitable_logement, "m²<br>",
                           "<strong>Énergie principale de chauffage :</strong>", Type_énergie_principale_chauffage
                         ),
                         radius = 5, fillOpacity = 0.7) %>%
        addLegend("bottomright", pal = palette_DPE, values = ~Etiquette_DPE, 
                  title = "Étiquette DPE", opacity = 1)
    } else {
      leaflet() %>%
        addTiles() %>%
        addPopups(lng = 2.349014, lat = 48.864716, popup = "Aucun point géographique valide trouvé.")
    }
  })
  
  # Histogramme
  output$histogramme <- renderPlot({
    ggplot(df_donnees, aes(x = Surface_habitable_logement)) +
      geom_histogram(binwidth = 10, fill = "steelblue", color = "white") +
      labs(title = "Distribution des surfaces habitables", x = "Surface habitable (m²)", y = "Fréquence")
  })
  
  # Boîte à moustaches
  output$boxplot <- renderPlot({
    ggplot(df_donnees, aes(x = Etiquette_DPE, y = Surface_habitable_logement)) +
      geom_boxplot(fill = "lightgreen") +
      labs(title = "Surface habitable par étiquette DPE", x = "Étiquette DPE", y = "Surface habitable (m²)")
  })
  
  # Calcul de corrélation et régression linéaire
  output$correlation <- renderText({
    req(input$x_var, input$y_var)
    x <- df_donnees[[input$x_var]]
    y <- df_donnees[[input$y_var]]
    cor_value <- cor(x, y, use = "complete.obs")
    paste("Coefficient de corrélation entre", input$x_var, "et", input$y_var, ":", round(cor_value, 3))
  })
  
  # Bouton d'exportation des données
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(df_donnees, file)
    }
  )
  
  # Fonction pour filtrer les données
  filtered_data <- reactive({
    data <- df_donnees
    if (input$color_var_scatter != "Aucune") {
      data <- data[!is.na(data[[input$color_var_scatter]]), ]
    }
    data
  })
  
  # Nuage de points
  output$scatterPlot <- renderPlot({
    req(input$x_var_scatter, input$y_var_scatter)
    p <- ggplot(filtered_data(), aes_string(x = input$x_var_scatter, y = input$y_var_scatter)) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      theme_minimal() +
      labs(title = paste("Nuage de points:", input$x_var_scatter, "vs", input$y_var_scatter))
    
    if (input$color_var_scatter != "Aucune") {
      p <- p + aes_string(color = input$color_var_scatter)
    }
    p
  })
  
  # Calcul R² et corrélation pour le nuage de points
  output$r_squared_scatter <- renderValueBox({
    req(input$x_var_scatter, input$y_var_scatter)
    model <- lm(as.formula(paste(input$y_var_scatter, "~", input$x_var_scatter)), data = filtered_data())
    r_squared <- summary(model)$r.squared
    valueBox(
      value = round(r_squared, 3),
      subtitle = "R²",
      icon = icon("calculator"),
      color = "blue"
    )
  })
  
  output$correlation_scatter <- renderValueBox({
    req(input$x_var_scatter, input$y_var_scatter)
    correlation <- cor(filtered_data()[[input$x_var_scatter]], filtered_data()[[input$y_var_scatter]], use = "complete.obs")
    valueBox(
      value = round(correlation, 3),
      subtitle = "Corrélation",
      icon = icon("chart-line"),
      color = "purple"
    )
  })
  
  # Boîte à moustaches
  output$boxPlot <- renderPlot({
    req(input$x_var_box, input$y_var_box)
    p <- ggplot(filtered_data(), aes_string(x = input$x_var_box, y = input$y_var_box)) +
      geom_boxplot() +
      theme_minimal() +
      labs(title = paste("Boîte à moustaches:", input$y_var_box, "par", input$x_var_box))
    
    if (input$color_var_box != "Aucune") {
      p <- p + aes_string(fill = input$color_var_box)
    }
    p
  })
  
  # Calcul médiane et IQR pour la boîte à moustaches
  output$median_box <- renderValueBox({
    req(input$y_var_box)
    median_value <- median(filtered_data()[[input$y_var_box]], na.rm = TRUE)
    valueBox(
      value = round(median_value, 2),
      subtitle = "Médiane",
      icon = icon("sort-numeric-down"),
      color = "green"
    )
  })
  
  output$iqr_box <- renderValueBox({
    req(input$y_var_box)
    iqr_value <- IQR(filtered_data()[[input$y_var_box]], na.rm = TRUE)
    valueBox(
      value = round(iqr_value, 2),
      subtitle = "IQR",
      icon = icon("arrows-alt-v"),
      color = "yellow"
    )
  })
  
  # Carte améliorée
  output$map <- renderLeaflet({
    req(input$map_color)
    
    if (input$map_color == "Etiquette_DPE") {
      pal <- colorFactor(palette = palette_DPE, domain = df_donnees_filtered$Etiquette_DPE)
    } else if (input$map_color == "Etiquette_GES") {
      pal <- colorFactor(palette = "viridis", domain = df_donnees_filtered$Etiquette_GES)
    } else {
      pal <- colorFactor(palette = "Set3", domain = df_donnees_filtered$Type_bâtiment)
    }
    
    map <- leaflet(df_donnees_filtered) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.Positron)
    
    if (input$cluster_map) {
      map <- map %>% addMarkers(
        ~lon, ~lat,
        popup = ~paste(
          "<strong>Type de bâtiment :</strong>", Type_bâtiment, "<br>",
          "<strong>Étiquette DPE :</strong>", Etiquette_DPE, "<br>",
          "<strong>Étiquette GES :</strong>", Etiquette_GES, "<br>",
          "<strong>Année de construction :</strong>", Année_construction, "<br>",
          "<strong>Surface habitable :</strong>", Surface_habitable_logement, "m²<br>",
          "<strong>Énergie principale de chauffage :</strong>", Type_énergie_principale_chauffage
        ),
        clusterOptions = markerClusterOptions()
      )
    } else {
      map <- map %>% addCircleMarkers(
        ~lon, ~lat,
        color = ~pal(df_donnees_filtered[[input$map_color]]),
        popup = ~paste(
          "<strong>Type de bâtiment :</strong>", Type_bâtiment, "<br>",
          "<strong>Étiquette DPE :</strong>", Etiquette_DPE, "<br>",
          "<strong>Étiquette GES :</strong>", Etiquette_GES, "<br>",
          "<strong>Année de construction :</strong>", Année_construction, "<br>",
          "<strong>Surface habitable :</strong>", Surface_habitable_logement, "m²<br>",
          "<strong>Énergie principale de chauffage :</strong>", Type_énergie_principale_chauffage
        ),
        radius = 5,
        fillOpacity = 0.7
      )
    }
    
    map %>% addLegend("bottomright", pal = pal, values = df_donnees_filtered[[input$map_color]], 
                      title = input$map_color, opacity = 1)
  })
  
  # Fonction pour télécharger le graphique en PNG
  output$downloadScatterPlot <- downloadHandler(
    filename = function() {
      paste("scatterplot-", input$x_var_scatter, "-", input$y_var_scatter, ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(
        ggplot(filtered_data(), aes_string(x = input$x_var_scatter, y = input$y_var_scatter)) +
          geom_point() +
          geom_smooth(method = "lm", se = FALSE, color = "red") +
          theme_minimal() +
          labs(title = paste("Nuage de points:", input$x_var_scatter, "vs", input$y_var_scatter),
               x = input$x_var_scatter, 
               y = input$y_var_scatter)
      )
      dev.off()
    }
  )
  
  # Téléchargement de la boîte à moustaches
  output$downloadBoxPlot <- downloadHandler(
    filename = function() {
      paste("boxplot-", input$x_var_box, "-", input$y_var_box, ".png", sep = "")
    },
    content = function(file) {
      png(file)
      print(
        ggplot(filtered_data(), aes_string(x = input$x_var_box, y = input$y_var_box)) +
          geom_boxplot() +
          theme_minimal() +
          labs(title = paste("Boîte à moustaches:", input$y_var_box, "par", input$x_var_box),
               x = input$x_var_box,
               y = input$y_var_box)
      )
      dev.off()
    }
  )
  # Téléchargement de l'histogramme 1
  output$downloadHistoPlot1 <- downloadHandler(
    filename = function() {
      paste("histogramme1-", input$histo1_var, ".png", sep = "")
    },
    content = function(file) {
      png(file)
      if(input$histo1_type == "dist" && is.numeric(df_donnees[[input$histo1_var]])) {
        print(
          ggplot(df_donnees, aes_string(x = input$histo1_var)) +
            geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "black") +
            geom_density(color = "red") +
            theme_minimal() +
            labs(title = paste("Distribution de", input$histo1_var), x = input$histo1_var, y = "Densité")
        )
      } else {
        print(
          ggplot(df_donnees, aes_string(x = input$histo1_var)) +
            geom_bar(fill = "steelblue", color = "black") +
            theme_minimal() +
            labs(title = paste("Fréquence de", input$histo1_var), x = input$histo1_var, y = "Fréquence")
        )
      }
      dev.off()
    }
  )
  
  # Téléchargement des données
  output$downloadScatterData <- downloadHandler(
    filename = function() { paste("scatter_data-", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  
  output$downloadBoxData <- downloadHandler(
    filename = function() { paste("box_data-", Sys.Date(), ".csv", sep="") },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
  # Téléchargement de l'histogramme 2
  output$downloadHistoPlot2 <- downloadHandler(
    filename = function() {
      paste("histogramme2-", input$histo2_var, ".png", sep = "")
    },
    content = function(file) {
      png(file)
      if(input$histo2_type == "dist" && is.numeric(df_donnees[[input$histo2_var]])) {
        print(
          ggplot(df_donnees, aes_string(x = input$histo2_var)) +
            geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "black") +
            geom_density(color = "red") +
            theme_minimal() +
            labs(title = paste("Distribution de", input$histo2_var), x = input$histo2_var, y = "Densité")
        )
      } else {
        print(
          ggplot(df_donnees, aes_string(x = input$histo2_var)) +
            geom_bar(fill = "steelblue", color = "black") +
            theme_minimal() +
            labs(title = paste("Fréquence de", input$histo2_var), x = input$histo2_var, y = "Fréquence")
        )
      }
      dev.off()
    }
  )
  
  # Téléchargement de la carte
  output$downloadMap <- downloadHandler(
    filename = function() {
      paste("carte-DPE-", Sys.Date(), ".png", sep = "")
    },
    content = function(file) {
      png(file, width = 800, height = 600)
      print(leaflet(df_donnees_filtered) %>%
              addTiles() %>%
              addCircleMarkers(~lon, ~lat, 
                               color = ~palette_DPE(Etiquette_DPE),
                               radius = 5, fillOpacity = 0.7) %>%
              addLegend("bottomright", pal = palette_DPE, values = ~Etiquette_DPE, 
                        title = "Étiquette DPE", opacity = 1))
      dev.off()
    }
  )  
  # Palette de couleurs officielle pour les étiquettes DPE
  palette_DPE <- c("A" = "#319834", "B" = "#33cc31", "C" = "#fff200", 
                   "D" = "#ff9a00", "E" = "#ff0000", "F" = "#bb0000", "G" = "#770000")
  
  # Graphiques pour la page d'accueil
  output$dpe_distribution <- renderPlot({
    ggplot(df_donnees, aes(x = Etiquette_DPE, fill = Etiquette_DPE)) +
      geom_bar() +
      scale_fill_manual(values = palette_DPE) +
      theme_minimal() +
      labs(title = "Répartition des étiquettes DPE", x = "Étiquette DPE", y = "Nombre de logements")
  })
  
  output$dpe_evolution <- renderPlot({
    df_donnees %>%
      mutate(Année = as.numeric(format(as.Date(Date_établissement_DPE), "%Y"))) %>%
      filter(!is.na(Année)) %>%
      group_by(Année, Etiquette_DPE) %>%
      summarise(Nombre = n(), .groups = 'drop') %>%
      ggplot(aes(x = Année, y = Nombre, color = Etiquette_DPE)) +
      geom_line() +
      scale_color_manual(values = palette_DPE) +
      theme_minimal() +
      labs(title = "Évolution des DPE par année", x = "Année", y = "Nombre de DPE") +
      scale_x_continuous(breaks = function(x) seq(ceiling(x[1]), floor(x[2]), by = 1))
  })
  
  # Histogrammes
  output$histoPlot1 <- renderPlotly({
    req(input$histo1_var, input$histo1_type)
    
    if(input$histo1_type == "dist" && is.numeric(df_donnees[[input$histo1_var]])) {
      p <- ggplot(df_donnees, aes_string(x = input$histo1_var)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "black") +
        geom_density(color = "red") +
        theme_minimal() +
        labs(title = paste("Distribution de", input$histo1_var), x = input$histo1_var, y = "Densité")
    } else {
      p <- ggplot(df_donnees, aes_string(x = input$histo1_var)) +
        geom_bar(fill = "steelblue", color = "black") +
        theme_minimal() +
        labs(title = paste("Fréquence de", input$histo1_var), x = input$histo1_var, y = "Fréquence")
    }
    
    ggplotly(p) %>% 
      layout(dragmode = "zoom") %>%
      config(displayModeBar = TRUE)
  })
  
  output$histoPlot2 <- renderPlotly({
    req(input$histo2_var, input$histo2_type)
    
    if(input$histo2_type == "dist" && is.numeric(df_donnees[[input$histo2_var]])) {
      p <- ggplot(df_donnees, aes_string(x = input$histo2_var)) +
        geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "black") +
        geom_density(color = "red") +
        theme_minimal() +
        labs(title = paste("Distribution de", input$histo2_var), x = input$histo2_var, y = "Densité")
    } else {
      p <- ggplot(df_donnees, aes_string(x = input$histo2_var)) +
        geom_bar(fill = "steelblue", color = "black") +
        theme_minimal() +
        labs(title = paste("Fréquence de", input$histo2_var), x = input$histo2_var, y = "Fréquence")
    }
    
    ggplotly(p) %>% 
      layout(dragmode = "zoom") %>%
      config(displayModeBar = TRUE)
  })
  
  # Suggestions de variables
  observeEvent(input$suggest_scatter, {
    suggestions <- list(
      list(x = "Surface_habitable_logement", y = "Conso_energie"),
      list(x = "Année_construction", y = "Conso_energie"),
      list(x = "Surface_habitable_logement", y = "Emission_GES")
    )
    suggestion <- sample(suggestions, 1)[[1]]
    updateSelectInput(session, "x_var_scatter", selected = suggestion$x)
    updateSelectInput(session, "y_var_scatter", selected = suggestion$y)
  })
  
  observeEvent(input$suggest_boxplot, {
    suggestions <- list(
      list(x = "Etiquette_DPE", y = "Surface_habitable_logement"),
      list(x = "Type_bâtiment", y = "Conso_energie"),
      list(x = "Etiquette_GES", y = "Emission_GES")
    )
    suggestion <- sample(suggestions, 1)[[1]]
    updateSelectInput(session, "x_var_box", selected = suggestion$x)
    updateSelectInput(session, "y_var_box", selected = suggestion$y)
  })
  
  # Ajout d'une fonction pour vérifier la validité des données
  check_data_validity <- function() {
    if (is.null(df_donnees) || nrow(df_donnees) == 0) {
      showNotification("Les données sont vides ou n'ont pas été chargées correctement.", type = "error")
      return(FALSE)
    }
    return(TRUE)
  }
  
  # Utilisation de la fonction au démarrage du serveur
  observe({
    if (!check_data_validity()) {
      showModal(modalDialog(
        title = "Erreur de données",
        "Les données n'ont pas pu être chargées correctement. Veuillez vérifier votre fichier de données.",
        easyClose = TRUE,
        footer = NULL
      ))
    }
  })
  
  # Liste réactive pour stocker les graphiques personnalisés
  custom_graphs <- reactiveVal(list())
  
  # Inputs dynamiques en fonction du type de graphique sélectionné
  output$dynamic_inputs <- renderUI({
    base_inputs <- switch(input$graph_type,
                          "pie" = list(
                            selectInput("pie_var", "Variable:", choices = names(df_donnees))
                          ),
                          "histogram" = list(
                            selectInput("hist_var", "Variable:", choices = names(df_donnees)),
                            numericInput("hist_bins", "Nombre de barres:", value = 30, min = 1)
                          ),
                          "boxplot" = list(
                            selectInput("box_x", "Variable X:", choices = names(df_donnees)),
                            selectInput("box_y", "Variable Y:", choices = names(df_donnees))
                          ),
                          "scatter" = list(
                            selectInput("scatter_x", "Variable X:", choices = names(df_donnees)),
                            selectInput("scatter_y", "Variable Y:", choices = names(df_donnees))
                          )
    )
    
    if (input$graph_type != "pie") {
      base_inputs <- c(base_inputs, list(
        selectInput("color_var", "Variable pour la couleur:", choices = c("Aucune", names(df_donnees)))
      ))
    }
    
    base_inputs
  })
  
  # Fonction pour créer un graphique en fonction du type sélectionné
  create_graph <- function(type, inputs, date_range) {
    tryCatch({
      filtered_data <- df_donnees %>%
        filter(Date_établissement_DPE >= date_range[1] & Date_établissement_DPE <= date_range[2])
      
      p <- switch(type,
                  "pie" = plot_ly(filtered_data, labels = ~get(inputs$pie_var), type = "pie"),
                  "histogram" = {
                    p <- plot_ly(filtered_data, x = ~get(inputs$hist_var), type = "histogram", nbinsx = inputs$hist_bins)
                    if (inputs$color_var != "Aucune") {
                      p <- p %>% add_histogram(color = ~get(inputs$color_var))
                    }
                    p
                  },
                  "boxplot" = {
                    p <- plot_ly(filtered_data, x = ~get(inputs$box_x), y = ~get(inputs$box_y), type = "box")
                    if (inputs$color_var != "Aucune") {
                      p <- p %>% add_boxplot(color = ~get(inputs$color_var))
                    }
                    p
                  },
                  "scatter" = {
                    p <- plot_ly(filtered_data, x = ~get(inputs$scatter_x), y = ~get(inputs$scatter_y), type = "scatter", mode = "markers")
                    if (inputs$color_var != "Aucune") {
                      p <- p %>% add_markers(color = ~get(inputs$color_var))
                    }
                    p
                  }
      )
      
      p %>% layout(title = paste("Graphique", type))
    }, error = function(e) {
      showNotification(paste("Erreur lors de la création du graphique:", e$message), type = "error")
      return(NULL)
    })
  }
  
  # Ajouter un nouveau graphique
  observeEvent(input$add_graph, {
    new_graph <- create_graph(input$graph_type, reactiveValuesToList(input), input$date_range)
    if (!is.null(new_graph)) {
      current_graphs <- custom_graphs()
      custom_graphs(c(current_graphs, list(new_graph)))
    }
  })
  
  # Réinitialiser tous les graphiques
  observeEvent(input$reset_graphs, {
    custom_graphs(list())
  })
  
  # Afficher les graphiques personnalisés avec disposition optimisée
  output$custom_graphs <- renderUI({
    graphs <- custom_graphs()
    
    if (length(graphs) == 0) {
      return(NULL)
    }
    
    # Calculer le nombre optimal de colonnes
    n_graphs <- length(graphs)
    n_cols <- ifelse(n_graphs <= 3, n_graphs, ceiling(sqrt(n_graphs)))
    
    plot_output_list <- lapply(seq_along(graphs), function(i) {
      plotlyOutput(paste0("custom_plot_", i), height = 400)
    })
    
    # Diviser les graphiques en lignes selon le nombre de colonnes
    rows <- split(plot_output_list, ceiling(seq_along(plot_output_list) / n_cols))
    
    # Créer l'UI avec des lignes et colonnes optimisées
    do.call(tagList, lapply(rows, function(row) {
      fluidRow(
        lapply(row, function(plot) {
          column(width = 12 / length(row), plot)
        })
      )
    }))
  })
  
  # Rendre les graphiques personnalisés
  observe({
    graphs <- custom_graphs()
    for (i in seq_along(graphs)) {
      local({
        local_i <- i
        output[[paste0("custom_plot_", local_i)]] <- renderPlotly({
          graphs[[local_i]]
        })
      })
    }
  })
  
  # Réagir aux changements de disposition
  observeEvent(input$graph_layout, {
    # Forcer la mise à jour de l'UI des graphiques personnalisés
    output$custom_graphs <- renderUI({
      graphs <- custom_graphs()
      layout <- input$graph_layout
      
      if (length(graphs) == 0) {
        return(NULL)
      }
      
      plot_output_list <- lapply(seq_along(graphs), function(i) {
        plotlyOutput(paste0("custom_plot_", i), height = 400)
      })
      
      if (layout == "grid") {
        n_cols <- ceiling(sqrt(length(graphs)))
        rows <- split(plot_output_list, ceiling(seq_along(plot_output_list) / n_cols))
        do.call(tagList, lapply(rows, function(row) {
          fluidRow(
            lapply(row, function(plot) {
              column(width = 12 / length(row), plot)
            })
          )
        }))
      } else if (layout == "horizontal") {
        fluidRow(
          lapply(plot_output_list, function(plot) {
            column(width = 12 / length(plot_output_list), plot)
          })
        )
      } else {  # vertical
        lapply(plot_output_list, function(plot) {
          fluidRow(column(width = 12, plot))
        })
      }
    })
  })
}

# Lancer l'application
shinyApp(ui = ui, server = server)
