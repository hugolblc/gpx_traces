library(shiny)
library(leaflet)
library(XML)
library(tidyverse)
library(sf)
library(ggplot2)
library(plotly)
library(shinythemes)

# Interface utilisateur
ui <- fluidPage(
   theme = shinytheme("flatly"),
   titlePanel("Visualisation des traces GPX par localité"),
   sidebarLayout(
      sidebarPanel(
         selectInput("localite", "Choisir une localité", choices = NULL, width = "100%"),
         selectInput("activite", "Choisir une activité", choices = NULL, width = "100%"),
         uiOutput("trace_ui"),
         downloadButton("export", "Exporter la trace sélectionnée", class = "btn-primary")
      ),
      mainPanel(
         tabsetPanel(
            tabPanel("Carte", 
                     leafletOutput("map", height = "500px"),
                     plotlyOutput("profil_altitude", height = "300px")),
            tabPanel("Statistiques", verbatimTextOutput("statistiques"))
         )
      )
   )
)

# Logique du serveur
server <- function(input, output, session) {
   
   # Liste des localités disponibles
   localites <- list.dirs("traces_gpx", recursive = FALSE, full.names = FALSE)
   updateSelectInput(session, "localite", choices = localites)
   
   observeEvent(input$localite,{
      # Liste des localités disponibles
      activites <- list.dirs(file.path("traces_gpx",input$localite), recursive = FALSE, full.names = FALSE)
      updateSelectInput(session, "activite", choices = activites)
   })

   
   # Liste des traces disponibles pour la localité sélectionnée
   traces_disponibles <- reactive({
      req(input$activite)
      list.files(paste0("traces_gpx/", input$localite,'/',input$activite), pattern = "\\.gpx$", full.names = TRUE)
   })
   
   # Mise à jour de la liste des traces
   output$trace_ui <- renderUI({
      req(traces_disponibles())
      selectInput("trace", "Choisir une trace", choices = basename(traces_disponibles()), width = "100%")
   })
   
   # Lecture de toutes les traces de la localité sélectionnée
   toutes_les_traces <- reactive({
      req(input$activite)
      paths <- list.files(paste0("traces_gpx/", input$localite,'/', input$activite), pattern = "\\.gpx$", full.names = TRUE)
      traces <- lapply(paths, function(path) {
         if (file.size(path) == 0) {
            showNotification(paste("Le fichier", basename(path), "est vide."), type = "error")
            return(NULL)
         }
         gpx_parsed <- tryCatch({
            htmlTreeParse(file = path, useInternalNodes = TRUE)
         }, error = function(e) {
            showNotification(paste("Erreur lors de la lecture du fichier", basename(path)), type = "error")
            return(NULL)
         })
         if (is.null(gpx_parsed)) return(NULL)
         
         coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
         df <- data.frame(
            lat = as.numeric(coords["lat", ]),
            lon = as.numeric(coords["lon", ])
         )
         sf_points <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
         sf_line <- sf_points %>%
            summarise(do_union = FALSE) %>%
            st_cast("LINESTRING")
         sf_line
      })
      traces <- Filter(Negate(is.null), traces)
      names(traces) <- basename(paths)
      traces
   })
   
   # Lecture de la trace sélectionnée
   trace_selectionnee <- reactive({
      req(input$trace)
      path <- paste0("traces_gpx/", input$localite, "/",input$activite,"/", input$trace)
      
      # Vérifier si le fichier est vide
      if (file.size(path) == 0) {
         showNotification("Le fichier sélectionné est vide.", type = "error")
         return(NULL)
      }
      
      # Lire le fichier GPX
      gpx_parsed <- tryCatch({
         htmlTreeParse(file = path, useInternalNodes = TRUE)
      }, error = function(e) {
         showNotification("Erreur lors de la lecture du fichier GPX.", type = "error")
         return(NULL)
      })
      if (is.null(gpx_parsed)) return(NULL)
      
      # Extraire les coordonnées, l'altitude et le temps
      coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
      elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)
      time <- xpathSApply(doc = gpx_parsed, path = "//trkpt/time", fun = xmlValue)
      
      # Créer un data.frame avec les données extraites
      df <- data.frame(
         lat = as.numeric(coords["lat", ]),
         lon = as.numeric(coords["lon", ]),
         elevation = as.numeric(elevation),
         time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ")
      )
      
      # Calculer la distance cumulée
      df <- df %>%
         mutate(
            dist = c(0, sqrt(diff(lat)^2 + diff(lon)^2))*100,
            dist_cumul = cumsum(dist)
         )
      
      # Convertir en objet sf (points)
      sf_points <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
      
      # Convertir les points en ligne (LINESTRING)
      sf_line <- sf_points %>%
         summarise(do_union = FALSE) %>%
         st_cast("LINESTRING")
      
      # Retourner à la fois les points et la ligne
      list(points = sf_points, line = sf_line)
   })
   
   # Calcul des statistiques
   calcul_statistiques <- reactive({
      req(trace_selectionnee())
      # Extraire les données des points (avec dist_cumul et elevation)
      df <- st_drop_geometry(trace_selectionnee()$points)

      # Calculer le dénivelé positif et négatif
      denivele_positif <- sum(pmax(0, diff(df$elevation)))
      denivele_negatif <- sum(pmin(0, diff(df$elevation)))

      # Distance totale
      distance_totale <- max(df$dist_cumul)   # Convertir en kilomètres

      # Retourner les statistiques
      list(
         denivele_positif = denivele_positif,
         denivele_negatif = denivele_negatif,
         distance_totale = distance_totale
      )
   })
   
   # Affichage des statistiques
   output$statistiques <- renderPrint({
      stats <- calcul_statistiques()
      
      dist <- sqrt((stats$distance_totale^2) + (sum(stats$denivele_positif + stats$denivele_negatif))^2 ) 
      cat("Dénivelé positif cumulé : ", round(stats$denivele_positif, 2), " m\n")
      cat("Dénivelé négatif cumulé : ", round(stats$denivele_negatif, 2), " m\n")
      cat("Distance totale : ", round(dist, 2), " km\n")
   })
   
   # Affichage de toutes les traces sur la carte Leaflet
   output$map <- renderLeaflet({
      req(toutes_les_traces())
      leaflet() %>%
         addTiles() %>%
         # Ajouter toutes les traces en taille 1 avec des couleurs différentes
         addPolylines(data = do.call(rbind, toutes_les_traces()), color = "black", weight = 3, group = "Toutes les traces") %>%
         # Ajouter la trace sélectionnée en taille 3
         addPolylines(data = trace_selectionnee()$line, color = "red", weight = 10, group = "Trace sélectionnée") %>%
         # Ajouter des contrôles pour afficher/masquer les traces
         addLayersControl(
            overlayGroups = c("Toutes les traces", "Trace sélectionnée"),
            options = layersControlOptions(collapsed = FALSE)
         )
   })
   
   # Affichage du profil d'altitude avec plotly
   output$profil_altitude <- renderPlotly({
      req(trace_selectionnee())
      
      # Extraire les données des points (avec dist_cumul et elevation)
      df <- st_drop_geometry(trace_selectionnee()$points)
      
      p <- ggplot(df, aes(x = dist_cumul, y = elevation)) +
         geom_line(size = 1, color = 'white') +
         labs(title = "Profil d'altitude", x = "Distance horizontale (km)", y = "Altitude (m)") +
         theme_minimal() +
         theme(
            panel.background = element_rect(fill = "black"),
            panel.border = element_rect(colour = "black", fill = NA, linewidth = 1.5),
            panel.grid.major = element_line(colour = "black", linetype = "dotted"),
            panel.grid.minor = element_line(colour = "grey", linetype = "dotted"),
            text = element_text(size = 14, family = 'sans', face = 'bold'),
            strip.background = element_rect(color = "black", fill = "white", linewidth = 1, linetype = "solid"),
            legend.position = 'none'
         )
      
      ggplotly(p)
   })
   # Export de la trace sélectionnée
   output$export <- downloadHandler(
      filename = function() {
         input$trace
      },
      content = function(file) {
         write_sf(trace_selectionnee()$line, file, driver = "GPX")
      }
   )
}

# Lancer l'application
shinyApp(ui = ui, server = server)