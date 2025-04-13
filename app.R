library(shiny)
library(leaflet)
library(XML)
library(tidyverse)
library(sf)
library(ggplot2)
library(plotly)
library(shinythemes)

# ----------- Préchargement de toutes les traces GPX -----------
# Lecture des fichiers GPX au démarrage de l'app
chemins_gpx <- list.files("traces_gpx", pattern = "\\.gpx$", recursive = TRUE, full.names = TRUE)

# Fonction pour lire un GPX de manière sécurisée
lire_trace_gpx <- function(path) {
  parts <- strsplit(path, .Platform$file.sep)[[1]]
  localite <- parts[2]
  activite <- parts[3]
  nom_fichier <- basename(path)
  
  if (file.size(path) == 0) return(NULL)
  
  gpx_parsed <- tryCatch({
    htmlTreeParse(file = path, useInternalNodes = TRUE)
  }, error = function(e) return(NULL))
  
  if (is.null(gpx_parsed)) return(NULL)
  
  coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
  if (length(coords) == 0) return(NULL)
  
  df <- data.frame(
    lat = as.numeric(coords["lat", ]),
    lon = as.numeric(coords["lon", ])
  )
  sf_points <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
  sf_line <- sf_points %>%
    summarise(do_union = FALSE) %>%
    st_cast("LINESTRING")
  
  list(
    localite = localite,
    activite = activite,
    nom = nom_fichier,
    path = path,
    ligne = sf_line
  )
}

base_traces <- lapply(chemins_gpx, lire_trace_gpx)
base_traces <- Filter(Negate(is.null), base_traces)

# ----------- UI -----------
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

# ----------- SERVER -----------
server <- function(input, output, session) {
  # Localités uniques
  updateSelectInput(session, "localite", choices = unique(sapply(base_traces, `[[`, "localite")))
  
  observeEvent(input$localite, {
    activites <- unique(sapply(Filter(function(t) t$localite == input$localite, base_traces), `[[`, "activite"))
    updateSelectInput(session, "activite", choices = activites)
  })
  
  traces_filtrees <- reactive({
    req(input$localite, input$activite)
    Filter(function(t) t$localite == input$localite && t$activite == input$activite, base_traces)
  })
  
  output$trace_ui <- renderUI({
    req(traces_filtrees())
    selectInput("trace", "Choisir une trace",
                choices = sapply(traces_filtrees(), function(t) t$nom), width = "100%")
  })
  
  trace_selectionnee <- reactive({
    req(input$trace, traces_filtrees())
    trace <- Filter(function(t) t$nom == input$trace, traces_filtrees())[[1]]
    
    gpx_parsed <- tryCatch({
      htmlTreeParse(file = trace$path, useInternalNodes = TRUE)
    }, error = function(e) return(NULL))
    
    if (is.null(gpx_parsed)) return(NULL)
    
    coords <- xpathSApply(doc = gpx_parsed, path = "//trkpt", fun = xmlAttrs)
    elevation <- xpathSApply(doc = gpx_parsed, path = "//trkpt/ele", fun = xmlValue)
    time <- xpathSApply(doc = gpx_parsed, path = "//trkpt/time", fun = xmlValue)
    
    df <- data.frame(
      lat = as.numeric(coords["lat", ]),
      lon = as.numeric(coords["lon", ]),
      elevation = as.numeric(elevation),
      time = as.POSIXct(time, format = "%Y-%m-%dT%H:%M:%SZ")
    )
    
    df <- df %>%
      mutate(
        dist = c(0, sqrt(diff(lat)^2 + diff(lon)^2)) * 100,
        diff_ele = c(0, sqrt(diff(elevation)^2) / 1000),
        dist_3d = dist + diff_ele,
        dist_cumul = cumsum(dist),
        dist_cumul_3d = cumsum(dist_3d)
      )
    
    sf_points <- st_as_sf(df, coords = c("lon", "lat"), crs = 4326)
    sf_line <- sf_points %>%
      summarise(do_union = FALSE) %>%
      st_cast("LINESTRING")
    
    list(points = sf_points, line = sf_line)
  })
  
  # Carte leaflet
  output$map <- renderLeaflet({
    req(traces_filtrees(), trace_selectionnee())
    
    leaflet() %>%
      addTiles() %>%
      addPolylines(
        data = do.call(rbind, lapply(traces_filtrees(), function(t) t$ligne)),
        color = "black", weight = 3, group = "Toutes les traces"
      ) %>%
      addPolylines(
        data = trace_selectionnee()$line,
        color = "red", weight = 10, group = "Trace sélectionnée"
      ) %>%
      addLayersControl(
        overlayGroups = c("Toutes les traces", "Trace sélectionnée"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # Profil d'altitude
  output$profil_altitude <- renderPlotly({
    req(trace_selectionnee())
    
    df <- st_drop_geometry(trace_selectionnee()$points)
    
    p <- ggplot(df, aes(x = dist_cumul, y = elevation)) +
      geom_line(size = 1, color = 'white') +
      labs(title = "Profil d'altitude", x = "Distance (km)", y = "Altitude (m)") +
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
  
  # Statistiques
  output$statistiques <- renderPrint({
    req(trace_selectionnee())
    df <- st_drop_geometry(trace_selectionnee()$points)
    
    denivele_positif <- sum(pmax(0, diff(df$elevation)))
    denivele_negatif <- sum(pmin(0, diff(df$elevation)))
    distance_totale <- max(df$dist_cumul_3d)
    
    cat("Dénivelé positif cumulé : ", round(denivele_positif, 2), " m\n")
    cat("Dénivelé négatif cumulé : ", round(denivele_negatif, 2), " m\n")
    cat("Distance totale : ", round(distance_totale, 2), " km\n")
  })
  
  # Export
  output$export <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$trace), ".gpx")
    },
    content = function(file) {
      st_write(trace_selectionnee()$line, file, driver = "GPX", delete_dsn = TRUE)
    }
  )
}

# Lancer l'app
shinyApp(ui = ui, server = server)
