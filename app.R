library(shiny)
library(leaflet)
library(sf)
library(ggplot2)
library(plotly)
library(shinythemes)

# ----------- Chargement des fichiers .rds -----------
chemins_rds <- list.files("traces_rds", pattern = "\\.rds$", recursive = TRUE, full.names = TRUE)
base_traces <- lapply(chemins_rds, readRDS)

# ----------- UI -----------
ui <-
  navbarPage(
    
    "Visualize & Download gpx", id = "nav", tabPanel("Interactive map",
                                                     
                                                     div(
                                                       class = "outer",
                                                       
                                                       tags$head(# Include our custom CSS
                                                         includeCSS("styles.css"), includeScript("gomap.js")),
                                                       
                                                       # If not using custom CSS, set height of leafletOutput to a number instead of percent
                                                       leafletOutput("map", width = "100%", height = "100%"),
                                                       
                                                       # Shiny versions prior to 0.11 should use class = "modal" instead.
                                                       absolutePanel(
                                                         id = "controls",
                                                         class = "panel panel-default",
                                                         fixed = TRUE,
                                                         draggable = TRUE,
                                                         top = 60,
                                                         left = 20,
                                                         right = "auto",
                                                         bottom = "auto",
                                                         width = 500,
                                                         height = "auto",
                                                         
                                                         selectInput(
                                                           "localite",
                                                           "Choisir une localité",
                                                           choices = NULL,
                                                           width = "100%"
                                                         ),
                                                         selectInput(
                                                           "activite",
                                                           "Choisir une activité",
                                                           choices = NULL,
                                                           width = "100%"
                                                         ),
                                                         uiOutput("trace_ui"),
                                                         downloadButton("export", "Exporter la trace sélectionnée", class = "btn-primary"),
                                                         plotlyOutput("profil_altitude", height = "300px")
                                                       ),
                                                     )), tabPanel("Data explorer",
                                                                  tabPanel("Statistiques", verbatimTextOutput("statistiques")))
    
  )


# ----------- SERVER -----------
server <- function(input, output, session) {
  
  # Initialisation des localités
  updateSelectInput(session, "localite", choices = sort(unique(sapply(base_traces, `[[`, "localite"))))
  
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
    Filter(function(t) t$nom == input$trace, traces_filtrees())[[1]]
  })
  
  # Carte leaflet
  output$map <- renderLeaflet({
    req(traces_filtrees(), trace_selectionnee())
    
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri World Imagery") %>%
      addPolylines(
        data = do.call(rbind, lapply(traces_filtrees(), function(t) t$ligne)),
        color = "black", weight = 3, group = "Toutes les traces"
        
      ) %>%
      addPolylines(
        data = trace_selectionnee()$ligne,
        color = "red", weight = 10, group = "Trace sélectionnée"
      ) %>%
      addLayersControl(
        overlayGroups = c("Toutes les traces", "Trace sélectionnée"),
        baseGroups = c("Esri World Imager", "OSM"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    # leaflet() %>%
    #   addTiles() %>%
    #   addPolylines(
    #     data = trace_selectionnee()$ligne,
    #     color = "blue",
    #     weight = 5,
    #     opacity = 0.7,
    #     dashArray = "5,10",
    #     popup = ~paste("Nom:", trace_selectionnee()$nom, "<br>", "Distance:", round(max(st_drop_geometry(trace_selectionnee()$points)$dist_cumul_3d), 2), "km")
    #   )
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
  
  # Export GPX
  output$export <- downloadHandler(
    filename = function() {
      paste0(tools::file_path_sans_ext(input$trace), ".gpx")
    },
    content = function(file) {
      st_write(trace_selectionnee()$ligne, file, driver = "GPX", delete_dsn = TRUE)
    }
  )
}

# Lancer l'application
shinyApp(ui = ui, server = server)

