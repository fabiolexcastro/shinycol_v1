
library(shiny)
library(tidyverse)
library(sf)
library(fs)
library(glue)
library(leaflet)

# setwd('./col_dpto_v1')

rslt <- st_read('www/popu_v1.gpkg')
pal <- colorNumeric(palette = "YlOrRd", domain = NULL)

# Server y UI -------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Mapa Coropletico - Población"),
  sidebarLayout(
    sidebarPanel(
      selectInput("depto_sel", "Selecciona un departamento:", choices = rslt$DPTO_CNMBR, selected = "AZUAY"),
      selectInput("var_sel", "Selecciona una variable:", choices = c("urbana", "rural"), selected = "urbana")
    ),
    mainPanel(
      leafletOutput("mapa", width = '1500px', height = '1000px')
    )
  )
)

server <- function(input, output) {
  
  output$mapa <- renderLeaflet({
    # Subset del departamento seleccionado
    sel_depto <- rslt[rslt$DPTO_CNMBR == input$depto_sel, ]
    
    # Definir paleta de colores
    pal <- colorNumeric(palette = "YlOrRd", domain = sel_depto[[input$var_sel]])
    
    # Crear mapa coropletico
    leaflet(data = sel_depto) %>% 
      addTiles() %>%
      addPolygons(
        fillColor = pal(sel_depto[[input$var_sel]]),
        fillOpacity = 0.8,
        color = "#BDBDC3",
        weight = 1,
        highlight = highlightOptions(
          weight = 2,
          color = "#666",
          fillOpacity = 0.7,
          bringToFront = TRUE),
        label = sprintf("%s: %s", sel_depto$DPTO_CNMBR, sel_depto[[input$var_sel]]),
        group = "mapa") %>%
      addLegend(pal = pal, values = sel_depto[[input$var_sel]],
                title = "Población", position = "bottomright")
  })
  
}

shinyApp(ui = ui, server = server)
