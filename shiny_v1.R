

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(shiny, leaflet, sf, fs, tidyverse, rgdal, RColorBrewer)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
dpto <- st_read('https://raw.githubusercontent.com/fabiolexcastro/tutorials_youtube/master/populationMap/gpkg/mpio.geojson')
popu <- suppressMessages(read_csv('https://raw.githubusercontent.com/fabiolexcastro/tutorials_youtube/master/populationMap/tble/demografia_urbana_rural.csv'))
popu <- filter(popu, year == 2022, mpio != 'Colombia') %>% mutate(codigo = as.character(codigo))
rslt <- inner_join(dpto, popu, by = c('MPIO_CCNCT' = 'codigo'))

ui <- fluidPage(
  titlePanel("Mapa Coropletico - Población"),
  sidebarLayout(
    sidebarPanel(
      selectInput("depto_sel", "Selecciona un departamento:", choices = rslt$DPTO_CNMBR, selected = "AZUAY"),
      selectInput("var_sel", "Selecciona una variable:", choices = c("urbana", "rural"), selected = "urbana")
    ),
    mainPanel(
      leafletOutput("mapa")
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

