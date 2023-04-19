

# Load libraries ----------------------------------------------------------
require(pacman)
pacman::p_load(shiny, leaflet, sf, fs, tidyverse, rgdal, RColorBrewer)

g <- gc(reset = T); rm(list = ls()); options(scipen = 999, warn = -1)

# Load data ---------------------------------------------------------------
dpto <- st_read('https://raw.githubusercontent.com/fabiolexcastro/tutorials_youtube/master/populationMap/gpkg/mpio.geojson')
popu <- suppressMessages(read_csv('https://raw.githubusercontent.com/fabiolexcastro/tutorials_youtube/master/populationMap/tble/demografia_urbana_rural.csv'))
popu <- filter(popu, year == 2022, mpio != 'Colombia') %>% mutate(codigo = as.character(codigo))
rslt <- inner_join(dpto, popu, by = c('MPIO_CCNCT' = 'codigo'))
st_write(rslt, 'popu_v1.gpkg')

# Server y UI -------------------------------------------------------------
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


#

ui <- fluidPage(
  titlePanel("Visualización de información espacial"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "depto_sel", label = "Seleccionar departamento:",
                  choices = unique(rslt$DPTO_CNMBR), selected = NULL),
      selectInput(inputId = "var_sel", label = "Seleccionar variable:",
                  choices = c("Población urbana" = 'urbana', "Población rural" = 'rural'), selected = NULL)
    ),
    mainPanel(
      leafletOutput("mapa", width = "100%", height = "800px")
    )
  )
)

server <- function(input, output, session) {
  
  depto_sel <- reactive({
    rslt %>% filter(DPTO_CNMBR == input$depto_sel)
  })
  
  output$mapa <- renderLeaflet({
    leaflet(depto_sel()) %>%
      addProviderTiles("OpenStreetMap.Mapnik") %>%
      setView(lng = -79.3889, lat = 43.5387, zoom = 8) %>%
      addPolygons(fillColor = ~colorQuantile("YlOrRd", get(input$var_sel))(get(input$var_sel)),
                  fillOpacity = 0.7, color = "#BDBDC3", weight = 1) %>%
      addLegend(pal = colorQuantile("YlOrRd", get(input$var_sel)), 
                values = depto_sel()[, get(input$var_sel)], 
                title = "Población", position = "bottomright")
  })
  
}

shinyApp(ui, server)

##
pal <- colorNumeric(palette = "YlOrRd", domain = NULL)

ui <- fluidPage(
  titlePanel("Visualización de información espacial"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "depto_sel",
                  label = "Seleccione el departamento:",
                  choices = unique(rslt$DPTO_CNMBR)),
      selectInput(inputId = "var_sel",
                  label = "Seleccione la variable:",
                  choices = c("Población Urbana" = "urbana",
                              "Población Rural" = "rural"))
    ),
    mainPanel(
      leafletOutput(outputId = "map", width = "100%", height = "600px")
    )
  )
)

# Define el server
server <- function(input, output) {
  
  # Filtar el shapefile
  sel_dept <- reactive({
    rslt[rslt$DPTO_CNMBR == input$depto_sel, ]
  })
  
  # Crear el mapa
  output$map <- renderLeaflet({
    # Crear mapa base
    map <- leaflet() %>%
      addTiles()
    
    # Añadir capa de datos
    map %>% 
      addPolygons(data = sel_dept(),
                  fillColor = ~ pal(sel_dept()[, input$var_sel]),
                  fillOpacity = 0.7,
                  color = "#BDBDC3",
                  weight = 1,
                  popup = paste0("<strong>Departamento:</strong> ",
                                 sel_dept()$DPTO_CNMBR,
                                 "<br>",
                                 "<strong>Población ",
                                 ifelse(input$var_sel == "urbana", "Urbana:", "Rural:"),
                                 "</strong> ",
                                 sel_dept()[, input$var_sel])) %>%
      # Añadir leyenda
      addLegend(position = "bottomright",
                pal = pal,
                values = sel_dept()[, input$var_sel],
                title = "Población",
                labFormat = labelFormat())
  })
}

# Run the app
shinyApp(ui = ui, server = server)
