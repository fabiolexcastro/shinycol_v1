


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


# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("Visualización de información espacial"),
  sidebarLayout(
    sidebarPanel(
      # Selecciona el departamento
      selectInput("depto", "Selecciona un departamento:",
                  choices = rslt$DPTO_CNMBR)
    ),
    mainPanel(
      # Muestra el mapa
      leafletOutput("map")
    )
  )
)

# Define el server
server <- function(input, output) {
  
  # Subconjunto de datos del departamento seleccionado
  depto_sel <- reactive({
    subset(rslt, rslt$DPTO_CNMBR == input$depto)
  })
  
  # Crea el mapa
  output$map <- renderLeaflet({
    leaflet(depto_sel()) %>%
      addTiles() %>%
      addPolygons()
  })
  
}

shinyApp(ui, server)


##


# Define la UI
ui <- fluidPage(
  titlePanel("Visualización de información espacial"),
  sidebarLayout(
    sidebarPanel(
      # Selecciona el departamento
      selectInput("depto", "Selecciona un departamento:",
                  choices = rslt$DPTO_CNMBR)
    ),
    mainPanel(
      # Muestra el mapa
      leafletOutput("map", width = "100%", height = "700px")
    )
  )
)

# Define el server
server <- function(input, output) {
  
  # Subconjunto de datos del departamento seleccionado
  depto_sel <- reactive({
    subset(rslt, rslt$DPTO_CNMBR == input$depto)
  })
  
  # Crea el mapa
  output$map <- renderLeaflet({
    leaflet(depto_sel()) %>%
      addTiles() %>%
      addPolygons(popup = paste("Población urbana: ",
                                depto_sel()$urbana))
  })
  
}

# Ejecuta la aplicación
shinyApp(ui, server)


##


# Define la UI
ui <- fluidPage(
  titlePanel("Visualización de información espacial"),
  sidebarLayout(
    sidebarPanel(
      # Selecciona el departamento
      selectInput("depto", "Selecciona un departamento:",
                  choices = rslt$DPTO_CNMBR)
    ),
    mainPanel(
      # Muestra el mapa
      leafletOutput("map", width = "100%", height = "700px")
    )
  )
)

# Define el server
server <- function(input, output) {
  
  # Subconjunto de datos del departamento seleccionado
  depto_sel <- reactive({
    subset(rslt, rslt$DPTO_CNMBR == input$depto)
  })
  
  # Crea el mapa
  output$map <- renderLeaflet({
    max_urbana <- ifelse(all(is.na(depto_sel()$urbana)), 0, max(depto_sel()$urbana))
    leaflet(depto_sel()) %>%
      addTiles() %>%
      addPolygons(
        fillColor = ~colorBin("YlOrRd", depto_sel()$urbana, bins = 5),
        weight = 1,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7,
        popup = paste("Población urbana: ", depto_sel()$urbana)
      ) %>%
      addLegend(
        "bottomright",
        title = "Población",
        colors = brewer.pal(5, "YlOrRd"),
        labels = formatC(
          seq(0, max_urbana, length.out = 5),
          digits = 0,
          format = "d"
        ),
        opacity = 0.7
      )
  })
  
}

# Ejecuta la aplicación
shinyApp(ui, server)

