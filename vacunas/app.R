#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(htmlwidgets)
library(rio)
library(sf)
library(xts)
library(crosstalk)
library(DT)
library(shinyWidgets)

mapadist = import("https://github.com/ChiaraZamoraM/RDiplomatura/raw/main/MapaDist.RDS")

mapadist$Avance = round(mapadist$Avance)
mapadist$pc1 = round(mapadist$pc1,2)

risk.bins <-c(0, 25, 55, 65, 75, 85, 95, 100) 
risk.pal <- colorBin( "plasma", bins=risk.bins, na.color = "#aaff56")


subtitulo = {'background-color: #155084; 
            color: white;
            font-size: 20px;
            font-weight: bold;
            text-align: center;
            padding-top: 15px;
            padding-bottom: 15px;
            font-family: "Open Sans";'}

ui <- fluidPage(
    tags$style('.container-fluid {
                             background-color: #AFD2F0;
              }'),
    titlePanel(windowTitle = "Trabajo Final",
               h1("Avance de la vacunación y Elecciones Generales 2021",
                  style={'background-color: #155084;
                  color: white;
            font-weight: bold;
            text-align: center;
            padding-top: 15px;
            padding-bottom: 15px;
            font-family: "Open Sans";'})
    ),
    
    sidebarLayout(
        sidebarPanel(
            h5("A diciembre de 2021, ¿cuál era el avance acumulado de la vacunación en los distritos según el partido con mayoría de votos válidos en la primera y segunda vuelta de las Elecciones Generales 2021?",
               style={'color: black;
            font-weight: bold;
            text-align: justify;
            padding-top: 10px;
            padding-bottom: 10px;
            font-size: 20px;
            line-height: 120%;
            font-family: "Open Sans";'}),
            selectInput("partido",
                        h5("Seleccione un partido",
                           style= {'font-size: 18px;
                               font-family: "Open Sans";'}),
                        choices = unique(mapadist$Ganador1)
            ),
            tags$a(href="https://www.datosabiertos.gob.pe/", 
                   h5("Referencia de los datos",
                      style= {'font-size: 18px;
                               font-family: "Open Sans";'}),
                   target= "_blank"),
            width=3
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(h5("Primera Vuelta",
                            style= {'font-size: 18px;
                            font-weight: bold;
                            font-family: "Open Sans";'}),
                         titlePanel(h2("Resultados de la primera vuelta de las EG 2021",
                                       style= subtitulo)
                         ),
                         fluidRow(column(5,leafletOutput("primera", height = 650)),
                                  column(7,dataTableOutput("dataIDE1", height = 650))),
                         ),
                tabPanel(h5("Segunda vuelta",
                            style= {'font-size: 18px;
                            font-weight: bold;
                            font-family: "Open Sans";'}),
                         titlePanel(h2("Resultados de la segunda vuelta de las EG 2021",
                                       style= subtitulo)
                         ),
                         fluidRow(column(5,leafletOutput("segunda", height = 650)),
                                  column(7,dataTableOutput("dataIDE2", height = 650))))
            ,
            id ="tabselected"),
            width=9
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    
    partido_mapadist <- reactive({
        y    <- mapadist %>% filter(Ganador1 == input$partido)
        return(y)
    })
    
    output$primera = renderLeaflet({
        #pal1 = colorNumeric(palette = "Reds", domain = mapadist$Avance)
        
        partido_mapadist()  %>% 
            st_transform(crs= "+init=epsg:4326") %>%
            leaflet() %>%
            addProviderTiles(provider= "CartoDB.Positron") %>%
            addPolygons(label= paste0(partido_mapadist()$distrito,': ', round(partido_mapadist()$Avance),"%"),
                        stroke = TRUE, 
                        smoothFactor =  .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        color= "grey",
                        weight = 0.5,
                        fillColor = ~risk.pal(partido_mapadist()$Avance),
                        highlightOptions = highlightOptions(weight = 2,
                                                            fillOpacity= 1,
                                                            color = "grey",
                                                            opacity = 1,
                                                            bringToFront = TRUE)) %>%
            leaflet::addLegend("bottomright",
                               pal = risk.pal,
                               values = ~Avance,
                               title= "Porcentaje (%)",
                               opacity= 0.7)
    })
    
    output$dataIDE1 = renderDT({
        datatable(partido_mapadist()[,c(23,25,26,40,47)], 
                  colnames = c('Departamento' = 'departamento','Provincia'='provincia','Distrito'="distrito",'Porcentaje' = 'Avance','IDE'='pc1'),
                  filter = 'top',
                  options = list(pageLength = 12
                  ))})
    
   output$segunda = renderLeaflet({
        #pal1 = colorNumeric(palette = "Reds", domain = mapadist$Avance)
        
        partido_mapadist()  %>% 
            st_transform(crs= "+init=epsg:4326") %>%
            leaflet() %>%
            addProviderTiles(provider= "CartoDB.Positron") %>%
            addPolygons(label= paste0(partido_mapadist()$distrito,': ', round(partido_mapadist()$Avance),"%"),
                        stroke = TRUE, 
                        smoothFactor =  .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        color= "grey",
                        weight = 0.5,
                        fillColor = ~risk.pal(partido_mapadist()$Avance),
                        highlightOptions = highlightOptions(weight = 2,
                                                            fillOpacity= 1,
                                                            color = "grey",
                                                            opacity = 1,
                                                            bringToFront = TRUE)) %>%
            leaflet::addLegend("bottomright",
                               pal = risk.pal,
                               values = ~Avance,
                               title= "Porcentaje (%)",
                               opacity= 0.7)
    })
    
    output$dataIDE2 = renderDT({
        datatable(partido_mapadist()[,c(23,25,26,40,47)], 
                  colnames = c('Departamento' = 'departamento','Provincia'='provincia','Distrito'="distrito",'Porcentaje' = 'Avance','IDE'='pc1'),
                  filter = 'top',
                  options = list(pageLength = 12
                  ))})
}

# Run the application 
shinyApp(ui = ui, server = server)
