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

ODSSpr = import("https://github.com/ChiaraZamoraM/RDiplomatura/raw/main/MapaDist.RDS")

subtitulo = {'background-color: #34282C; 
            color: white;
            font-size: 20px;
            font-weight: bold;
            text-align: center;
            padding-top: 15px;
            padding-bottom: 15px;'}

ui <- fluidPage(
    tags$style('.container-fluid {
                             background-color: white;
              }'),
    titlePanel(windowTitle = "Trabajo Final",
               h1("Avance de la vacunación",
                  style={'background-color: #34282C;
                  color: white;
            font-weight: bold;
            text-align: center;
            padding-top: 25px;
            padding-bottom: 25px;'})
    ),
    
    sidebarLayout(
        sidebarPanel(
            h5("A diciembre de 2021, ¿cuál era el avance acumulado de la vacunación en los distritos según el partido con mayoría de votos válidos en la primera y segunda vuelta de las Elecciones Generales 2021?"),
            selectInput("ano",
                        "Seleccione un partido",
                        choices = unique(ODSSpr$Ano)
            ),
            tags$a(href="https://www.datosabiertos.gob.pe/", 
                   "Referencia de los datos",
                   target= "_blank")
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Primera Vuelta",
                         titlePanel(h2("Resultados de la primera vuelta de las EG 2021",
                                       style= subtitulo)
                         ),
                         fluidRow(column(6,leafletOutput("primera", height = 650,width=500)),
                                  column(6,leafletOutput("mapaIDE", height = 650,width=500)))),
                tabPanel("Segunda Vuelta",
                         titlePanel(h2("Resultados de la segunda vuelta de las EG 2021",
                                       style= subtitulo)
                         ),
                         fluidRow(column(6,leafletOutput("segunda", height = 650)),
                                  column(6,leafletOutput("mapaIDE", height = 650))))
            )
        )
    )
)

# Define server logic required to draw a histogram

server <- function(input, output) {
    
    partido_mapadist <- reactive({
        y    <- mapadist %>% filter(Partido == input$Ganador1)
        return(y)
    })
    
    output$primera = renderLeaflet({
        pal1 = colorNumeric(palette = "Reds", domain = mapadist$Avance)
        
        partido_mapadist()  %>% 
            st_transform(crs= "+init=epsg:4326") %>%
            leaflet() %>%
            addProviderTiles(provider= "CartoDB.Positron") %>%
            addPolygons(label= paste0(partido_mapadist()$departamento,': ', partido_mapadist()$Avance,"%"),
                        stroke = TRUE, 
                        smoothFactor =  .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        color= "grey",
                        weight = 0.5,
                        fillColor = ~pal1(partido_mapadist()$Avance),
                        highlightOptions = highlightOptions(weight = 2,
                                                            fillOpacity= 1,
                                                            color = "grey",
                                                            opacity = 1,
                                                            bringToFront = TRUE)) %>%
            leaflet::addLegend("bottomright",
                               pal = pal1,
                               values = ~Avance,
                               title= "Porcentaje (%)",
                               opacity= 0.7)
    })
    
    output$table1_1_1 = renderDT({
        datatable(ano_ODSSpr()[c("DEPARTAMEN","ODS1_1_1")], 
                  colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1_1_1'),
                  filter = 'top',
                  options = list(pageLength = 13
                  ))})
    
    
    output$pobrezatot = renderLeaflet({
        pal2 = colorNumeric(palette = "Reds", domain = ODSSpr$ODS1_2_1)
        
        ano_ODSSpr()  %>% 
            st_transform(crs= "+init=epsg:4326") %>%
            leaflet() %>%
            addProviderTiles(provider= "CartoDB.Positron")%>%
            addPolygons(data = mapa_prov, 
                        stroke = TRUE, 
                        smoothFactor =  .5,
                        opacity = 1,
                        fillOpacity = 0,
                        weight = 0.5,
                        color= "black") %>%
            addPolygons(label= paste0(ano_ODSSpr()$DEPARTAMEN,': ', ano_ODSSpr()$ODS1_2_1,"%"),
                        stroke = TRUE, 
                        smoothFactor =  .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        color= "grey",
                        weight = 0.5,
                        fillColor = ~pal2(ano_ODSSpr()$ODS1_2_1),
                        highlightOptions = highlightOptions(weight = 2,
                                                            fillOpacity= 1,
                                                            color = "grey",
                                                            opacity = 1,
                                                            bringToFront = TRUE))%>%
            
            leaflet::addLegend("bottomright",
                               pal = pal2,
                               values = ~ODS1_2_1,
                               title= "Porcentaje (%)",
                               opacity= 0.7) 
    })
    output$table1_2_1 = renderDT({
        datatable(ano_ODSSpr()[c("DEPARTAMEN","ODS1_2_1")], 
                  colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1_2_1'),
                  filter = 'top',
                  options = list(pageLength = 13
                  ))})
    
    output$pension = renderLeaflet({
        pal3 = colorNumeric(palette = "Reds", domain = ODSSpr$ODS1_3_1)
        
        ano_ODSSpr()  %>% 
            st_transform(crs= "+init=epsg:4326") %>%
            leaflet() %>%
            addProviderTiles(provider= "CartoDB.Positron") %>%
            addPolygons(data = mapa_prov, 
                        stroke = TRUE, 
                        smoothFactor =  .5,
                        opacity = 1,
                        fillOpacity = 0,
                        weight = 0.5,
                        color= "black")%>%
            addPolygons(label= paste0(ano_ODSSpr()$DEPARTAMEN,': ', ano_ODSSpr()$ODS1_3_1,"%"),
                        stroke = TRUE, 
                        smoothFactor =  .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        fillColor = ~pal3(ano_ODSSpr()$ODS1_3_1),
                        color="grey",
                        weight = 0.5,
                        highlightOptions = highlightOptions(weight = 2,
                                                            fillOpacity= 1,
                                                            color = "grey",
                                                            opacity = 1,
                                                            bringToFront = TRUE))%>%
            
            leaflet::addLegend("bottomright",
                               pal = pal3,
                               values = ~ODS1_3_1,
                               title= "Porcentaje (%)",
                               opacity= 0.7) 
    })
    output$table1_3_1 = renderDT({
        datatable(ano_ODSSpr()[c("DEPARTAMEN","ODS1_4_1")], 
                  colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1_4_1'),
                  filter = 'top',
                  options = list(pageLength = 13
                  ))})
    
    output$servbasicos = renderLeaflet({
        pal4 = colorNumeric(palette = "Reds", domain = ODSSpr$ODS1_4_1)
        
        ano_ODSSpr()  %>% 
            st_transform(crs= "+init=epsg:4326") %>%
            leaflet() %>%
            addProviderTiles(provider= "CartoDB.Positron") %>%
            addPolygons(data = mapa_prov, 
                        stroke = TRUE, 
                        smoothFactor =  .5,
                        opacity = 1,
                        fillOpacity = 0,
                        weight = 0.5,
                        color= "black")%>%
            addPolygons(label= paste0(ano_ODSSpr()$DEPARTAMEN,': ', ano_ODSSpr()$ODS1_4_1,"%"),
                        stroke = TRUE, 
                        smoothFactor =  .5,
                        opacity = 1,
                        fillOpacity = 0.7,
                        fillColor = ~pal4(ano_ODSSpr()$ODS1_4_1),
                        color= "grey",
                        weight = 0.5,
                        highlightOptions = highlightOptions(weight = 2,
                                                            fillOpacity= 1,
                                                            color = "black",
                                                            opacity = 1,
                                                            bringToFront = TRUE))%>%
            
            leaflet::addLegend("bottomright",
                               pal = pal4,
                               values = ~ODS1_4_1,
                               title= "Porcentaje (%)",
                               opacity= 0.7) 
    })
    output$table1_4_1 = renderDT({
        datatable(ano_ODSSpr()[c("DEPARTAMEN","ODS1_4_1")], 
                  colnames = c('Departamento' = 'DEPARTAMEN','Porcentaje' = 'ODS1_4_1'),
                  filter = 'top',
                  options = list(pageLength = 13
                  ))})
}

# Run the application 
shinyApp(ui = ui, server = server)
