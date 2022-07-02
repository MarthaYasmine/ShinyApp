library(shiny)
library(datasets)
library(DT)
library(ggplot2)
library(dplyr)
library(ggpubr)

data(iris)
ddatos <- iris

# Set Working Directory ----
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Define UI  ----
ui <- fluidPage(
  tabsetPanel(
    
# Anallisis Exploratorio ----    
    tabPanel("Analisis exploratorio",
             
             # Lectura de datos ----
             fluidRow(
               column(12,h2("Lectura de datos:"))
             ),
             fluidRow(
               column(4,selectInput("archtipo",
                                    "Funcion para leer datos",
                                    c("read.xls","read.txt","read.csv"),
                                    selected = "read.txt")),
               
               # Widgets condicionados
               column(4,
                      fluidRow(uiOutput(outputId = "wg1")),
                      fluidRow(uiOutput(outputId = "wg2"))
                      ),
               column(4,fileInput("archivo",
                                  "Cargar archivo:"))
             ),
             
             # Datos ----
             fluidRow(
               column(12,h2("Datos"))
             ),
             fluidRow(
               
               column(12,selectInput("variables",
                                     "Variables a mostrar:",
                                     names(ddatos),
                                     selected = names(ddatos),
                                     multiple = TRUE,
                                     selectize = TRUE))
             ),
             fluidRow(tableOutput("datos")),
             fluidRow(DT::dataTableOutput('tbl2')),
             
             # Graficos ----
             fluidRow(
               column(12,h2("Graficos"))
             ),
             fluidRow(
               
               sidebarPanel(fluidRow(selectizeInput("var1",
                                                "Seleccion de Variables:",
                                                num <- names(ddatos)[unlist(lapply(ddatos, is.numeric))],
                                                selected = names(ddatos)[1:2],
                                                multiple=TRUE,
                                                options = list(maxItems = 2))
                            ),
                            fluidRow(actionButton("ab1","Actualiza Grafica")
                              
                            )
                 
                 
               ),
               mainPanel(

                 column(12, fluidRow(
                   plotOutput("pplots")
                 ))
                 #column(6,fluidRow(
                  #                plotOutput("pplots", height = 200),
                   #               plotOutput("histPlot2", height = 200)
                    #              )
                     #   ),
                 #column(6,plotOutput("dispPlot", height = 400))
                 
               )

             ),
             
             # Desctiptivos ----
             fluidRow(
               column(12,h2("Descriptivos"))
             ),
             fluidRow(
               column(4, offset = 8, selectizeInput("var2",
                                                    "Seleccion de Variables:",
                                                    names(ddatos),
                                                    multiple=TRUE,
                                                    options = list(maxItems = 2)))
             ),
             fluidRow(
               column(4, offset = 8, actionButton("ab2","Actualiza Descriptivos"))
             ),
             fluidRow(
               column(4, offset = 8, downloadButton("dlb1","Descarga Descriptivos"))
             ),
             # Correlaciones ----
             fluidRow(
               column(12,h2("Correlaciones"))
             ),
             fluidRow(
               column(4, offset = 8, selectizeInput("var3",
                                                    "Seleccion de Variables:",
                                                    names(ddatos),
                                                    multiple=TRUE,
                                                    options = list(maxItems = 2)))
             ),
             fluidRow(
               column(4, offset = 8, actionButton("ab3","Actualiza Correlaciones"))
             )
    ),
             

# Analisis Componentes Principales ----    
    tabPanel("Analisis de Componenetes principales",
             fluidRow(
               column(12,h2("Variables para ACP"))
             ),
             
             fluidRow(
               column(12,h2("Valores Propios"))
             ),
             fluidRow(
               column(12,h2("Circulo de correlaciones"))
             ),
             fluidRow(
               column(12,h2("Componentes Principales"))
             ),
             fluidRow(
               column(12,h2("Bigrafico"))
             ),
             fluidRow(
               column(12,h2("Contribuciones"))
             ))
  )
  
)
  

# Define server logic  ----
server <- function(input, output) {

# Analisis Exploratorio ----

#Lectura de datos
  
  # Widgets condicionados
  output$wg1 <- renderUI({
    if(input$archtipo == "read.xls"){
      numericInput("sheets",
                   "Numero de Hoja",
                   value = 1,
                   min = 1)
    }else{
      selectInput("headers",
                  "Contiene encabezados:",
                  c(TRUE,FALSE),
                  selected = TRUE)
    }
  })
  
  output$wg2 <- renderUI({
    req(input$archtipo == "read.txt")
    textInput("separador",
              "Campo separador:",
              value = ";")
  })

# Datos
  
  output$tbl2 <- DT::renderDataTable({
    DT::datatable(ddatos[ ,input$variables])
  })
  
# Graficos
  
  output$pplots <- renderPlot({
    if(length(input$var1)==2){
      h1 <- ggplot(ddatos, aes_string(x=input$var1[1])) + geom_histogram()
      h2 <- ggplot(ddatos, aes_string(x=input$var1[2])) + geom_histogram()
      ds <- ggplot(ddatos, aes_string(x=input$var1[1], y=input$var1[2])) + geom_point()
    
      ggarrange(ggarrange(h1,h2, nrow = 2),ds)
    } else{
      ggplot(ddatos, aes_string(x=input$var1)) + geom_histogram()
    }
    
    
  })

  
}

# Run the app ----
shinyApp(ui = ui, server = server)