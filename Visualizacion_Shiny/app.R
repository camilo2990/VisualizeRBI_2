##### CURSO DASHBOARD CON SHINY ######


### paquetes a utilizar ####

library(shiny)
library(shinydashboard)
library(DT)
library(data.table)
library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)
library(tools)

### dashboardPage, dashboardHeader, dashboardSidebar, y dashboardBody ##

### INTERFAZ DEL USUARIO
ui<-dashboardPage(title = "Dashboard Shiny", skin = "red",
    dashboardHeader(title = "Visualizacion"),
    dashboardSidebar(
     sidebarMenu(
       menuItem("Lectura de datos", tabName = "Lectura", icon = icon("eye")),
       menuItem("Visualizacion de datos", tabName = "Visualizacion", 
                icon = icon("arrow-right")),       
       uiOutput("controles_ejex")
                 )
    
     ),
    
    dashboardBody(
        tabItems( ### agregar elementos de la barra lateral 
          tabItem(tabName = "Lectura",
                  fluidRow( 
                      box(
                      title = "Configuración", width = 6,
                      fileInput("Datos", "Cargar Datos", buttonLabel = "Buscar",
                      placeholder = "No hay archivo Seleccionado"),
                      uiOutput("controles_excel")
                 
                     )
                      
                  ),
                  fluidRow( # para extender de forma horizontal las cajas
                     tabBox(
                      title = "vista previa", width=12, # para el tamaño de las columnas
                       tabPanel(title = "Estructura", 
                        verbatimTextOutput("estructura")
                                           
                                  ),
                          
                             tabPanel( 
                                      "Datos Crudos", DTOutput("datoscrudos")
                         )
                    ) 
                      
                )
                
                  
            ),
          
          tabItem(tabName = "Visualizacion", 
                  box(title = "grafica", width = 9 ,
                      plotOutput(outputId = "grafica"))
                  )
                 
            )
        )
    )
    

server<- function(input, output){
   output$controles_excel<-renderUI({
       validate(need(
           !is.null(input$Datos$datapath), "Esperando por Archivo"
       ))
       if(tolower(file_ext(input$Datos$datapath))%in% c("xlsx","xls")){
          n <- excel_sheets(path=input$Datos$datapath)
          sliderInput('HojasExcel', label = 'Hoja excel', min = 1, 
                      max = length(n), value = 1, step = 1)
       }  
   })
     datos_crudos<-reactive({
        validate(need(
                   !is.null(input$Datos$datapath), "Esperando por Archivo"
        ))
        if(tolower(file_ext(input$Datos$datapath))%in% c("xlsx","xls")){
            datos<-read_excel(input$Datos$datapath, sheet = input$HojasExcel)
            }
        else{
            datos<-fread(input$Datos$datapath)  ## para leer la data   
       
         }
             return(datos)
        
    })
    
   output$datoscrudos<-renderDT({
       return(datos_crudos())
   }, 
   options= list(scrollX =T)) # Agregar la barra de navegación en la tabla  
   output$estructura<-renderPrint({ ### para mostar "algo en la consola"
       return(str(datos_crudos()))    
   })
   
   output$controles_ejex <- renderUI({
       
       selectInput(inputId = 'eje_x', label = 'Eje X', 
                   choices = c('Ninguna', colnames(datos_crudos())),
                   selected = 'Ninguna')
      
   })
   
 output$grafica<-renderPlot({
     datos<-as.data.frame(datos_crudos())
     validate(
        need(input$eje_x !="Ninguna", message = "selecciona la variable a graficar en el eje X")
     )
    
     if(is.numeric(datos[, input$eje_x])){
         ggplot(data = datos, aes_string(x=input$eje_x))+
             geom_density(col="darkblue")
       } else if(is.character(datos[,input$eje_x]) | is.factor(datos[,input$eje_x])){
         
         tab <- table(datos[,input$eje_x]) %>% 
             as.data.frame()
         colnames(tab) <- c(input$eje_x, 'Total')
         
         ggplot(data = tab, aes_string(x = input$eje_x, y = 'Total')) +
             geom_bar(stat = 'identity')
         
         
      }
     
 })
   
     
} 


shinyApp(ui = ui, server = server)

    
    
    

