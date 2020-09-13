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


### dashboardPage, dashboardHeader, dashboardSidebar, y dashboardBody ##

### INTERFAZ DEL USUARIO
ui<-dashboardPage(title = "Dashboard Shiny", skin = "red",
    dashboardHeader(title = "Visualizacion"),
    dashboardSidebar(
     sidebarMenu(
       menuItem("Lectura de datos", tabName = "Lectura", icon = icon("eye")),
       menuItem("Visualizacion de datos", tabName = "Visualizacion", 
                icon = icon("arrow-right"))          
              )
    
     ),
    
    dashboardBody(
        tabItems( ### agregar elementos de la barra lateral 
          tabItem(tabName = "Lectura",
                  fluidRow( 
                      box(
                      title = "Configuración", width = 6,
                      fileInput("Datos", "Cargar Datos", buttonLabel = "Buscar",
                                placeholder = "No hay archivo Seleccionado")
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
          tabItem(tabName = "Visualizacion")
        )
    )
    
)


server<- function(input, output){
    datos_crudos<-reactive({
        datos<-fread(input$Datos$datapath) ## para leer la data
        return(datos)
        
    })
    
   output$datoscrudos<-renderDT({
       return(datos_crudos())
   }, 
   options= list(scrollX =T)) # Agregar la barra de navegación en la tabla  
   output$estructura<-renderPrint({ ### para mostar "algo en la consola"
       return(str(datos_crudos()))    
   })
   
   
} 


shinyApp(ui = ui, server = server)

    
    
    

