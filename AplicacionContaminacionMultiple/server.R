#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr, quietly = TRUE)
library(gapminder)
library(corrplot)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({
        contaminante<- input$Contaminante
        periodo_Regresion_Multiple<- input$Periodo
        estacion= input$Estacion
        
        df_todos_RM<- read_csv(contaminante, 
                               col_types = cols(
                                   X1=col_character(),
                                   id_station = col_character(),
                                   date= col_character(),
                                   id_parameter = col_character(),
                                   partes_por_billon = col_double(),
                                   PA_mmHg = col_double(),
                                   TMP_Celsius = col_double(),
                                   Direccion_viento = col_double(),
                                   Velocidad_viento = col_double(),
                                   RH_porcentaje = col_double()))
        if(periodo_Regresion_Multiple== "annio"){
            df_todos_RM$date<- paste("01/01/",substring(df_todos_RM$date,7,8),sep = "") 
        }else if(periodo_Regresion_Multiple=="mes"){
            df_todos_RM$date<- paste("01/",substring(df_todos_RM$date,4,8),sep = "")
        }else if(periodo_Regresion_Multiple=="dia"){
            df_todos_RM$date<- substring(df_todos_RM$date,1,8)
        }
        
        df_todos_RM$date<- as.Date(df_todos_RM$date, format="%d/%m/%Y")
        
        df_estacion_RM <- df_todos_RM[df_todos_RM$id_station==estacion, ]
        
        
        df_estacion_RM<- select(df_estacion_RM, date, 
                                partes_por_billon,
                                PA_mmHg, 
                                TMP_Celsius,
                                RH_porcentaje,
                                Direccion_viento,
                                Velocidad_viento)
        df_estacion_RM %>%
            group_by(date) %>%
            tally()
        df_estacion_RM%>%group_by(date)%>%
            summarise_all(mean)-> df_estacion_RM
        
        df_estacion_RM<- select(df_estacion_RM,  
                                partes_por_billon,
                                PA_mmHg, 
                                TMP_Celsius,
                                RH_porcentaje,
                                Direccion_viento,
                                Velocidad_viento)
        
        corrplot(cor(df_estacion_RM), method="color")

    })
    
    output$resumen<- renderPrint({
        contaminante<- input$Contaminante
        periodo_Regresion_Multiple<- input$Periodo
        estacion= input$Estacion
        df_todos_RM<- read_csv(contaminante, 
                               col_types = cols(
                                   X1=col_character(),
                                   id_station = col_character(),
                                   date= col_character(),
                                   id_parameter = col_character(),
                                   partes_por_billon = col_double(),
                                   PA_mmHg = col_double(),
                                   TMP_Celsius = col_double(),
                                   Direccion_viento = col_double(),
                                   Velocidad_viento = col_double(),
                                   RH_porcentaje = col_double()))
        if(periodo_Regresion_Multiple== "annio"){
            df_todos_RM$date<- paste("01/01/",substring(df_todos_RM$date,7,8),sep = "") 
        }else if(periodo_Regresion_Multiple=="mes"){
            df_todos_RM$date<- paste("01/",substring(df_todos_RM$date,4,8),sep = "")
        }else if(periodo_Regresion_Multiple=="dia"){
            df_todos_RM$date<- substring(df_todos_RM$date,1,8)
        }
        
        df_todos_RM$date<- as.Date(df_todos_RM$date, format="%d/%m/%Y")

        df_estacion_RM <- df_todos_RM[df_todos_RM$id_station==estacion, ]
        
        
        df_estacion_RM<- select(df_estacion_RM, date, 
                           partes_por_billon,
                           PA_mmHg, 
                           TMP_Celsius,
                           RH_porcentaje,
                           Direccion_viento,
                           Velocidad_viento)
        df_estacion_RM %>%
            group_by(date) %>%
            tally()
        df_estacion_RM%>%group_by(date)%>%
            summarise_all(mean)-> df_estacion_RM
        
        df_estacion_RM<- select(df_estacion_RM,  
                                partes_por_billon,
                                PA_mmHg, 
                                TMP_Celsius,
                                RH_porcentaje,
                                Direccion_viento,
                                Velocidad_viento)
        regresionMultiple = lm(df_estacion_RM$partes_por_billon ~
                                   df_estacion_RM$PA_mmHg +
                                   df_estacion_RM$TMP_Celsius +
                                   df_estacion_RM$RH_porcentaje +
                                   df_estacion_RM$Direccion_viento +
                                   df_estacion_RM$Velocidad_viento
                               , data=df_estacion_RM)
        print(summary(regresionMultiple))
        
        
    })

})
