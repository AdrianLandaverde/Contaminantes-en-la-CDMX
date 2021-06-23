#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(dplyr, quietly = TRUE)
library(gapminder)



# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Correlación de Contamimantes respecto a Varibales Meteorológicas"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            
            selectInput(inputId = "Periodo", label = h4("1.) Seleccione el Periodo"), 
                        choices = list("Años" = "annio", "Meses" = "mes", "Días" = "dia"), 
                        selected = "mes"),hr(),
            
            selectInput(inputId = "Contaminante", label = h4("2.) Seleccione el Contaminante"), 
                        choices = list("SO2" = "df_SO2.csv", "PM10" = "df_PM10.csv", "NO2" = "df_NO2.csv", "NO" = "df_NO.csv"),
                        selected = "SO2"),hr(),
            
            selectInput(inputId = "Estacion", label = h4("3.) Seleccione La Estación"), 
                        choices = list("AJM" = "AJM", "CUT" = "CUT", "HGM" = "HGM", "INN" = "INN", "MER" = "MER", "MON" = "MON", "MPA" = "MPA", "TLA" = "TLA", "XAL" = "XAL"),
                        selected = 8),hr()
        ),

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("distPlot"),
            #plotOutput("graficoRegresionMultiple"),
            verbatimTextOutput("resumen")
        )
    )
))
