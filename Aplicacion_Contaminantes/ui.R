#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Concentración de SO2 en el tiempo"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(

            selectInput(inputId = "Periodo", label = h4("1.) Seleccione el Periodo"), 
                        choices = list("Años" = "annio", "Meses" = "mes", "Días" = "dia"), 
                        selected = "annio"),hr(),
            
            
            selectInput(inputId = "Estacion", label = h4("2.) Seleccione La Estación"), 
                        choices = list("AJM" = 1, "CUT" = 2, "HGM" = 3, "INN" = 4, "MER" = 5, "MON" = 6, "MPA" = 7, "TLA" = 8, "XAL" = 9),
                        selected = 8),hr()
            
            ),
        

        # Show a plot of the generated distribution
        mainPanel(
            plotOutput("regresionLinealGrafico"),
            verbatimTextOutput("resumen")
            
        )
    )
))
