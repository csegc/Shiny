library(shiny)
library(maps)
library(mapproj)
#source("C:/Users/Cristian/Dropbox/Curso UC/Workshop R shiny/census/helpers.R")
#counties <- readRDS("C:/Users/Cristian/Dropbox/Curso UC/Workshop R shiny/census/data/counties.rds")
source("helpers.R")
counties <- readRDS("data/counties.rds")

ui <- fluidPage(
  titlePanel("CensusVis"),
  sidebarLayout(
    sidebarPanel(
      helpText("Crear mapas Demograficos con la", 
                "informacion del censo de US 2010"),
      selectInput("var",
                  label = "elija la variable a mostrar", 
                  choices = list("Porcentaje Blancos", "Porcentaje Negros",
                                 "Porcentaje Hispanicos", "Porcentaje Asiaticos"),
                  selected = "Porcentaje Hispanicos"),
      
      selectInput("color",
                  label = "elija el color de la grafica", 
                  choices = list("verde", "verde oscuro",
                                 "rojo", "azul", "amarillo"),
                  selected = "verde"
                  ),
      
      sliderInput("range", 
                  label = "rango de interes",
                  min = 0, max = 100, value = c(0,100)
                  )
      ),
      mainPanel(
        textOutput("text1"),
        textOutput("text2"),
        plotOutput("map"),
        tags$hr()
        )
    ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("var2",
                  label = "elija la variable a mostrar", 
                  choices = list("Porcentaje Blancos", "Porcentaje Negros",
                                 "Porcentaje Hispanicos", "Porcentaje Asiaticos"),
                  selected = "Porcentaje Hispanicos"),
      sliderInput("range2", 
                  label = "rango de interes",
                  min = 0, max = 100, value = c(0,100))
    ),
    mainPanel(      plotOutput("map2")    )
  )
)
# Define server logic ----
server <- function(input, output) {
  output$text1 <- renderText({
    paste("has seleccionado:", input$var)
  })
  
  output$text2 <- renderText({
    paste("en un rango de ", input$range[1], " a ", input$range[2])
  })
  
  output$map <- renderPlot({
    data <- switch(input$var,
                   "Porcentaje Blancos" = counties$white,
                   "Porcentaje Negros" = counties$black,
                   "Porcentaje Hispanicos" = counties$hispanic,
                   "Porcentaje Asiaticos" = counties$asian)
    color <- switch (input$color,
                    "verde" = "green",
                    "verde oscuro" = "darkgreen",
                    "rojo" = "red",
                    "azul" = "blue",
                    "amarillo" = "yellow")
    title <- input$var
    percent_map(data, color, title, input$range[1], input$range[2])
  })
  
  output$map2 <- renderPlot({
    args <- switch(input$var2,
                   "Porcentaje Blancos" = list(counties$white, "darkgreen", "% White"),
                   "Porcentaje Negros" = list(counties$black, "black", "% Black"),
                   "Porcentaje Hispanicos" = list(counties$hispanic, "darkorange", "% Hispanic"),
                   "Porcentaje Asiaticos" = list(counties$asian, "darkviolet", "% Asian"))
    args$min <- input$range2[1]
    args$max <- input$range2[2]
    do.call(percent_map, args)
  })
}

# Run the app ----
shinyApp(ui, server)
