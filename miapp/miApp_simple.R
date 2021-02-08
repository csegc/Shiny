## app.R ##
library(shinydashboard)
#library(ggplot2)
#library(lattice)

ui <- dashboardPage(
  dashboardHeader(title = "DashBoard SIEV"),

## Menu Columna izquierda
  dashboardSidebar(
    sidebarMenu(
      menuItem("Cargar Archivo", tabName = "upload", icon = icon("upload"))
    )
  ),
  
## Contenido de cada menu de barra izquierda
dashboardBody(
 tabItems(
  
  ## Contenido del Tab Cargar archivo
  tabItem(tabName = "upload",
      h2("vista Archivo"),
      tabsetPanel(
        tabPanel("cargar archivo",
          fileInput(inputId = "file", label = "Cargue el archivo de datos"),
          tags$hr(),
          checkboxInput(inputId = "header", label = "Encabezado", value = TRUE),
          radioButtons(inputId = "sep" , label= "Separador", 
                       choices = c(Coma = ",", Punto_Coma = ";", Tab = "\t", Espacio = " "),
                       selected = ","),
          tags$hr(),
          radioButtons("disp", "Mostrar",
                       choices = c(Head = "head",
                                   All = "all"),
                       selected = "head")
        ),
        tabPanel("Propiedades archivo",
          tabsetPanel(
            tabPanel("Propiedades", tableOutput("filedf")),
            tabPanel("Datos", tableOutput("table")),
            tabPanel("Resumen", verbatimTextOutput("sum", placeholder = TRUE))))
      )
      )
  )
)
)

server <- function(input, output) {

  ## manejo del archivo cargado
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read.table(file = file1$datapath, sep = input$sep, header = input$header,  
                    colClasses = NA)})
  
  #Propiedades del archivo
   df <- reactive ({
     output$filedf <-renderTable({
      if(is.null(data())){return()}
      input$file})
      })
     
  #Datos del Archivo mostrado en tabla
  output$table <- renderTable({
    if(is.null(data())){return()}
    if(input$disp == "head") {return(head(data()))}
    else {return(data())}
    })
  #Resumen del archivo
  output$sum <- renderPrint({
    if(is.null(data())){return()}
    summary(data())})
}

shinyApp(ui, server)