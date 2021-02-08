## app.R ##
library(shinydashboard)
library(ggplot2)
#library(lattice)

ui <- dashboardPage(
  
  dashboardHeader(title = "DashBoard SIEV"),
  
## Menu Columna izquierda
  dashboardSidebar(
    sidebarMenu(
      
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th")),
      menuItem("Cargar Archivo", tabName = "upload", icon = icon("upload"))
    )
  ),
  
  
## Contenido de cada menu de barra izquierda
dashboardBody(
 tabItems(
  ## Contenido de primer Tab
  tabItem(tabName = "dashboard",
    # las cajas se ponen en filas
        fluidRow(
          box(plotOutput("plot1", height = 250)),
          box(title = "Controls", sliderInput("slider1", "Number of observations:", 1, 100, 50))
        )
      ),
  
  ## Contenido del segundo Tab
  tabItem(tabName = "widgets",
    h2("COntenidos de apps"),
    tabsetPanel(
      tabPanel("panel1", 
               box(plotOutput("plot2", height = 250)),
               box(title = "Controls", sliderInput("slider2", "Number of observations:", 1, 100, 50))),
      tabPanel("panel2", "contenido2"),
      tabPanel("panel3", "contenido3")
    )
  ),
  
  ## COntenido del tercer Tab Cargar archivo
  tabItem(tabName = "upload",
      h2("vista Archivo"),
      tabsetPanel(
        tabPanel("cargar archivo",
          fileInput(inputId = "file", label = "Cargue el archivo de datos"),
          checkboxInput(inputId = "header", label = "Encabezado", value = FALSE),
          radioButtons(inputId = "sep" , label= "Separador", 
                       choices = c(Coma = ",", Punto_Coma = ";", Tab = "\t", Espacio = " "),
                       selected = ",")
        ),
        tabPanel("Propiedades archivo",
          mainPanel(
          uiOutput("tb"))
        )
      )
      )
  )
)
)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider1)]
    hist(data)
  })

  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider2)]
    hist(data)
  })
  
  ## manejo del archivo cargado
  data <- reactive({
    file1 <- input$file
    if(is.null(file1)){return()}
    read.table(file = file1$datapath, sep = input$sep,
               header = input$header)})
  
  #Propiedades del archivo
  output$filedf <- renderTable({
    if(is.null(data())){return()}
    input$file})
  #Datos del Archivo mostrado en tabla
  output$table <- renderTable({
    if(is.null(data())){return()}
    data()})
  #Resumen del archivo
  output$sum <- renderPrint({
    if(is.null(data())){return()}
    summary(data())})
  
  output$table2 <- DT::renderDataTable({
    if(is.null(data())){return()}
    data()})
  
  output$tb <- renderUI({
    tabsetPanel(
      tabPanel("Propiedades", tableOutput("filedf")),
      tabPanel("Datos", tableOutput("table")),
      tabPanel("Resumen", verbatimTextOutput("sum")),
      tabPanel("manejo datos", dataTableOutput("table2")))
  })
  
  
  }

shinyApp(ui, server)