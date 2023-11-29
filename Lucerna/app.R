library(shiny)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Escenarios de desarrollo demográfico para el cantón de Lucerna"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("scenario", "Seleccione un escenario:",
                  choices = c("Alto" = "hoch", "Medio" = "referenz", "Bajo" = "tief", "Todos los escenarios" = "all")),
      selectInput("year", "Seleccione un año:", choices = c("Todos los años" = "all")),
      selectizeInput("municipality", "Seleccione un municipio:", choices = NULL),
      selectInput("variable", "Seleccione una variable para el eje X:",
                  choices = c("Población Inicial" = "swb0", "Nacimientos" = "geb",
                              "Muertes" = "tod", "Movimientos Espaciales" = "ws",
                              "Inmigración" = "ein", "Población Final" = "swb")),
      selectInput("age", "Seleccione una edad:", choices = c("Todas las edades" = "all")),
      selectInput("sex", "Seleccione el sexo:", choices = c("Todos" = "all", "Hombre" = "m", "Mujer" = "w")),
      selectInput("nationality", "Seleccione la nacionalidad:", choices = c("Todas" = "all")),
      selectInput("variableY", "Seleccione una variable para el eje Y (opcional):",
                  choices = c("Ninguna" = "", "Población Inicial" = "swb0", "Nacimientos" = "geb",
                              "Muertes" = "tod", "Movimientos Espaciales" = "ws",
                              "Inmigración" = "ein", "Población Final" = "swb"), selected = ""),
      sliderInput("bins", "Número de divisiones:", min = 1, max = 50, value = 30)
    ),
    mainPanel(plotOutput("distPlot"))
  )
)

server <- function(input, output, session) {
  data_reactive <- reactive({
    req(input$scenario)
    if (input$scenario == "all") {
      # Combina los datos de todos los escenarios
      data_hoch <- read.csv("szbv-lu-2020-2050-hoch.csv", sep = ";")
      data_referenz <- read.csv("szbv-lu-2020-2050-referenz.csv", sep = ";")
      data_tief <- read.csv("szbv-lu-2020-2050-tief.csv", sep = ";")
      rbind(data_hoch, data_referenz, data_tief)
    } else {
      # Lee los datos para el escenario seleccionado
      read.csv(paste0("szbv-lu-2020-2050-", input$scenario, ".csv"), sep = ";")
    }
  })
  
  observe({
    data <- data_reactive()
    updateSelectInput(session, "year", choices = c("Todos los años" = "all", unique(data$jahr)))
    updateSelectizeInput(session, "municipality", choices = c("Todos los municipios" = "all", unique(data$gemeinde)), server = TRUE)
    updateSelectInput(session, "age", choices = c("Todas las edades" = "all", unique(data$alter)))
    updateSelectInput(session, "nationality", choices = c("Todas" = "all", unique(data$nation)))
  })
  
  output$distPlot <- renderPlot({
    data <- data_reactive()
    if (input$year != "all") {
      data <- data[data$jahr == input$year, ]
    }
    if (input$municipality != "all") {
      data <- data[data$gemeinde == input$municipality, ]
    }
    if (input$age != "all") {
      data <- data[data$alter == input$age, ]
    }
    if (input$sex != "all") {
      data <- data[data$sex == input$sex, ]
    }
    if (input$nationality != "all") {
      data <- data[data$nation == input$nationality, ]
    }
    
    var <- input$variable
    varY <- input$variableY
    if (varY != "") {
    ggplot(data, aes_string(x = var, y = varY)) + 
        geom_point(alpha = 0.7, color = "blue") +
      ggtitle(paste(var, "vs.", varY,
                    ifelse(input$year == "all", "para todos los años", paste("para el año", input$year)),
                    ifelse(input$municipality == "all", "", paste("en el municipio", input$municipality)),
                    ifelse(input$age == "all", "", paste("para la edad", input$age)),
                    ifelse(input$sex == "all", "", paste("para el sexo", input$sex)),
                    ifelse(input$nationality == "all", "", paste("para la nacionalidad", input$nationality))
      )) +
      xlab(var) + ylab(varY)
    }else{
      ggplot(data, aes_string(x = var)) + 
        geom_histogram(bins = input$bins, fill = "blue", alpha = 0.7) +
        ggtitle(paste("Distribución de", var, 
                      ifelse(input$year == "all", "para todos los años", paste("para el año", input$year)),
                      ifelse(input$municipality == "all", "", paste("en el municipio", input$municipality)),
                      ifelse(input$age == "all", "", paste("para la edad", input$age)),
                      ifelse(input$sex == "all", "", paste("para el sexo", input$sex)),
                      ifelse(input$nationality == "all", "", paste("para la nacionalidad", input$nationality))
        )) +
        xlab(var) + ylab("Frecuencia")
    }
  })
}

shinyApp(ui = ui, server = server)
