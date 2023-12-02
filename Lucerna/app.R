library(ggplot2)
library(tidyverse)
library(corrplot)
library(FactoMineR)
library(factoextra)
library(lawstat)
library(psych)
library(corrr)
library(ltm)

ui <- fluidPage(
  titlePanel("Escenarios de desarrollo demográfico para el cantón de Lucerna"),
  tabsetPanel(
    tabPanel("Distribuciones",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "scenario",
                   "Seleccione un escenario:",
                   choices = c(
                     "Alto" = "hoch",
                     "Medio" = "referenz",
                     "Bajo" = "tief",
                     "Todos los escenarios" = "all"
                   )
                 ),
                 selectInput(
                   "yearStart",
                   "Seleccione el año de inicio:",
                   choices = 2020:2050,
                   selected = 2020
                 ),
                 selectInput(
                   "yearEnd",
                   "Seleccione el año de finalización:",
                   choices = 2020:2050,
                   selected = 2050
                 ),
                 selectInput(
                   "municipalityGroup",
                   "Seleccione los municipios:",
                   choices = NULL,
                   selected = NULL,
                   multiple = TRUE,
                 ),
                 actionButton("deselectAll", "Deseleccionar todos los municipios"),
                 selectInput(
                   "variable",
                   "Seleccione una variable para el eje X:",
                   choices = c(
                     "Población Inicial" = "swb0",
                     "Nacimientos" = "geb",
                     "Muertes" = "tod",
                     "Movimientos Espaciales" = "ws",
                     "Inmigración" = "ein",
                     "Población Final" = "swb"
                   )
                 ),
                 selectInput(
                   "ageStart",
                   "Seleccione la edad de inicio:",
                   choices = 0:100,
                   selected = 0
                 ),
                 selectInput(
                   "ageEnd",
                   "Seleccione la edad de finalización:",
                   choices = 0:100,
                   selected = 100
                 ),
                 selectInput(
                   "sex",
                   "Seleccione el sexo:",
                   choices = c(
                     "Todos" = "all",
                     "Hombre" = "m",
                     "Mujer" = "w"
                   )
                 ),
                 selectInput(
                   "nationality",
                   "Seleccione la nacionalidad:",
                   choices = c("Todas" = "all")
                 ),
                 selectInput(
                   "variableY",
                   "Seleccione una variable para el eje Y (opcional):",
                   choices = c(
                     "Ninguna" = "",
                     "Población Inicial" = "swb0",
                     "Nacimientos" = "geb",
                     "Muertes" = "tod",
                     "Movimientos Espaciales" = "ws",
                     "Inmigración" = "ein",
                     "Población Final" = "swb"
                   ),
                   selected = ""
                 ),
                 selectInput(
                   "variableC",
                   "Seleccione una variable cualitativa:",
                   choices = c(
                     "Ninguna" = "",
                     "Genero" = "sex",
                     "Nacionalidad" = "nation",
                     "Municipio" = "gemeinde"
                   ),
                   selected = ""
                 ),
                 sliderInput(
                   "bins",
                   "Número de divisiones:",
                   min = 1,
                   max = 50,
                   value = 30
                 )
               ),
               mainPanel(plotOutput("distPlot"))
             )),
    tabPanel("Correlaciones",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "scenarioCor",
                   "Seleccione un escenario:",
                   choices = c(
                     "Alto" = "hoch",
                     "Medio" = "referenz",
                     "Bajo" = "tief",
                     "Todos los escenarios" = "all"
                   )
                 ),
                 selectInput(
                   "yearCor",
                   "Seleccione el año de inicio:",
                   choices = 2020:2050,
                   selected = 2020
                 ),
                 selectInput(
                   "ageStartCor",
                   "Seleccione la edad de inicio:",
                   choices = 0:100,
                   selected = 0
                 ),
                 selectInput(
                   "ageEndCor",
                   "Seleccione la edad de finalización:",
                   choices = 0:100,
                   selected = 100
                 )
               ),
               mainPanel(
                 plotOutput("matrixcorrelation"),
                 verbatimTextOutput("correlation"),
               )
             )),
    tabPanel(
      "Reducción de Dimensiones",
      tabsetPanel(
        tabPanel("ACP",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       "scenarioTab2",
                       "Seleccione un escenario:",
                       choices = c(
                         "Alto" = "hoch",
                         "Medio" = "referenz",
                         "Bajo" = "tief",
                         "Todos los escenarios" = "all"
                       )
                     ),
                     selectInput(
                       "yearStartTab2",
                       "Seleccione el año de inicio:",
                       choices = 2020:2050,
                       selected = 2020
                     ),
                     selectInput(
                       "yearEndTab2",
                       "Seleccione el año de finalización:",
                       choices = 2020:2050,
                       selected = 2050
                     ),
                     selectInput(
                       "ageStartTab2",
                       "Seleccione la edad de inicio:",
                       choices = 0:100,
                       selected = 0
                     ),
                     selectInput(
                       "ageEndTab2",
                       "Seleccione la edad de finalización:",
                       choices = 0:100,
                       selected = 100
                     ),
                     selectInput(
                       "sampleSize",
                       "Tamaño de la muestra:",
                       choices = 10:10000,
                       selected = 1000
                     ),
                     numericInput(
                       "numClusters",
                       "Número de Clusters (ingrese -1 para automático):",
                       value = -1,
                       min = -1
                     )
                   ),
                   mainPanel(
                     plotOutput("method"),
                     plotOutput("eigenPlot"),
                     plotOutput("optimal"),
                     plotOutput("clusterPlot")
                   )
                 )),
        tabPanel("ACM", ),
        tabPanel("AF",
                 sidebarLayout(
                   sidebarPanel(
                     selectInput(
                       "scenarioAF",
                       "Seleccione un escenario:",
                       choices = c(
                         "Alto" = "hoch",
                         "Medio" = "referenz",
                         "Bajo" = "tief",
                         "Todos los escenarios" = "all"
                       )
                     ),
                     selectInput(
                       "yearStartTab3",
                       "Seleccione el año de inicio:",
                       choices = 2020:2050,
                       selected = 2020
                     ),
                     selectInput(
                       "yearEndTab3",
                       "Seleccione el año de finalización:",
                       choices = 2020:2050,
                       selected = 2050
                     ),
                     selectInput(
                       "varSelect",
                       "Seleccione las variables para el análisis:",
                       choices = NULL,
                       multiple = TRUE,
                       selected = NULL
                     ),
                     numericInput(
                       "nFactors",
                       "Número de Factores:",
                       value = 2,
                       min = 1
                     ),
                     selectInput(
                       "sampleSizeTab3",
                       "Tamaño de la muestra:",
                       choices = 10:10000,
                       selected = 1000
                     ),
                     actionButton("runAnalysis", "Ejecutar Análisis")
                   ),
                   mainPanel(
                     plotOutput("screePlot"),
                     plotOutput("parallel"),
                     plotOutput("load"),
                     plotOutput("factorPlot")
                   )
                 ))
      )
    ),
    tabPanel("Clustering",
             sidebarLayout(
               sidebarPanel(
                 # Inputs para seleccionar opciones de clustering
                 selectInput(
                   "clusteringScenario",
                   "Seleccione un escenario:",
                   choices = c(
                     "Alto" = "hoch",
                     "Medio" = "referenz",
                     "Bajo" = "tief",
                     "Todos los escenarios" = "all"
                   )
                 ),
                 selectInput(
                   "yearStartTab4",
                   "Seleccione el año de inicio:",
                   choices = 2020:2050,
                   selected = 2020
                 ),
                 selectInput(
                   "yearEndTab4",
                   "Seleccione el año de finalización:",
                   choices = 2020:2050,
                   selected = 2050
                 ),
                 selectInput(
                   "ageStartTab4",
                   "Seleccione la edad de inicio:",
                   choices = 0:100,
                   selected = 0
                 ),
                 selectInput(
                   "ageEndTab4",
                   "Seleccione la edad de finalización:",
                   choices = 0:100,
                   selected = 100
                 ),
                 selectInput(
                   "sampleSizeTab4",
                   "Tamaño de la muestra:",
                   choices = 10:10000,
                   selected = 1000
                 ),
                 numericInput(
                   "numClustersClustering",
                   "Número de Clusters:",
                   value = 3,
                   min = 2
                 ),
                 actionButton("runClustering", "Ejecutar Clustering")
               ),
               mainPanel(
                 plotOutput("clusteringPlot"),
                 plotOutput("clusteringPlotKmeans")
               )
             ))
    
  )
)

server <- function(input, output, session) {
  observeEvent(input$deselectAll, {
    updateCheckboxGroupInput(session, "municipalityGroup", selected = character(0))
  })
  
  convert_to_numeric <- function(data, scenario) {
    data$sex <-
      ifelse(data$sex == "m", 0, ifelse(data$sex == "w", 1, NA))
    data$nation <-
      ifelse(data$nation == "A", 0, ifelse(data$nation == "CH", 1, NA))
    if (scenario == "all") {
      data$variante <-
        ifelse(data$variante == "tief",
               0,
               ifelse(
                 data$variante == "referenz",
                 1,
                 ifelse(data$variante == "hoch", 2, NA)
               ))
    }
    return(data)
  }
  
  data_reactive <- reactive({
    req(input$scenario)
    if (input$scenario == "all") {
      # Combina los datos de todos los escenarios
      data_hoch <-
        read.csv("szbv-lu-2020-2050-hoch.csv",
                 sep = ";",
                 fileEncoding = "latin1")
      data_referenz <-
        read.csv("szbv-lu-2020-2050-referenz.csv",
                 sep = ";",
                 fileEncoding = "latin1")
      data_tief <-
        read.csv("szbv-lu-2020-2050-tief.csv",
                 sep = ";",
                 fileEncoding = "latin1")
      rbind(data_hoch, data_referenz, data_tief)
    } else {
      # Lee los datos para el escenario seleccionado
      read.csv(
        paste0("szbv-lu-2020-2050-", input$scenario, ".csv"),
        sep = ";",
        fileEncoding = "latin1"
      )
    }
  })
  
  observe({
    data <- data_reactive()
    updateSelectInput(
      session,
      "municipalityGroup",
      choices = unique(data$gemeinde),
      selected = unique(data$gemeinde)
    )
    updateSelectInput(session, "nationality", choices = c("Todas" = "all", unique(data$nation)))
    updateSelectInput(
      session,
      "varSelect",
      choices = setdiff(names(data), c("ktnr", "kt", "gemeinde")),
      selected = c("gnr", "geb", "tod", "ws", "ein", "swb")
    )
  })
  
  output$distPlot <- renderPlot({
    data <- data_reactive()
    data <-
      data[data$jahr >= input$yearStart &
             data$jahr <= input$yearEnd, ]
    data <-
      data[data$alter >= input$ageStart &
             data$alter <= input$ageEnd, ]
    if (!"all" %in% input$municipalityGroup) {
      data <- data[data$gemeinde %in% input$municipalityGroup, ]
    }
    if (input$sex != "all") {
      data <- data[data$sex == input$sex, ]
    }
    if (input$nationality != "all") {
      data <- data[data$nation == input$nationality, ]
    }
    
    var <- input$variable
    varY <- input$variableY
    varC <- input$variableC
    
    if (varY != "") {
      if (varC != "") {
        ggplot(data, aes_string(x = var, y = varY, col = varC)) +
          geom_point(alpha = 0.7) +
          ggtitle(paste(
            var,
            "vs.",
            varY,
            ifelse(
              input$year == "all",
              "para todos los años",
              paste("para el año", input$year)
            ),
            ifelse(input$age == "all", "", paste("para la edad", input$age)),
            ifelse(input$sex == "all", "", paste("para el sexo", input$sex)),
            ifelse(
              input$nationality == "all",
              "",
              paste("para la nacionalidad", input$nationality)
            )
          )) +
          xlab(var) + ylab(varY)
      } else{
        ggplot(data, aes_string(x = var, y = varY)) +
          geom_point(alpha = 0.7, fill = "blue") +
          ggtitle(paste(
            var,
            "vs.",
            varY,
            ifelse(
              input$year == "all",
              "para todos los años",
              paste("para el año", input$year)
            ),
            ifelse(input$age == "all", "", paste("para la edad", input$age)),
            ifelse(input$sex == "all", "", paste("para el sexo", input$sex)),
            ifelse(
              input$nationality == "all",
              "",
              paste("para la nacionalidad", input$nationality)
            )
          )) +
          xlab(var) + ylab(varY)
      }
    } else{
      if (varC != "") {
        ggplot(data, aes_string(x = var, fill = varC)) +
          geom_histogram(bins = input$bins,
                         alpha = 0.7) +
          ggtitle(paste(
            "Distribución de",
            var,
            ifelse(
              input$year == "all",
              "para todos los años",
              paste("para el año", input$year)
            ),
            ifelse(input$age == "all", "", paste("para la edad", input$age)),
            ifelse(input$sex == "all", "", paste("para el sexo", input$sex)),
            ifelse(
              input$nationality == "all",
              "",
              paste("para la nacionalidad", input$nationality)
            )
          )) +
          xlab(var) + ylab("Frecuencia")
      } else{
        ggplot(data, aes_string(x = var)) +
          geom_histogram(bins = input$bins,
                         fill = "blue",
                         alpha = 0.7) +
          ggtitle(paste(
            "Distribución de",
            var,
            ifelse(
              input$year == "all",
              "para todos los años",
              paste("para el año", input$year)
            ),
            ifelse(input$age == "all", "", paste("para la edad", input$age)),
            ifelse(input$sex == "all", "", paste("para el sexo", input$sex)),
            ifelse(
              input$nationality == "all",
              "",
              paste("para la nacionalidad", input$nationality)
            )
          )) +
          xlab(var) + ylab("Frecuencia")
      }
    }
  })
  
  data_cor <- reactive({
    req(input$scenarioCor)
    if (input$scenarioCor == "all") {
      # Combina los datos de todos los escenarios
      data_hoch <-
        read.csv("szbv-lu-2020-2050-hoch.csv",
                 sep = ";",
                 fileEncoding = "latin1")
      data_referenz <-
        read.csv(
          "szbv-lu-2020-2050-referenz.csv",
          sep = ";",
          fileEncoding = "latin1"
        )
      data_tief <-
        read.csv("szbv-lu-2020-2050-tief.csv",
                 sep = ";",
                 fileEncoding = "latin1")
      data <- rbind(data_hoch, data_referenz, data_tief)
    } else {
      # Lee los datos para el escenario seleccionado
      data <-
        read.csv(
          paste0(
            "szbv-lu-2020-2050-",
            input$scenarioCor,
            ".csv"
          ),
          sep = ";",
          fileEncoding = "latin1"
        )
    }
    data <- convert_to_numeric(data, input$scenarioCor)

    data <-
      data[data$alter >= input$ageStartCor &
             data$alter <= input$ageEndCor,]
    selected_vars <-
      c("jahr",
        "gnr",
        "alter",
        "sex",
        "nation",
        "swb0",
        "geb",
        "tod",
        "ws",
        "ein",
        "swb")
    data <- data[, selected_vars]
    data
  })
  
  generar_matriz_correlaciones <- function(data, año_numero) {
    
    # Filtrar registros según el año proporcionado
    data_filtrado <- subset(data, jahr == año_numero)
    data_filtrado <- subset(data_filtrado, select = -c(jahr))
    
    # Obtener los nombres de las variables numéricas
    nombres_variables <- names(data_filtrado)
    
    # Crear una matriz de correlaciones con nombres de variables
    matriz_correlaciones <- matrix(NA, nrow = length(nombres_variables), ncol = length(nombres_variables))
    rownames(matriz_correlaciones) <- nombres_variables
    colnames(matriz_correlaciones) <- nombres_variables
    
    # Llenar la matriz de correlaciones
    for (i in 1:length(nombres_variables)) {
      for (j in 1:length(nombres_variables)) {
        # Calcular la correlación si ambas variables son numéricas
        if (is.numeric(data_filtrado[, i]) && is.numeric(data_filtrado[, j])) {
          matriz_correlaciones[i, j] <- round(cor(data_filtrado[, i], data_filtrado[, j]), 2)
        } 
        else if (names(data_filtrado)[i] == "sex" && names(data_filtrado)[j] != "nation"
                 && names(data_filtrado)[j] != "sex"){
          matriz_correlaciones[i, j] <- round(biserial.cor(data_filtrado[,j], data_filtrado[,i], use = c("all.obs"), level = 2), 2)
        }
        else if (names(data_filtrado)[j] == "sex" && names(data_filtrado)[i] != "nation"
                 && names(data_filtrado)[i] != "sex"){
          matriz_correlaciones[i, j] <- round(biserial.cor(data_filtrado[,i], data_filtrado[,j], use = c("all.obs"), level = 2), 2)
        }
        else if (names(data_filtrado)[i] == "nation" && names(data_filtrado)[j] != "sex"
                 && names(data_filtrado)[j] != "nation"){
          matriz_correlaciones[i, j] <- round(biserial.cor(data_filtrado[,j], data_filtrado[,i], use = c("all.obs"), level = 2), 2)
        }
        else if (names(data_filtrado)[j] == "nation" && names(data_filtrado)[i] != "sex"
                 && names(data_filtrado)[i] != "nation"){
          matriz_correlaciones[i, j] <- round(biserial.cor(data_filtrado[,i], data_filtrado[,j], use = c("all.obs"), level = 2), 2)
        }
        else if (names(data_filtrado)[i] == names(data_filtrado)[j]){
          matriz_correlaciones[i, j] <- c(1)
        }
        
        else {
          matriz_correlaciones[i, j] <- c(0)
        } 
      }
    }
    # Retornar la matriz de correlaciones con nombres de variables
    return(matriz_correlaciones)
  }
  
  output$matrixcorrelation <- renderPlot({
    data <- data_cor()
    matriz_cor <- generar_matriz_correlaciones(data, input$yearCor)
    corrplot(matriz_cor, method = "circle")
  })
  
  output$correlation <- renderPrint({
    data <- data_cor()
    matriz_cor <- generar_matriz_correlaciones(data, input$yearCor)
    matriz_cor
  })
  
  pca_data_reactive <- reactive({
    req(input$scenarioTab2)
    data <- if (input$scenarioTab2 == "all") {
      # Combina los datos de todos los escenarios
      data_hoch <-
        read.csv("szbv-lu-2020-2050-hoch.csv",
                 sep = ";",
                 fileEncoding = "latin1")
      data_referenz <-
        read.csv("szbv-lu-2020-2050-referenz.csv",
                 sep = ";",
                 fileEncoding = "latin1")
      data_tief <-
        read.csv("szbv-lu-2020-2050-tief.csv",
                 sep = ";",
                 fileEncoding = "latin1")
      data <- rbind(data_hoch, data_referenz, data_tief)
    } else {
      # Lee los datos para el escenario seleccionado
      data <-
        read.csv(
          paste0("szbv-lu-2020-2050-", input$scenarioTab2, ".csv"),
          sep = ";",
          fileEncoding = "latin1"
        )
    }
    
    data <-
      data[data$jahr >= input$yearStartTab2 &
             data$jahr <= input$yearEndTab2,]
    data <-
      data[data$alter >= input$ageStartTab2 &
             data$alter <= input$ageEndTab2,]
    data <- convert_to_numeric(data, input$scenarioTab2)
    selected_vars <-
      c("jahr",
        "gnr",
        "alter",
        "sex",
        "nation",
        "swb0",
        "geb",
        "tod",
        "ws",
        "ein",
        "swb")
    data <- data[, selected_vars]
    sample_size <- min(nrow(data), input$sampleSize)
    data_sample <- data[sample(nrow(data), sample_size),]
    data_sample
  })
  # Función para realizar PCA
  perform_pca <- reactive({
    data_sample <- pca_data_reactive()
    numeric_vars <- data_sample[, sapply(data_sample, is.numeric)]
    res.pca <- PCA(numeric_vars, ncp = 5, scale.unit = TRUE)
    res.pca
  })
  
  # Gráfico de varianza explicada
  output$method <- renderPlot({
    perform_pca()
  })
  
  output$eigenPlot <- renderPlot({
    res.pca <- perform_pca()
    fviz_eig(res.pca)
  })
  
  # Optimal clusters PCA
  output$optimal <- renderPlot({
    data_sample <- pca_data_reactive()
    numeric_vars <- data_sample[, sapply(data_sample, is.numeric)]
    # Determinar el número óptimo de clusters
    # Aquí, puedes modificar según tus necesidades, por ejemplo, utilizando métodos diferentes
    fviz_nbclust(numeric_vars, FUN = hcut, method = "wss")
  })
  
  # Clasificación jerárquica y visualización de clusters
  output$clusterPlot <- renderPlot({
    data_sample <- pca_data_reactive()
    numeric_vars <- data_sample[, sapply(data_sample, is.numeric)]
    res.pca <- perform_pca()
    
    # Ejecutar la clasificación jerárquica
    set.seed(123)  # Asegura la reproducibilidad
    res.hcpc <-
      HCPC(res.pca,
           nb.clust = input$numClusters,
           graph = FALSE)  # nb.clust = -1 determina el número automáticamente
    
    # Visualizar los clusters
    fviz_cluster(res.hcpc, data = numeric_vars)
  })
  
  data_reactive_AF <- reactive({
    req(input$scenarioAF)
    if (input$scenarioAF == "all") {
      # Combina los datos de todos los escenarios
      data_hoch <-
        read.csv("szbv-lu-2020-2050-hoch.csv",
                 sep = ";",
                 fileEncoding = "latin1")
      data_referenz <-
        read.csv("szbv-lu-2020-2050-referenz.csv",
                 sep = ";",
                 fileEncoding = "latin1")
      data_tief <-
        read.csv("szbv-lu-2020-2050-tief.csv",
                 sep = ";",
                 fileEncoding = "latin1")
      rbind(data_hoch, data_referenz, data_tief)
    } else {
      # Lee los datos para el escenario seleccionado
      read.csv(
        paste0("szbv-lu-2020-2050-", input$scenarioAF, ".csv"),
        sep = ";",
        fileEncoding = "latin1"
      )
    }
  })
  
  # Server AF
  observeEvent(input$runAnalysis, {
    req(input$varSelect)
    data_for_af <- reactive({
      data <- data_reactive_AF()
      data <- convert_to_numeric(data, input$scenarioAF)
      selected_vars <- input$varSelect
      if (length(selected_vars) < 2) {
        selected_vars <- c("gnr", "geb", "tod", "ws", "ein", "swb")
      }
      data <-
        data[data$jahr >= input$yearStartTab3 &
               data$jahr <= input$yearEndTab3,]
      data <- data[, selected_vars]
      data[sample(nrow(data), input$sampleSizeTab3),]
    })
    
    output$screePlot <- renderPlot({
      mydata <- data_for_af()
      scree(mydata, pc = FALSE)
    })
    
    output$parallel <- renderPlot({
      mydata <- data_for_af()
      fa.parallel(mydata, fa = "fa")
    })
    
    res_af <- reactive({
      mydata <- data_for_af()
      factanal(mydata, input$nFactors, rotation = "promax")
    })
    
    output$load <- renderPlot({
      res <- res_af()
      mydata <- data_for_af()
      load <- res$loadings[, input$nFactors]
      plot(load, type = "n") + text(load, labels = names(mydata), cex =
                                      .7)
    })
    
    output$factorPlot <- renderPlot({
      res <- res_af()
      fa.diagram(res$loadings)
    })
    
  })
  
  observeEvent(input$runClustering, {
    data_clustering <- reactive({
      req(input$clusteringScenario)
      if (input$clusteringScenario == "all") {
        # Combina los datos de todos los escenarios
        data_hoch <-
          read.csv("szbv-lu-2020-2050-hoch.csv",
                   sep = ";",
                   fileEncoding = "latin1")
        data_referenz <-
          read.csv(
            "szbv-lu-2020-2050-referenz.csv",
            sep = ";",
            fileEncoding = "latin1"
          )
        data_tief <-
          read.csv("szbv-lu-2020-2050-tief.csv",
                   sep = ";",
                   fileEncoding = "latin1")
        data <- rbind(data_hoch, data_referenz, data_tief)
      } else {
        # Lee los datos para el escenario seleccionado
        data <-
          read.csv(
            paste0(
              "szbv-lu-2020-2050-",
              input$clusteringScenario,
              ".csv"
            ),
            sep = ";",
            fileEncoding = "latin1"
          )
      }
      data <- convert_to_numeric(data, input$clusteringScenario)
      data <-
        data[data$jahr >= input$yearStartTab4 &
               data$jahr <= input$yearEndTab4,]
      data <-
        data[data$alter >= input$ageStartTab4 &
               data$alter <= input$ageEndTab4,]
      selected_vars <-
        c("jahr",
          "gnr",
          "alter",
          "sex",
          "nation",
          "swb0",
          "geb",
          "tod",
          "ws",
          "ein",
          "swb")
      data <- data[, selected_vars]
      sample_size <- min(nrow(data), input$sampleSizeTab4)
      data_sample <- data[sample(nrow(data), sample_size),]
      data_sample
    })
    
    output$clusteringPlot <- renderPlot({
      data_sample <- data_clustering()
      numeric_vars <- data_sample[, sapply(data_sample, is.numeric)]
      
      # Ejecutar la clasificación jerárquica
      set.seed(123)  # Asegura la reproducibilidad
      hcpc <-
        HCPC(numeric_vars,
             nb.clust = input$numClustersClustering,
             graph = FALSE)  # nb.clust = -1 determina el número automáticamente
      
      # Visualizar los clusters
      fviz_cluster(hcpc, data = numeric_vars)
    })
    
    output$clusteringPlotKmeans <- renderPlot({
      data <- data_clustering()
      numeric_vars <- data[, sapply(data, is.numeric)]
      set.seed(123) # Para reproducibilidad
      clusters <-
        kmeans(numeric_vars, centers = input$numClustersClustering)
      
    })
  })
  
  
}

shinyApp(ui = ui, server = server)
