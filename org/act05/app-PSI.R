#' SummaryPSI est une fonction calculant des statistiques a partir d'une série d'observations.
#' @param x le vecteur numérique d'observations
#' @return un vecteur numérique nommé contenant des indicateurs statistiques.
SummaryPSI <- function(x) {
  ## TODO
  return(c("N"=0))
}

#' HistPSI est une fonction construisant un histogramme d'une série d'observations.
#' @param x le vecteur numérique d'observations
#' @return un histogramme construit avec ggplot2
HistPSI <- function(x, main, binwidth=0.5) {
  ## TODO 
  if(require("ggplot2")) {
    ## ggplot est installé.
    qplot(c(), geom="histogram")
  } else {
    ## ggplot n'est pas installé. 
    ## On utilise les graphes de 'base'.
    hist(0:10)
  }
}

#' SampleMean renvoie la moyenne d'un sous-échantillon aléatoire de taille size d'un vecteur x.
#' @param x le vecteur numérique d'observations
#' @param size le nombre d'observations du sous-échantillon
#' @return la moyenne du sous-échantillon
SampleMean <- function(x, size) {
  ## TODO 
  return(0)
}

#' SampleMeans renvoie un vecteur de longueur n dont les éléments sont des estimations de la moyenne basées sur un échantillon aléatoire de taille size.
#' @param x le vecteur numérique d'observations
#' @param size le nombre de sous-échantillons
#' @param size le nombre d'observations du sous-échantillon
#' @return les moyennes des sous-échantillons
SampleMeans <- function(x, n, size) {
  ## TODO
  return(0)
}

library(shiny) 
#' La fonction server d'une application shiny réalise le traitement des données et la génération des graphiques/tableaux.
server <- function(input, output) {
  ## Vecteur réactif des observations qui dépend du fichier de données. 
  rtemp <- reactive({
    ## Validation du fichier de données
    validate(
      need(!is.null(input$tempfile), "Choisir un fichier CSV")
    )
    ## Lecture du fichier de données dans un vecteur
    scan(file = input$tempfile$datapath, quiet = TRUE)
  })


  ## Vecteur réactif des moyennes des sous-échantillons qui dépend du fichier de données et des entrées dans l'application. 
  rmean <- reactive({
    ## Validation des entrées saisies dans l'application.
    validate(
      need(input$sampleCount > 0, "Nombre d'échantillons négatif ou nul"),
      need(input$sampleSize > 0, "Taille d'échantillon négative ou nul")
    )
    ## Calcul des moyennes des sous-échantillons
    SampleMeans(rtemp(), input$sampleCount, input$sampleSize)
  }) 

  ## Une petite fonction utilitaire pour construire un tableau
  renderTablePSI <- function(x) {
    renderTable(
      SummaryPSI(x()),
      striped = TRUE, hover = TRUE, bordered = FALSE,
      spacing = "m", width = "auto", align = NULL,
      rownames = TRUE, colnames = FALSE
    )
  }

  ## Tableau récapitulatif des températures 
  output$summaryTemp <- renderTablePSI(rtemp)
  ## Histogramme des températures 
  output$histTemp <- renderPlot(
    HistPSI(rtemp(), "Histogramme des températures")
  )

  ## Tableau récapitulatif des moyennes estimées 
  output$summaryMean <- renderTablePSI(rmean)
  ## Histogramme des moyennes estimées 
  output$histMean <- renderPlot(
    HistPSI(rmean(), "Histogramme des moyennes estimées", binwidth=0.125)
  )
}

#' La fonction server d'une application shiny construit l'interface graphique à partir de ses entrées/sorties.
ui <- fluidPage(
  sidebarLayout(
    ## Barre latérale contenant les entrées de l'application
    sidebarPanel(
      fileInput(
        "tempfile", "Choisir un fichier CSV",
        accept = c(
          "text/csv",
            "text/comma-separated-values,text/plain",
          ".csv")
      ),
      numericInput("sampleCount", label = h3("Nombre d'échantillons"), min = 1, value = 100),
      numericInput("sampleSize", label = h3("Taille de l'échantillon"), min = 1, value = 10),
      width = 2
    ),
    ## Panneau principal contenant les sorties de l'application
    mainPanel(
      column(3, tableOutput("summaryTemp")),
      column(7, plotOutput("histTemp")),
      column(3, tableOutput("summaryMean")),
      column(7, plotOutput("histMean"))
    )
  )
)
## Construit un objet représentant l'application
shinyApp(ui = ui, server = server)
