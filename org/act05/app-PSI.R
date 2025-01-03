#' SummaryPSI est une fonction calculant des statistiques a partir d'une série d'observations.
#' @param x le vecteur numérique d'observations
#' @return un vecteur numérique nommé contenant des indicateurs statistiques.
SummaryPSI <- function(x) {
  ## TODO
  return(c("N"=0))
}

#' HistPSI est une fonction construisant un histogramme d'une série d'observations.
#' @param x le vecteur numérique d'observations
#' @param main le titre de l'histogramme
#' @param binwidth la largeur d'une barre de l'histogramme
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
#' @param n le nombre de sous-échantillons
#' @param size le nombre d'observations du sous-échantillon
#' @return les moyennes des sous-échantillons
SampleMeans <- function(x, n, size) {
  ## TODO
  return(0)
}
