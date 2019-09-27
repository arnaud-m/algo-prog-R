source('gain-roulette.R')

LireType <- function() {
  ## Le return est implicite !
  menu( ## choisir une option dans l'interpréteur
    title = "Veuillez saisir le type de la mise : ", 
    choices = c("Plein", "Transversale", "Colonne", "Douzaine", "Pair-Impair", "Manque-Passe") )  
}

LireNumero <- function() {
  cat("Veuillez saisir un numéro de la mise :\n")
  numero <- scan( n = 1, quiet = TRUE)
  stopifnot(numero >= 0, numero <= 36)
  return(numero)
}

LireMontant <- function() {
  cat("Veuillez saisir le montant de la mise :\n")
  montant <- scan( n = 1, quiet = TRUE)
  stopifnot(montant >= 0)
  return(montant)
}

TirerNumeroGagnant <- function() {
  return(sample(0:36, size = 1))
}

JouerRoulette <- function() {
  ## L'utilisateur saisit sa mise
  type <- LireType()
  numero <- LireNumero()
  montant <- LireMontant()
  ## Le joueur peut miser sur 0 uniquement pour Plein.
  stopifnot( type <= 1 || numero > 0)
  
  ## Le croupier tire le numéro gagnant
  gagnant <- TirerNumeroGagnant()
  cat("Le numéro gagnant est le ", gagnant, ".\n", sep = "")
  
  ## Le croupier détermine le montant du gain.
  gain <- montant * GainRoulette(type, numero, gagnant)
  
  ## On affiche le montant du gain
  cat("Vous avez gagné ", gain, ".\n", sep = "")

  ## On renvoie le montant du gain  
  return(gain)
}
