LireMise <- function() {
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

GainPlein <- function(numero, gagnant) {
  if(numero == gagnant) return(35)
  else return(0)
}

GainTransversale <- function(numero, gagnant) {
  if((numero-1) %/% 3 == (gagnant-1) %/% 3) return(11)
  else return(0)
}

GainColonne <- function(numero, gagnant) {
  if(numero %% 3 == gagnant %% 3) return(2)
  else return(0)
}

GainDouzaine <- function(numero, gagnant) {
  if((numero-1) %/% 12 == (gagnant-1) %/% 12) return(2)
  else return(0)
}

GainPairImpair <- function(numero, gagnant) {
  if(numero %% 2 == gagnant %% 2) return(1)
  else return(0)
}

GainManquePasse <- function(numero, gagnant) {
  if((numero-1) %/% 18 == (gagnant-1) %/% 18) return(1)
  else return(0)
}

GainRoulette <- function(mise, numero, gagnant) {
  ## Le return est implicite !
  if(mise == 1) GainPlein(numero, gagnant)
  else if(mise == 2) GainTransversale(numero, gagnant)
  else if(mise == 3) GainColonne(numero, gagnant)
  else if(mise == 4) GainDouzaine(numero, gagnant)
  else if(mise == 5) GainPairImpair(numero, gagnant)
  else if(mise == 6) GainManquePasse(numero, gagnant)
  else return(0)
}

JouerRoulette <- function() {
  ## L'utilisateur saisit sa mise
  mise <- LireMise()
  numero <- LireNumero()
  montant <- LireMontant()
  ## Le joueur peut miser sur 0 uniquement pour Plein.
  stopifnot( mise <= 1 || numero > 0)
  
  ## Le croupier tire le numéro gagnant
  gagnant <- TirerNumeroGagnant()
  cat("Le numéro gagnant est le ", gagnant, ".\n", sep = "")
  
  ## Le croupier détermine et affiche le montant gagné.
  gain <- montant * GainRoulette(mise, numero, gagnant)
  cat("Vous avez gagné ", gain, ".\n", sep = "")
  ## On renvoie le montant gagné par le joueur 
  return(gain)
}
