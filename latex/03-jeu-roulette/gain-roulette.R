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
