library(moments)
library(ggplot2)



# target case$SalePrice  sulla y

# QUALITATIVA
# serve la devianza:
# interna (within) ai gruppi quanto sparsi all'interno dei gruppi, valore alto vuol dire dati più saprsi
# tra gruppi (between) quanto sono diversi le medie dei vari gruppi, valore alto vuol dire dati più saprsi
# eta_quadro quadro vicino a 1 : le dev_within hanno medie simili tra loro, ma le between hanno dati disparati


# se la devianza beetween è alta e la withnìin è bassa c'è correlazione
# infatti il eta_quadro è vicino a uno

aov(SalePrice ~ factor(BsmtExposure), data = case) 

# ci conferma le devianze e calcola anche il pvalue








# QUANTITATIVA 

# cov standardizzata = 0 non c'è correlazione lineare -> non posso fare lm e un modello

# correlazioni cor se è vicino a 1 o -1 faccio lm  perché c'é correlazione
# se invece è vicino allo zero non faccio nulla 



# se covarianza è zero 

# 




# Funzione per calcolare la devianza entro i gruppi e fra i gruppi
calcola_devianza <- function(numerical_var, categorical_var) {
  # Creiamo un dataframe temporaneo per facilitare i calcoli
  data <- data.frame(numerical_var, categorical_var)
  
  # Calcoliamo la media generale
  mean_total <- mean(data$numerical_var)
  
  # Devianza totale
  devianza_totale <- sum((data$numerical_var - mean_total)^2)
  
  # Calcolo della devianza tra i gruppi
  devianza_tra_gruppi <- 0
  livelli <- levels(data$categorical_var)
  for (livello in livelli) {
    gruppo <- data[data$categorical_var == livello,]
    n <- nrow(gruppo)
    mean_gruppo <- mean(gruppo$numerical_var)
    devianza_tra_gruppi <- devianza_tra_gruppi + n * (mean_gruppo - mean_total)^2
  }
  
  # Calcolo della devianza entro i gruppi
  devianza_entro_gruppi <- 0
  for (livello in livelli) {
    gruppo <- data[data$categorical_var == livello,]
    mean_gruppo <- mean(gruppo$numerical_var)
    devianza_entro_gruppi <- devianza_entro_gruppi + sum((gruppo$numerical_var - mean_gruppo)^2)
  }
  
  return(list(devianza_totale = devianza_totale, devianza_tra_gruppi = devianza_tra_gruppi, devianza_entro_gruppi = devianza_entro_gruppi))
}



factor((case$BsmtExposure))
calcola_devianza(case$SalePrice, factor(case$BsmtExposure))
