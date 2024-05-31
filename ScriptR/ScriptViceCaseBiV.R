library(moments)
library(ggplot2)

case <- read.csv("~/Documents/UNI/2_anno_23_24/Analytics/6_HW2/Dati/train.csv", header=TRUE)
prezzi <- case$SalePrice
case <- case[, 22:41]
case$SalePrice <- prezzi

# Funzione per calcolare la devianza entro i gruppi e fra i gruppi
calcola_devianza <- function(numerical_var, categorical_var) {
  # Creiamo un dataframe temporaneo per facilitare i calcoli
  data <- data.frame(numerical_var, categorical_var)
  
  # Calcoliamo la media generale
  mean_total <- mean(data$numerical_var, na.rm = T)
  
  # Devianza totale
  devianza_totale <- sum((data$numerical_var - mean_total)^2)
  
  # Calcolo della devianza tra i gruppi
  devianza_tra_gruppi <- 0
  livelli <- levels(data$categorical_var)
  for (livello in livelli) {
    gruppo <- data[data$categorical_var == livello, ]
    n <- nrow(gruppo)
    mean_gruppo <- mean(gruppo$numerical_var, na.rm = T)
    devianza_tra_gruppi <- devianza_tra_gruppi + n * (mean_gruppo - mean_total)^2
  }
  
  # Calcolo della devianza entro i gruppi
  devianza_entro_gruppi <- 0
  for (livello in livelli) {
    gruppo <- data[data$categorical_var == livello,]
    mean_gruppo <- mean(gruppo$numerical_var)
    devianza_entro_gruppi <- devianza_entro_gruppi + sum((gruppo$numerical_var - mean_gruppo)^2)
  }
  
  return(list(devianza_totale = devianza_totale, devianza_tra_gruppi = devianza_tra_gruppi, devianza_entro_gruppi = devianza_entro_gruppi, eta2 = (devianza_tra_gruppi/devianza_totale)))
}

# grafico a violino
ggplot(case, aes(x = as.factor(RoofStyle), y = SalePrice)) + geom_violin() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# grafico colorato
ggplot(case, aes(x = as.factor(MasVnrType), y = SalePrice, fill = as.factor(MasVnrType))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)



# RoofStyle
case$RoofStyle <- factor(case$RoofStyle)
calcola_devianza(case$SalePrice, case$RoofStyle)
ggplot(case, aes(x = as.factor(RoofStyle), y = SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# si nota che la devianza within ha un valore molto elevato, dovuta soprattuto alle classi Gabel e Hip che hanno dei valori outlier
# le case con i prezzi più alti appartengono alle due classi sopracitate, ma
# la variabile RoofStyle comunque non sembra influenzare il prezzo delle case in quanto la media dei prezzi di ogni classe è comunque allineata



# RoofMatl
case$RoofMatl <- factor(case$RoofMatl, levels = c("Roll", "ClyTile", "CompShg", "Tar&Grv", "Metal", "Membran", "WdShake", "WdShngl"))
calcola_devianza(case$SalePrice, case$RoofMatl)
ggplot(case, aes(x = as.factor(RoofMatl), y = SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
ggplot(case, aes(x = as.factor(RoofMatl), y = SalePrice)) + geom_violin() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# si nota che la devianza within ha un valore molto elevato, dovuta soprattuto alla classa CompShg che ha numerosi valori outlier
# l'andamento dei prezzi medi delle case sembrano seguire l'ordine dei factor
# tuttavia , come si evince dal grafico a violino, si hanno pochi dati per alcune classe di variabili
# in particolare per Roll, ClyTile, Metal e Membran
# la variabile RoofMatl non sembra influenzare il prezzo delle case in quanto il valore di ETA^2 è comunque basso



# Exterior1st
case$Exterior1st <- factor(case$Exterior1st)
calcola_devianza(case$SalePrice, case$Exterior1st)
ggplot(case, aes(x = as.factor(Exterior1st), y = SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# le varianze within e between hanno valori elevati, e la ETA^2 ha un valore di circa 0.15
# di conseguenza c'è una bassa correlazione tra la variabile Exterior1st e il prezzo



# Exterior2nd
case$Exterior2nd <- factor(case$Exterior2nd)
calcola_devianza(case$SalePrice, case$Exterior2nd)
ggplot(case, aes(x = as.factor(Exterior2nd), y = SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# le varianze within e between hanno valori elevati, e la ETA^2 ha un valore di circa 0.15
# di conseguenza c'è una bassa correlazione tra la variabile Exterior1st e il prezzo



# MasVnrType
case$MasVnrType <- factor(case$MasVnrType)
calcola_devianza(case$SalePrice, case$MasVnrType)
ggplot(case, aes(x = as.factor(MasVnrType), y = SalePrice)) + geom_boxplot() + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red")
ggplot(case, aes(x = as.factor(MasVnrType), y = SalePrice)) + geom_violin()
# il valore di ETA^2 è di circa 0.19 che mostra una leggera correlazione tra il prezzo e MasVnrType
# in particolare, in media le case rifinite in Stone e in BrickFace hanno un prezzo più alto





