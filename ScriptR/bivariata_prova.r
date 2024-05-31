library(moments)
library(ggplot2)
case <- read.csv("C:\\Users\\giova\\Downloads\\train.csv", stringsAsFactors = TRUE)

# bivariata
# sales_price variabile target

#sales_price vs MSSubClass
ggplot(case, aes(x = as.factor(MSSubClass), y = SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# fai grafico a violino
ggplot(case, aes(x = as.factor(MSSubClass), y = SalePrice)) + geom_violin() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

cor(case$MSSubClass, case$SalePrice)
modello <- lm(SalePrice ~ MSSubClass, data = case)
summary(modello)
ggplot(case, aes(x = factor(MSSubClass), y = SalePrice)) + geom_boxplot() +stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") + geom_smooth(method = "lm") +   geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") 


# aggiungi qualche commento sulle informazioni ottenute
# la variabile MSSubClass non sembra avere un impatto significativo sul prezzo di vendita

#sales_price vs MSZoning
ggplot(case, aes(x = as.factor(MSZoning), y = SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# fai grafico a violino
ggplot(case, aes(x = as.factor(MSZoning), y = SalePrice)) + geom_violin() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

modello <- lm(SalePrice ~ MSZoning, data = case)
summary(modello)
deviance(modello)
ggplot(case, aes(x = MSZoning, y = SalePrice)) + geom_point() + geom_smooth(method = "lm")

# aggiungi qualche commento sulle informazioni ottenute
# la variabile MSZoning sembra avere un impatto significativo sul prezzo di vendita

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
    mean_gruppo <- mean(gruppo$numerical_var, na.remove = TRUE)
    devianza_entro_gruppi <- devianza_entro_gruppi + sum((gruppo$numerical_var - mean_gruppo)^2, na.rm = TRUE)
  }
  
  return(list(devianza_totale = devianza_totale, devianza_tra_gruppi = devianza_tra_gruppi, devianza_entro_gruppi = devianza_entro_gruppi))
}

# Calcoliamo la devianza per la variabile MSSubClass
devianza_MSSubClass <- calcola_devianza(case$SalePrice, factor(case$MSSubClass))
devianza_MSSubClass
aov(SalePrice ~ factor(MSSubClass), data = case)

# quantitativa e quantitativa
ggplot(case, aes(x = GrLivArea, y = SalePrice)) + geom_point()
cov(case$GrLivArea, case$SalePrice) # inutile
cor(case$GrLivArea, case$SalePrice)
model <- lm(SalePrice ~ GrLivArea, data = case)
summary(model)
ggplot(case, aes(x = GrLivArea, y = SalePrice)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

# se c'è forte correlazione vicino allo 1 o -1, se troviamo correlazione = 0 non c'è correlazione
