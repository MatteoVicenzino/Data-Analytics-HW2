library(moments)
library(ggplot2)

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

#YearRemodAdd
calcolo_cov_cor(case$YearRemodAdd)
model <- lm(SalePrice ~ YearRemodAdd, data = case)
summary(model)
ggplot(case, aes(x = log(YearRemodAdd), y = log(SalePrice)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# il coefficiente di correlazione lineare è circa 0.5. Dal modello di regressione lineare osserviamo un valore di R^2 pari a circa 0.26.

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



# MasVnrArea
cor(case$MasVnrArea, case$SalePrice, use = "complete.obs")
model <- lm(SalePrice ~ MasVnrArea, data = subset(case, MasVnrArea != 0), na.action = "na.omit")
summary(model)
ggplot(case, aes(x = MasVnrArea, y = SalePrice)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(data = subset(case, MasVnrArea != 0), aes(x = MasVnrArea, y = SalePrice)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# si nota che la correlazione è di circa 0.48
# la variabile MasVnrArea quindi  influenza debolmente il prezzo delle case, come si può vedere dal grafico



# ExterQual
case$ExterQual <- factor(case$ExterQual, levels = c("Fa","TA","Gd","Ex"))
calcola_devianza(case$SalePrice, case$ExterQual)
ggplot(case, aes(x = as.factor(ExterQual), y = SalePrice, fill = as.factor(ExterQual))) + geom_violin() + geom_boxplot(width=0.3, alpha=1/5) + scale_fill_brewer(palette = "RdYlGn") + guides(fill = FALSE)
# la variabile ExterQual influenza fortemente il prezzo di una casa,
# si può infatti notare che il rapporto tra la la devianza between e la devianza totale è di 0.4
# come si vede anche dal grafico più la finitura esterna è di qualità e più il prezzo della casa sale



# ExterCond
case$ExterCond <- factor(case$ExterCond, levels = c("Po","Fa","TA","Gd","Ex"))
calcola_devianza(case$SalePrice, case$ExterCond)
ggplot(case, aes(x = as.factor(ExterCond), y = SalePrice, fill = as.factor(ExterCond))) + geom_violin() + geom_boxplot(width=0.3, alpha=1/5)  + scale_fill_brewer(palette = "RdYlGn") + guides(fill = FALSE)
# la variabile ExterCond influenza debolmente il prezzo,
# il valore di ETS^2 è di 0.2 infatti dal grafico si evince che le classi Average/typical e Good contengono numerosi valori outlier, con un prezzo più alto della media



# Foundation
case$Foundation <- factor(case$Foundation, levels = c("Slab","Stone","BrkTil","CBlock","Wood","PConc"))
calcola_devianza(case$SalePrice, case$Foundation)
ggplot(case, aes(x = as.factor(Foundation), y = SalePrice, fill = as.factor(Foundation))) + geom_violin() + geom_boxplot(width=0.3, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "black")
# il tipo di fondamenta influenza moderatamente il prezzo delle case
# infatti, le case con le fondamenta in Poured Contrete hanno in media il prezzo più alto
# e anche in assoluto, le case con il prezzo più alto hanno le fondamenta in Poured Contrete
# tuttavia BrkTil e CBlock contengono alcuni valori outlier di conseguenza il valore di ETA^2 non è elevatissimo



# BsmtQual
case$BsmtQual <- factor(case$BsmtQual, levels = c("Fa","TA","Gd","Ex"))
calcola_devianza(case$SalePrice, case$BsmtQual)
ggplot(case, aes(x = as.factor(BsmtQual), y = SalePrice, fill = as.factor(BsmtQual))) + geom_violin() + geom_boxplot(width=0.3, alpha=1/5) + scale_fill_brewer(palette = "RdYlGn") + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "black")
# si nota una forte correlazione tra la qualità del seminterrato e il prezzo delle case, 
# il rapporto tra la devianza totale e la devianza tra i gruppi è vicina al 55% che 
# il che è dimostrato dal grafico in cui si vede che la media dei prezzi delle case con la qualità del seminterrato Fair è la più bassa
# mentre la media dei prezzi delle case con la qualità del seminterrato Excellent è la più alta



# BsmtCond
case$BsmtCond <- factor(case$BsmtCond, levels = c("Po","Fa","TA","Gd"))
calcola_devianza(case$SalePrice, case$BsmtCond)
ggplot(case, aes(x = as.factor(BsmtCond), y = SalePrice, fill = as.factor(BsmtCond))) + geom_violin() + scale_fill_brewer(palette = "RdYlGn") + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "black")
# la correlazione tra la condizione del seminterrato ha una debole correlazione, 
# a casusa di numerosi valori outlier presenti nella classe Average/Typical
# e per il fatto che la devianza within è molto elevata
# infatti la devianza between è molto minore rispetto alla devianza totale



# BsmtExposure
case$BsmtExposure <- factor(case$BsmtExposure, levels = c("No", "Mn", "Av", "Gd"))
calcola_devianza(case$SalePrice, case$BsmtExposure)
ggplot(case, aes(x = as.factor(BsmtExposure), y = SalePrice, fill = as.factor(BsmtExposure))) + geom_violin() + geom_boxplot(width=0.3, alpha=1/5) + scale_fill_brewer(palette = "RdYlGn") + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "black")
# l'influenza con la variabile BsmtExposure è bassa, infatti le medie dei vari gruppi sono piuttosto vicine tra loro
# tuttavia in parte si osserva che le case con una buona esposizione hanno un prezzo medio leggermente maggiore rispetto alle altre



# BsmtFinType1
case$BsmtFinType1 <- factor(case$BsmtFinType1, levels = c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
calcola_devianza(case$SalePrice, case$BsmtFinType1)
ggplot(case, aes(x = as.factor(BsmtFinType1), y = SalePrice, fill = as.factor(BsmtFinType1))) + geom_violin() + geom_boxplot(width=0.3, alpha=1/5) + scale_fill_brewer(palette = "RdYlGn") + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "black")
# dal grafico si nota che le medie e le mediane di ogni classe sono tutte sullo stesso livello
# questo indica un basso valore di varianza between, infatti il rapporto con l avarianza totale è di 0.21
# questo indica una bassa influenza della variabile BsmtFinType1 sui prezzi delle case



# BsmtFinSF1
cor(case$BsmtFinSF1, case$SalePrice, use = "complete.obs")
model <- lm(SalePrice ~ BsmtFinSF1, data = case, na.action = "na.omit")
model <- lm(SalePrice ~ BsmtFinSF1, data = subset(case, BsmtFinSF1 != 0), na.action = "na.omit")
summary(model)
ggplot(case, aes(x = BsmtFinSF1, y = SalePrice), ) + geom_point() + geom_smooth(method = "lm") + xlim(0,2500)
ggplot(data = subset(case, BsmtFinSF1 != 0), aes(x = BsmtFinSF1, y = SalePrice), ) + geom_point() + geom_smooth(method = "lm") + xlim(0,2500)
# la correlazione delle due variabili è di 0.39 e come si nota dal grafico la il modello non ha una un'ottima accuratezza 
# la variabile BsmtFinSF1 quindi, non influenza pesantemente il prezzo delle case



# BsmtFinType2
case$BsmtFinType2 <- factor(case$BsmtFinType2, levels = c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
calcola_devianza(case$SalePrice, case$BsmtFinType2)
ggplot(case, aes(x = as.factor(BsmtFinType2), y = SalePrice, fill = as.factor(BsmtFinType2))) + geom_violin() + geom_boxplot(width=0.3, alpha=1/5) + scale_fill_brewer(palette = "RdYlGn") + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 2, color = "black")
# le due variabili non sono correlate infatti un grande numero di case, anche con prezzi elevati, hanno il seminterrato incompleto
# questo si può notare anche dal valore di ETA^2 che è molto basso, dello 0.19%
# dovuto alla codevianza between molto bassa


# BsmtFinSF2
cor(case$BsmtFinSF2, case$SalePrice, use = "complete.obs")
model <- lm(SalePrice ~ BsmtFinSF2, data = case, na.action = "na.omit")
model <- lm(SalePrice ~ BsmtFinSF2, data = subset(case, BsmtFinSF2 != 0), na.action = "na.omit")
summary(model)
ggplot(case, aes(x = BsmtFinSF2, y = SalePrice), ) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(data = subset(case, BsmtFinSF2 != 0), aes(x = BsmtFinSF2, y = SalePrice), ) + geom_point() + geom_smooth(method = "lm", se = FALSE)
# essendo questa variabile la superficie della finitura descritta alla variabile precedente,
# anche qui non c'è nessuna relazione tra le due variabili, il valore della correlazione è di -0.11% : molto prossima allo zero



# BsmtUnfSF
cor(case$BsmtUnfSF, case$SalePrice, use = "complete.obs")
model <- lm(SalePrice ~ BsmtUnfSF, data = case, na.action = "na.omit")
model <- lm(SalePrice ~ BsmtUnfSF, data = subset(case, BsmtUnfSF != 0), na.action = "na.omit")
summary(model)
ggplot(case, aes(x = BsmtUnfSF, y = SalePrice), ) + geom_point() + geom_smooth(method = "lm", se = FALSE)
ggplot(data = subset(case, BsmtUnfSF != 0), aes(x = BsmtUnfSF, y = SalePrice), ) + geom_point() + geom_smooth(method = "lm", se = FALSE)


# TotalBsmtSF
cor(case$TotalBsmtSF, case$SalePrice, use = "complete.obs")
model <- lm(SalePrice ~ TotalBsmtSF, data = case, na.action = "na.omit")
model <- lm(SalePrice ~ TotalBsmtSF, data = subset(case, TotalBsmtSF != 0), na.action = "na.omit")
summary(model)
ggplot(case, aes(x = TotalBsmtSF, y = SalePrice), ) + geom_point() + geom_smooth(method = "lm", se = FALSE) + xlim(0,3500)
ggplot(data = subset(case, TotalBsmtSF != 0), aes(x = TotalBsmtSF, y = SalePrice), ) + geom_point() + geom_smooth(method = "lm", se = FALSE) + xlim(0,3500)


# commentare queste 2
# fare le ultime 2
# daje roma daje
