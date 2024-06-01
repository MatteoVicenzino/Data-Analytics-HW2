library(ggplot2)
library(dplyr)
case <- read.csv("train.csv")

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

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
    mean_gruppo <- mean(gruppo$numerical_var, na.rm = T)
    devianza_entro_gruppi <- devianza_entro_gruppi + sum((gruppo$numerical_var - mean_gruppo)^2, na.rm = T)
  }
  
  return(list(devianza_totale = devianza_totale, devianza_tra_gruppi = devianza_tra_gruppi, devianza_entro_gruppi = devianza_entro_gruppi, eta2 = (devianza_tra_gruppi/devianza_totale)))
}

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#MSSubClass:
  
calcola_devianza(case$SalePrice, factor(case$MSSubClass))
ggplot(case, aes(x = factor(MSSubClass), y = SalePrice, fill = factor(MSSubClass))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "MSSubClass")

#MSZoning:

calcola_devianza(case$SalePrice, factor(case$MSZoning))
ggplot(case, aes(x = factor(MSZoning), y = SalePrice, fill = factor(MSZoning))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "MSZoning")


#LotFrontage: 

cor(case$LotFrontage, case$SalePrice, use="complete.obs")
model <- lm(SalePrice ~ LotFrontage, data = case)
summary(model)
ggplot(case, aes(x = LotFrontage, y = SalePrice)) + geom_point(na.rm = T) + geom_smooth(method = "lm", se = FALSE, na.rm = T)

#LotFrontage Senza Valori Estremi:
Frontage <- case[case$LotFrontage < 200,]
cor(Frontage$LotFrontage, Frontage$SalePrice, use="complete.obs")
model <- lm(SalePrice ~ LotFrontage, data = Frontage)
summary(model)
ggplot(Frontage, aes(x = LotFrontage, y = SalePrice)) + geom_point(na.rm = T) + geom_smooth(method = "lm", se = FALSE, na.rm = T)


#LotArea: 

cor(case$LotArea, case$SalePrice, use="complete.obs")
model <- lm(SalePrice ~ LotArea, data = case)
summary(model)
ggplot(case, aes(x = LotArea, y = SalePrice)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 

#LotArea Senza Valori Estremi:
Area <- case[case$LotArea < 100000,]
cor(Area$LotArea, Area$SalePrice, use="complete.obs")
model <- lm(SalePrice ~ LotArea, data = Area)
summary(model)
ggplot(Area, aes(x = LotArea, y = SalePrice)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

#Street: 

calcola_devianza(case$SalePrice, factor(case$Street))
ggplot(case, aes(x = factor(Street), y = SalePrice, fill = factor(Street))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Street")

#Alley:   

Alley_F <- factor(replace(caseL$Alley, is.na(caseL$Alley), "Non Presente"))
calcola_devianza(case$SalePrice, Alley_F)
ggplot(case, aes(x = Alley_F, y = SalePrice, fill = Alley_F)) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Alley")

#LotShape: 

calcola_devianza(case$SalePrice, factor(case$LotShape))
ggplot(case, aes(x = ordered(factor(LotShape),levels = c("Reg","IR1","IR2","IR3")), y = SalePrice, fill = factor(LotShape))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "LotShape")

#LandContour:

calcola_devianza(case$SalePrice, factor(case$LandContour))
ggplot(case, aes(x = ordered(factor(LandContour),levels = c("Low","Lvl","Bnk","HLS")), y = SalePrice, fill = factor(LandContour))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "LandContour")

#Utilities:

calcola_devianza(case$SalePrice, factor(case$Utilities))
ggplot(case, aes(x = factor(Utilities), y = SalePrice, fill = factor(Utilities))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Utilities")

#LotConfig: 

calcola_devianza(case$SalePrice, factor(case$LotConfig))
ggplot(case, aes(x = ordered(factor(LotConfig), levels = c("Inside","Corner","CulDSac","FR2","FR3")), y = SalePrice, fill = factor(LotConfig))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "LotConfig")

#LandSlope:

calcola_devianza(case$SalePrice, factor(case$LandSlope))
ggplot(case, aes(x = factor(LandSlope), y = SalePrice, fill = factor(LandSlope))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "LandSlope")

#Neighborhood:

calcola_devianza(case$SalePrice, factor(case$Neighborhood))
ggplot(case, aes(x = factor(Neighborhood), y = SalePrice, fill = factor(Neighborhood))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Neighborhood")

#Condition1:

calcola_devianza(case$SalePrice, factor(case$Condition1))
ggplot(case, aes(x = factor(Condition1), y = SalePrice, fill = factor(Condition1))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Condition1")

#Condition2:

calcola_devianza(case$SalePrice, factor(case$Condition2))
ggplot(case, aes(x = factor(Condition2), y = SalePrice, fill = factor(Condition2))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Condition2")


#BldgType:

calcola_devianza(case$SalePrice, factor(case$BldgType))
ggplot(case, aes(x = factor(BldgType), y = SalePrice, fill = factor(BldgType))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "BldgType")

#HouseStyle:  

calcola_devianza(case$SalePrice, factor(case$HouseStyle))
ggplot(case, aes(x = ordered(factor(HouseStyle), levels = c("1Story","1.5Fin","1.5Unf","2Story","2.5Fin","2.5Unf","SFoyer","SLvl")), y = SalePrice, fill = factor(HouseStyle))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "HouseStyle")

#OverallQual: 

calcola_devianza(case$SalePrice, factor(case$OverallQual))
ggplot(case, aes(x = factor(OverallQual), y = SalePrice, fill = factor(OverallQual))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "OverallQual")

#OverallCond:

calcola_devianza(case$SalePrice, factor(case$OverallCond))
ggplot(case, aes(x = factor(OverallCond), y = SalePrice, fill = factor(OverallCond))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "OverallCond")

#YearBuilt:

cor(case$YearBuilt, case$SalePrice, use="complete.obs")
model <- lm(SalePrice ~ YearBuilt, data = case)
summary(model)
ggplot(case, aes(x = YearBuilt, y = SalePrice)) + geom_point() + geom_smooth(method = "lm", se = FALSE)




