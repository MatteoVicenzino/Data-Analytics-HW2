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
---
Visto che stiamo lavorando con la variabile "SalePrice" come target avremo in generale un valore molto alto per la devianza. La devianza totale è infatti 9.207911e+12 che in questo caso vediamo essere principalmente Devianza "Entro". 
Eta Quadro è infatti 0.246 il che mi conferma che la variazione delle medie tra i gruppi è meno marcata della variazione dei valori nei gruppi stessi.
Il grafico ci mostra infatti che tutte le medie dei gruppi sono vicine alla media generale mentre classi come la "60" e la "20" contengono anche valori di "SalePrice" decisamente elevati.


#MSZoning:

calcola_devianza(case$SalePrice, factor(case$MSZoning))
ggplot(case, aes(x = factor(MSZoning), y = SalePrice, fill = factor(MSZoning))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "MSZoning")
---
In relazione a "SalePrice" abbiamo quasi esclusivamente devianza "Entro", Eta Quadro è infatti basso.
Questo perché le medie dei gruppi sono tutte vicine a quella generale e per via del fatto che i valori estremi di "RL" incidono molto sul calcolo della devianza essendo esso il gruppo più comune.
Vediamo che il gruppo "C" è quello con la media dei prezzi più bassa e che nessunodi essi supera la media generale. 

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
---
Con target la variabile "SalePrice" vediamo che il coefficiente di correlazione è 0.3517991, vediamo quindi una correlazione bassa, ma comunque presente, tra le due variabili.
R Quadro è invece molto basso e infatti vediamo una dispersione molto ampia dei punti. 
Anche rimuovendo i valori più estremi la correlazione cambia poco.


#LotArea: 

cor(case$LotArea, case$SalePrice, use="complete.obs")
model <- lm(SalePrice ~ LotArea, data = case)
summary(model)
ggplot(case, aes(x = LotArea, y = SalePrice)) + geom_point() + geom_smooth(method = "lm", se = FALSE) 
----
La correlazione lineare con "SalePrice" è positiva ma bassa ed R Quadro pure ha un valore molto vicino allo zero. 
Rimuovendo i valori di "LotArea" maggiori di 100000 vediamo che la correlazione sale di quasi 0.10 . C'è quindi una correlazione lineare molto più marcata una volta rimossi i 4 dati più estremi. 
Questa è un osservazione importante perché, anche se il numero di queste osservazioni non è elevato, ci si aspetterebbe che abitazioni con Aree sopra i 100000 metri quadrati fossero estremamente più costose rispetto alle altre.

#LotArea Senza Valori Estremi:
Area <- case[case$LotArea < 100000,]
cor(Area$LotArea, Area$SalePrice, use="complete.obs")
model <- lm(SalePrice ~ LotArea, data = Area)
summary(model)
ggplot(Area, aes(x = LotArea, y = SalePrice)) + geom_point() + geom_smooth(method = "lm", se = FALSE)

#Street: 

calcola_devianza(case$SalePrice, factor(case$Street))
ggplot(case, aes(x = factor(Street), y = SalePrice, fill = factor(Street))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Street")
---
Visto che il tipo "Grvl" corrisponde a sole 6 osservazioni, questo pesa molto poco
sulla devianza che infatti è quasi solamente composta di devianza "Entro".
L'indice Eta Quadro è infatti uguale a 0.001683915 (la devianza "tra" è inferiore 
allo 0.2% di quella totale). 

#Alley:   

Alley_F <- factor(replace(caseL$Alley, is.na(caseL$Alley), "Non Presente"))
calcola_devianza(case$SalePrice, Alley_F)
ggplot(case, aes(x = Alley_F, y = SalePrice, fill = Alley_F)) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Alley")
---
Come per la variabile precedente, abbiamo un gruppo drasticamente più numeroso rispetto
gli altri. La devianza è quindi principalmente composta dalla devianza "Entro" ed
Eta Quadro è infatti 0.02040754. Si nota che tutti i valori più elevati di 
"SalePrice" si trovano all'interno del gruppo "Non Presente".

#LotShape: 

calcola_devianza(case$SalePrice, factor(case$LotShape))
ggplot(case, aes(x = ordered(factor(LotShape),levels = c("Reg","IR1","IR2","IR3")), y = SalePrice, fill = factor(LotShape))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "LotShape")
---
Anche in questo caso la devianza "Tra" è decisamente inferiore rispetto alla "Entro".
Eta quadro e infatti 0.07637571. In questo caso il valore basso della deviazione "Tra" è 
dovuto al fatto che i 4 gruppi possiedono tutti medie molto vicine a quella generale.

#LandContour:

calcola_devianza(case$SalePrice, factor(case$LandContour))
ggplot(case, aes(x = ordered(factor(LandContour),levels = c("Low","Lvl","Bnk","HLS")), y = SalePrice, fill = factor(LandContour))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "LandContour")
---
La devianza "Tra" è ancora molto bassa con Eta quadro uguale 0.02579409. Anche se
il gruppo "Lvl" non comprendesse 90% delle osservazioni, tutte le medie dei gruppi sono 
molto vicine a quella generale. I gruppi hanno range di valori simili fatta eccezione 
di "Lvl" che possiede anche quelli più estremi.

#Utilities:

calcola_devianza(case$SalePrice, factor(case$Utilities))
ggplot(case, aes(x = factor(Utilities), y = SalePrice, fill = factor(Utilities))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Utilities")
---
Il valore della devianza "Tra" è estremamente minore di quella "Entro" ed infatti
Eta Quadro è uguale a solo 0.0002048991. L'unico valore del gruppo NoSeWa è inoltre
molto vicino alla media generale.

#LotConfig: 

calcola_devianza(case$SalePrice, factor(case$LotConfig))
ggplot(case, aes(x = ordered(factor(LotConfig), levels = c("Inside","Corner","CulDSac","FR2","FR3")), y = SalePrice, fill = factor(LotConfig))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "LotConfig")
---
Un alto caso in cui il valore di Eta Quadro è estremamente basso. I gruppi hanno 
infatti medie molto vicine a quella generale e anche range simili.

#LandSlope:

calcola_devianza(case$SalePrice, factor(case$LandSlope))
ggplot(case, aes(x = factor(LandSlope), y = SalePrice, fill = factor(LandSlope))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "LandSlope")
---
Come in molti casi precedenti la devianza "Entro" è estremamente alta essendo 
il gruppo "Gtl" disproporzionalmente numeroso. Inoltre, anche le medie di "Mod" e 
"Sev" sono vicine alla media generale. I valori più alti della variabile prezzo 
sono presenti nel gruppo "Gtl"

#Neighborhood:

calcola_devianza(case$SalePrice, factor(case$Neighborhood))
ggplot(case, aes(x = factor(Neighborhood), y = SalePrice, fill = factor(Neighborhood))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Neighborhood")
---
In questo caso Eta Quadro è uguale a 0.545575 e infatti la devianza di tipo "Tra"
è quella prevalente. Si può vedere, infatti, che ci sono molti gruppi la cui media 
è decisamente distante da quella generale. Con questa variabile in particolare, il
sapere a che gruppo appartiene una certa abitazione può aiutarci a prevedere quale
sarà il suo prezzo. Proprietà situate in NoRidge, NridgHt o StoneBr sono, come si
può vedere dal grafico, molto più care rispetto la media generale mentre quelle
situate in Blueste, MeadowV, BrDale, NPkVill e IDOTRR sono sotto di essa. Bisogna
però fare attenzione perché queste 5 hanno un sample size molto basso.

#Condition1:

calcola_devianza(case$SalePrice, factor(case$Condition1))
ggplot(case, aes(x = factor(Condition1), y = SalePrice, fill = factor(Condition1))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Condition1")
---
Eta Quadro è molto basso in questo caso, infatti il gruppo "Norm" influisce 
sproporzionalmente di più sulla devianza rispetto gli altri. Tutti i gruppi hanno
comunque una media molto vicina a quella generale, il che contribuisce anche 
questo a tenere bassa da devianza "Tra" e quindi anche Eta Quadro.

#Condition2:

calcola_devianza(case$SalePrice, factor(case$Condition2))
ggplot(case, aes(x = factor(Condition2), y = SalePrice, fill = factor(Condition2))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "Condition2")
---
Eta Quadro è anche qui molto basso ed essendo tutti i gruppi diversi da "Norm" 
formati da al 6 proprietà, non abbiamo una sample size abbastanza grande per fare previsioni 
sul comportamento della variabile "SalePrice" al variare di "Condition2".

#BldgType:

calcola_devianza(case$SalePrice, factor(case$BldgType))
ggplot(case, aes(x = factor(BldgType), y = SalePrice, fill = factor(BldgType))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "BldgType")
---
Eta Quadro è 0.03453403, infatti la devianza "Entro" è quella prevalente. Vediamo
che le medie di tutti i gruppi sono vicine a quella generale e che l'unico gruppo
con valori che si discostano tanto dalla media è "1Fam"

#HouseStyle:  

calcola_devianza(case$SalePrice, factor(case$HouseStyle))
ggplot(case, aes(x = ordered(factor(HouseStyle), levels = c("1Story","1.5Fin","1.5Unf","2Story","2.5Fin","2.5Unf","SFoyer","SLvl")), y = SalePrice, fill = factor(HouseStyle))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "HouseStyle")
---
Eta Quadro, uguale a 0.08631263, è molto basso. Tutte le medie sono infatti molto vicine
a quella generale anche se la presenza di ulteriori piani ad un edificio farebbe 
pensare ad una correlazione stretta con l'aumento di "SalePrice"

#OverallQual: 

calcola_devianza(case$SalePrice, factor(case$OverallQual))
ggplot(case, aes(x = factor(OverallQual), y = SalePrice, fill = factor(OverallQual))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "OverallQual")
---
Per l'analisi bivariata la consideriamo come una Variabile Qualitativa.
Abbiamo Eta Quadro uguale a 0.6841813, il che denota una prevalenza di devianza "Tra".
Si vede infatti come all'aumentare del valore di "OverallQual" la variabile "SalePrice"
salga. Volendo comunque usare la correlazione lineare con questa variabile, vediamo che essa 
arriva a 0.7909816, ovvero è presente una forte correlazione tra le due variabili.

#OverallCond:

calcola_devianza(case$SalePrice, factor(case$OverallCond))
ggplot(case, aes(x = factor(OverallCond), y = SalePrice, fill = factor(OverallCond))) + geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + stat_summary(fun = mean, geom = "point", shape = 18, size = 1, color = "red") +  geom_hline(yintercept = mean(case$SalePrice), linetype = "dashed", color = "blue") + labs(x = "OverallCond")
----
Dalla Devianza vediamo che quella principale è quella di tipo "Entro", Eta Quadro è infatti basso a 0.1253901.
Si vede che le medie dei gruppi sono tutte vicine a quella generale con i gruppi "5" e "6" quelli con i valori più estremi.

#YearBuilt:

cor(case$YearBuilt, case$SalePrice, use="complete.obs")
model <- lm(SalePrice ~ YearBuilt, data = case)
summary(model)
ggplot(case, aes(x = YearBuilt, y = SalePrice)) + geom_point() + geom_smooth(method = "lm", se = FALSE)
---
Il coefficiente di correlazione in questo caso è uguale a 0.5228973, il che denota 
una discreta correlazione tra le due variabili. R Quadro ha un valore di 0.2729 che 
mi indica una dispersione dei punti ampia intorno alla mia retta di regressione.





