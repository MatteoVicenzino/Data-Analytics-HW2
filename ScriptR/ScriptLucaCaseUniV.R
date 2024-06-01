case <- read.csv("train.csv")

#Funzione per le Variabili Quantitative
display_summary_and_var <- function(variabile){
  c(summary(variabile, na.rm = T), 
    var = var(variabile, na.rm = T), 
    sd = sd(variabile, na.rm = T),
    sk = skewness(variabile, na.rm = T))
}

#Funzione per le Variabili Qualitative
display_table <- function(variabile, titolo){
  DistAs <- table(variabile)
  DistRe <- prop.table(table(variabile))
  barplot(prop.table(table(variabile)), main = titolo)
  print(rbind(DistAs, DistRe))
}

#-----------------------------------------------------------------------------------------------------------------------------

#MSSubClass: Questa variabile è essenzialmente Categoriale anche se indicata come
#            numerica, infatti rappresenta il tipo dell'abitazione presa in considerazione
#            Si nota che che i due tipi di abitazione più popolari sono
#            "20", che rappresenta 1-STORY 1946 & NEWER ALL STYLES, e "60",
#            che sono 2-STORY 1946 & NEWER.

MSSubClass_F <- factor(replace(case$MSSubClass, is.na(case$MSSubClass), "Non Presente"))
display_table(MSSubClass_F, "Subclassi")

#MSZoning: Variabile categoriale che indica il tipo di classificazione della 
#          proporietà in vendita.
#          In questo caso "RL", che indica zone residenziali a bassa densità, è 
#          la più comune con il 78.83% dei casi osservati

MSZoning_F <- factor(replace(case$MSZoning, is.na(case$MSZoning), "Non Presente"))
display_table(MSZoning_F,"Classificazione della Proprietà")

#LotFrontage: Variablie Qunatitativa che rappresenta la quantità di strada in 
#             piedi che è a contatto con la proprietà.
#             Sono presenti valori estremi come 313 ed un picco molto più alto della
#             distribuzione tra 55 e 60

display_summary_and_var(case$LotFrontage)
hist(case$LotFrontage, probability = T, breaks =c(5*0:64),col = "gray", main = "Quantità di strada in piedi collegata alla proprietà")
abline(v = median(case$LotFrontage, na.rm = T),lwd = 1, col = "red")
abline(v = mean(case$LotFrontage, na.rm = T),lwd = 1)

#LotArea: Variabile Quantitativa che indica l'area della proprietà 
#         I dati, anche se il massimo è oltre 215000, si concentrano tra 0 e 25000

display_summary_and_var(case$LotArea)
hist(case$LotArea, probability = T, breaks = c(1250*0:176)+1000,col = "gray", main = "Area della Proprietà")
abline(v = median(case$LotArea, na.rm = T),lwd = 1, col = "red")
abline(v = mean(case$LotArea, na.rm = T),lwd = 1)

#LotArea: Senza valori estremi 
Area <- case[case$LotArea < 100000,]
hist(Area$LotArea, probability = T, breaks = c(1250*0:60)+1000,col = "gray", main = "Area della Proprietà")
abline(v = median(Area$LotArea, na.rm = T),lwd = 1, col = "red")
abline(v = mean(Area$LotArea, na.rm = T),lwd = 1)

#Street: Variabile Qualitativa che indica il tipo di strda di accesso alla
#        proprietà.
#        Queste strade sono quasi esclusivamente del tipo "Pave"

Street_F <- factor(replace(case$Street, is.na(case$Street), "Non Presente"))
display_table(Street_F, "Tipo di strada di accesso")

#Alley: Variabile Qualitativa che indica il tipo del vicolo di accesso alla
#       proprietà.
#       Vediamo che la granparte delle abitazioni non presenta un vicolo di accesso


Alley_F <- factor(replace(case$Alley, is.na(case$Alley), "Non Presente"))
display_table(Alley_F, "Tipo di Vicolo di accesso")

#LotShape: Variabile Qualitativa che descrive la forma generale della proprietà.
#          La forma più presente è quella "Regolare" cheviene vista piu spesso
#          di tutte le tre forme Irregolari combinate

LotShape_F <- factor(replace(case$LotShape, is.na(case$LotShape), "Non Presente"))
LotShape_F <- ordered(LotShape_F,levels = c("Reg","IR1","IR2","IR3"))
display_table(LotShape_F, "Forma della Proprietà")

#LandContour: Variabile Qualitativa che descrive i rilievi presenti sulla proprietà
#             Generalmente le case sono a livello con la strada

LandContour_F <- factor(replace(case$LandContour, is.na(case$LandContour), "Non Presente"))
LandContour_F <- ordered(LandContour_F,levels = c("Low","Lvl","Bnk","HLS"))
display_table(LandContour_F,"Rilievi")

#Utilities: Variabile qualitativa che descrive le utenze domestiche presenti
#           nella proprietà.
#           Dei 4 valori della variabile possibili sono solo presenti "AllPub" e "NoSeWa"
#           e anche di queste "NoSeWa" è stata osservata solo una volta (<0.1%)

Utilities_F <- factor(replace(case$Utilities, is.na(case$Utilities), "Non Presente"))
display_table(Utilities_F, "Utenze Domestiche")

#LotConfig: Variabile Qualitativa che descrive la particella catastale.
#           Il valore più comune della variabile è "Inside"

LotConfig_F <- factor(replace(case$LotConfig, is.na(case$LotConfig), "Non Presente"))
LotConfig_F <- ordered(LotConfig_F, levels = c("Inside","Corner","CulDSac","FR2","FR3"))
display_table(LotConfig_F, "Particella catastale")

#LandSlope: Variabile Qualitativa che descrive la pendenza del terreno su cui è 
#           costruita la proprietà
#           La il valore più visto è "Gtl", che indica una bassa pendenza

LandSlope_F <- factor(replace(case$LandSlope, is.na(case$LandSlope), "Non Presente"))
display_table(LandSlope_F, "Pendenza")

#Neighborhood: Variabile Qualitativa che descrive la locazione della proprietà in
#              Ames city
#              In questo caso abbimo 25 possibili valori per questa variabile con 
#              NAmes il piùfrequente e Blueste il meno

Neighborhood_F <- factor(replace(case$Neighborhood, is.na(case$Neighborhood), "Non Presente"))
display_table(Neighborhood_F,"Locazione")

#Condition1: Variabile Qualitativa che descrive la vicinanza della abitazione a 
#            punti di interesse
#            Il valore più comune è "Norm" che indica nessun punto di interesse 
#            nelle vicinanze

Condition1_F <- factor(replace(case$Condition1, is.na(case$Condition1), "Non Presente"))
display_table(Condition1_F, "Punti di Interesse 1")

#Condition2: Variabile Qualitativa che descrivela vicinanza della abitazione a 
#            punti di interesse, in caso ne siano presenti ulteriori 
#            "Norm" rimane il valore della variabile più comune e si nota che sono
#            quindi presenti 15 abitazioni con almeno 2 punti di interesse nelle vicinanze
#            Avendo solo 2 Variabili che mi indicano i punti di interesse vicini
#            non abbiamo la possibilità di sapere se questi 15 proprietà ne hanno ulteriori

Condition2_F <- factor(replace(case$Condition2, is.na(case$Condition2), "Non Presente"))
display_table(Condition2_F, "Punti di Interesse 2")

#BldgType: Variabile Qualitativa che descrive che tipo di abitazione è quella in vendita
#          Vengono viste principalmente abitazioni di una singola famiglia

BldgType_F <- factor(replace(case$BldgType, is.na(case$BldgType), "Non Presente"))
display_table(BldgType_F, "Tipo di Abitazione")

#HouseStyle: Variabile Qualitativa che descrive lo stile o tipo del edificio.
#            Vediamo che edifici con mezzi piani sono molto più rari e che gli edefici
#            con meno piano sono più numerosi

HouseStyle_F <- factor(replace(case$HouseStyle, is.na(case$HouseStyle), "Non Presente"))
HouseStyle_F <- ordered(HouseStyle_F, levels = c("1Story","1.5Fin","1.5Unf","2Story","2.5Fin","2.5Unf","SFoyer","SLvl"))
display_table(HouseStyle_F, "Stile della casa")

#OverallQual: Variabile Quantitativa che descrive la qualità dei materiali e della 
#             finitura casa in una scala che va da 1 a 10
#             Si vede che la maggiorparte delle case si trova della facia che va da 
#             "Avarege", ovvero 5, in su. Con media e mediana entrambe vicino a 
#             "Above Avarege", ovvero 6.

display_summary_and_var(case$OverallQual)
hist(case$OverallQual, probability = T,breaks= c(0:10)+0.5,col = "gray", main = "Qualità della casa")
abline(v = median(case$OverallQual, na.rm = T),lwd = 1, col = "red")
abline(v = mean(case$OverallQual, na.rm = T),lwd = 1)

#OverallCond: Variabile Quantitativa che descrive le condizioni attuali dell'abitazione
#             in una scala che va da 1 a 10
#             Si vede che la mediana e la media assumono valori notevolmente diversi
#             Si osserva che la mediana è anche la moda e che il primo e terzo quantile
#             sono estremamente vicini vicini

display_summary_and_var(case$OverallCond)
hist(case$OverallCond, probability = T,breaks= c(0:10)+0.5,col = "gray", main = "Condizione della casa")
abline(v = median(case$OverallCond, na.rm = T),lwd = 1, col = "red")
abline(v = mean(case$OverallCond, na.rm = T),lwd = 1)

#YearBuilt: Variabile Quantitativa che descrive l'anno in cui le case sono state
#           costruite.
#           Si nota che il picco è avvenuto nei primi anni del 2000 e che tra il
#           1980 e 1990, durante la recessione globale, c'è stato un grandissimo calo
#           anche maggiore di quelli avvenuti durante le guerre mondiali

display_summary_and_var(case$YearBuilt)
hist(case$YearBuilt, probability = T,breaks= c(5*0:28)+1870,col = "gray", main = "Anno di Costruzione")
abline(v = median(case$YearBuilt, na.rm = T),lwd = 1, col = "red")
abline(v = mean(case$YearBuilt, na.rm = T),lwd = 1)
