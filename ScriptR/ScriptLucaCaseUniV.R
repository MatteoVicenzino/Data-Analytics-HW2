library(moments)
case <- read.csv("train.csv")
caseL <- case[,2:20]

#Funzione per le Variabili Quantitative
display_summary_and_var <- function(variabile){
  c(summary(variabile, na.rm = T), 
    var = var(variabile, na.rm = T), 
    sd = sd(variabile, na.rm = T),
    sk = skewness(variabile, na.rm = T))
}

#Funzione per le Variabili Qualitative
display_table <- function(variabile){
  DistAs <- table(variabile)
  DistRe <- prop.table(table(variabile))
  barplot(prop.table(table(variabile)))
  print(rbind(DistAs, DistRe))
}

#MSSubClass: Questa variabile è essenzialmente Categoriale anche se indicata come
#            numerica, infatti rappresenta il tipo dell'abitazione presa in considerazione
#            Si nota che che i due tipi di abitazione più popolari sono
#            "20", che rappresenta 1-STORY 1946 & NEWER ALL STYLES, e "60",
#            che sono 2-STORY 1946 & NEWER.

MSSubClass_F <- factor(replace(caseL$MSSubClass, is.na(caseL$MSSubClass), "Non Presente"))
display_table(MSSubClass_F)

#MSZoning: Variabile categoriale che indica il tipo di classificazione della 
#          proporietà in vendita.
#          In questo caso "RL", che indica zone residenziali a bassa densità, è 
#          la più comune con il 78.83% dei casi osservati

MSZoning_F <- factor(replace(caseL$MSZoning, is.na(caseL$MSZoning), "Non Presente"))
display_table(MSZoning_F)

#LotFrontage: Variablie Qunatitativa che rappresenta la quantità di strada in 
#             piedi che è a contatto con la proprietà.
#             Sono presenti valori estremi come 313 ed un picco molto più alto della
#             distribuzione tra 55 e 60

display_summary_and_var(caseL$LotFrontage)
hist(caseL$LotFrontage, probability = T, breaks =c(5*0:64),col = "blue")
abline(v = median(caseL$LotFrontage, na.rm = T),lwd = 2, col = "red")
abline(v = mean(caseL$LotFrontage, na.rm = T),lwd = 2)

#LotArea: Variabile Quantitativa che indica l'area della proprietà 
#         I dati, anche se il massimo è oltre 215000, si concentrano tra 0 e 25000

display_summary_and_var(caseL$LotArea)
hist(caseL$LotArea, probability = T, breaks = c(1250*0:176)+1000,col = "blue")
abline(v = median(caseL$LotArea, na.rm = T),lwd = 1, col = "red")
abline(v = mean(caseL$LotArea, na.rm = T),lwd = 1)

#Street: Variabile Qualitativa che indica il tipo di strda di accesso alla
#        proprietà.
#        Queste strade sono quasi esclusivamente del tipo "Pave"

Street_F <- factor(replace(caseL$Street, is.na(caseL$Street), "Non Presente"))
display_table(Street_F)

#Alley: Variabile Qualitativa che indica il tipo del vicolo di accesso alla
#       proprietà.
#       Vediamo che la granparte delle abitazioni non presenta un vicolo di accesso


Alley_F <- factor(replace(caseL$Alley, is.na(caseL$Alley), "Non Presente"))
display_table(Alley_F)

#LotShape: Variabile Qualitativa che descrive la forma generale della proprietà.
#          La forma più presente è quella "Regolare" cheviene vista piu spesso
#          di tutte le tre forme Irregolari combinate

LotShape_F <- factor(replace(caseL$LotShape, is.na(caseL$LotShape), "Non Presente"))
LotShape_F <- ordered(LotShape_F,levels = c("Reg","IR1","IR2","IR3"))
display_table(LotShape_F)

#LandContour: Variabile Qualitativa che descrive i rilievi presenti sulla proprietà
#             Generalmente le case sono a livello con la strada

LandContour_F <- factor(replace(caseL$LandContour, is.na(caseL$LandContour), "Non Presente"))
LandContour_F <- ordered(LandContour_F,levels = c("Low","Lvl","Bnk","HLS"))
display_table(LandContour_F)

#Utilities: Variabile qualitativa che descrive le utenze domestiche presenti
#           nella proprietà.
#           Dei 4 valori della variabile possibili sono solo presenti "AllPub" e "NoSeWa"
#           e anche di queste "NoSeWa" è stata osservata solo una volta (<0.1%)

Utilities_F <- factor(replace(caseL$Utilities, is.na(caseL$Utilities), "Non Presente"))
display_table(Utilities_F)

#LotConfig: Variabile Qualitativa che descrive la particella catastale.
#           Il valore più comune della variabile è "Inside"

LotConfig_F <- factor(replace(caseL$LotConfig, is.na(caseL$LotConfig), "Non Presente"))
LotConfig_F <- ordered(LotConfig_F, levels = c("Inside","Corner","CulDSac","FR2","FR3"))
display_table(LotConfig_F)

#LandSlope: Variabile Qualitativa che descrive la pendenza del terreno su cui è 
#           costruita la proprietà
#           La il valore più visto è "Gtl", che indica una bassa pendenza

LandSlope_F <- factor(replace(caseL$LandSlope, is.na(caseL$LandSlope), "Non Presente"))
display_table(LandSlope_F)

#Neighborhood: Variabile Qualitativa che descrive la locazione della proprietà in
#              Ames city
#              In questo caso abbimo 25 possibili valori per questa variabile con 
#              NAmes il piùfrequente e Blueste il meno

Neighborhood_F <- factor(replace(caseL$Neighborhood, is.na(caseL$Neighborhood), "Non Presente"))
display_table(Neighborhood_F)

#Condition1: Variabile Qualitativa che descrive la vicinanza della abitazione a 
#            punti di interesse
#            Il valore più comune è "Norm" che indica nessun punto di interesse 
#            nelle vicinanze

Condition1_F <- factor(replace(caseL$Condition1, is.na(caseL$Condition1), "Non Presente"))
display_table(Condition1_F)

#Condition2: Variabile Qualitativa che descrivela vicinanza della abitazione a 
#            punti di interesse, in caso ne siano presenti ulteriori 
#            "Norm" rimane il valore della variabile più comune e si nota che sono
#            quindi presenti 15 abitazioni con almeno 2 punti di interesse nelle vicinanze
#            Avendo solo 2 Variabili che mi indicano i punti di interesse vicini
#            non abbiamo la possibilità di sapere se questi 15 proprietà ne hanno ulteriori

Condition2_F <- factor(replace(caseL$Condition2, is.na(caseL$Condition2), "Non Presente"))
display_table(Condition2_F)

#BldgType: Variabile Qualitativa che descrive che tipo di abitazione è quella in vendita
#          Vengono viste principalmente abitazioni di una singola famiglia

BldgType_F <- factor(replace(caseL$BldgType, is.na(caseL$BldgType), "Non Presente"))
display_table(BldgType_F)

#HouseStyle: Variabile Qualitativa che descrive lo stile o tipo del edificio.
#            Vediamo che edifici con mezzi piani sono molto più rari e che gli edefici
#            con meno piano sono più numerosi

HouseStyle_F <- factor(replace(caseL$HouseStyle, is.na(caseL$HouseStyle), "Non Presente"))
HouseStyle_F <- ordered(HouseStyle_F, levels = c("1Story","1.5Fin","1.5Unf","2Story","2.5Fin","2.5Unf","SFoyer","SLvl"))
display_table(HouseStyle_F)

#OverallQual: Variabile Quantitativa che descrive la qualità dei materiali e della 
#             finitura casa in una scala che va da 1 a 10
#             Si vede che la maggiorparte delle case si trova della facia che va da 
#             "Avarege", ovvero 5, in su. Con media e mediana entrambe vicino a 
#             "Above Avarege", ovvero 6.

display_summary_and_var(caseL$OverallQual)
hist(caseL$OverallQual, probability = T,breaks= c(0:10)+0.5,col = "blue")
abline(v = median(caseL$OverallQual, na.rm = T),lwd = 1, col = "red")
abline(v = mean(caseL$OverallQual, na.rm = T),lwd = 1)

#OverallCond: Variabile Quantitativa che descrive le condizioni attuali dell'abitazione
#             in una scala che va da 1 a 10
#             Si vede che la mediana e la media assumono valori notevolmente diversi
#             Si osserva che la mediana è anche la moda e che il primo e terzo quantile
#             sono molto vicini

display_summary_and_var(caseL$OverallCond)
hist(caseL$OverallCond, probability = T,breaks= c(0:10)+0.5,col = "blue")
abline(v = median(caseL$OverallCond, na.rm = T),lwd = 1, col = "red")
abline(v = mean(caseL$OverallCond, na.rm = T),lwd = 1)

#YearBuilt: Variabile Quantitativa che descrive l'anno in cui le case sono state
#           costruite.
#           Si nota che il picco è avvenuto nei primi anni del 2000 e che tra il
#           1980 e 1990, durante la recessione globale, c'è stato un grandissimo calo,
#           anche maggiore di quelli avvenuti durante le guerre mondiali

display_summary_and_var(caseL$YearBuilt)
hist(caseL$YearBuilt, probability = T,breaks= c(5*0:28)+1870,col = "blue")
abline(v = median(caseL$YearBuilt, na.rm = T),lwd = 1, col = "red")
abline(v = mean(caseL$YearBuilt, na.rm = T),lwd = 1)
