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

#MSSubClass:

Una Variabile Qualitativa che descrive il tipo di abitazione della proprietà in vendita.
Questi tipi di abitazione sono in totale 16 e vengono indicati, per brevità, usando 
dei numeri. I numeri delle classi non possono essere quindi usati per calcolare medie
o mediane.

La variabile non è ben distribuita, infatti solo le classi "20" e "60" comprendono
il 57.20% delle osservazioni totali. La classe meno presente è invece "40" con 
solo 4 osservazioni totali (<0.3%).

---
case$MSSubClass <- factor(replace(case$MSSubClass, is.na(case$MSSubClass), "Non Presente"))
display_table(case$MSSubClass, "Subclassi")

#MSZoning: 

Una Variabile Quantitativa che indica il tipo di classificazione della proprietà in vendita.
Sono presenti un totale di 5 diversi tipi indicati con una sigla di al più 2 lettere.

La variabile non è distribuita uniformemente con "RL" (Residenziale a bassa densità) 
che comprende il 78.73% delle osservazioni totali. La classe meno presente è "C" (Commerciale)
che viene vista sole 10 volte (<0.7%).

---
case$MSZoning <- factor(replace(case$MSZoning, is.na(case$MSZoning), "Non Presente"))
display_table(case$MSZoning,"Classificazione della Proprietà")

#LotFrontage: 

Variabile Quantitativa che salva la quantità di strada a contatto con la proprietà.
Sono presenti in questo caso 259 valori mancanti.
I valori di essa vanno da un minimo di 21 ad un massimo di 313. Con questi ultimi 
che sono valori estremi per questa variabile essendo ogni altro valore assunto da essa 
minore di 200.

Media e Mediana sono molto vicine, entrambe a circa 70. Si nota infatti che la 
Skewness è in questo caso molto bassa a 2.1 . Dal grafico vediamo che il picco di 
valori si trova nel range 55-80, che contiene sia il primo che terzo Quantile.

---
display_summary_and_var(case$LotFrontage)
hist(case$LotFrontage, probability = T, breaks =c(5*0:64),col = "gray", main = "Quantità di strada in piedi collegata alla proprietà")
abline(v = median(case$LotFrontage, na.rm = T),lwd = 1, col = "red")
abline(v = mean(case$LotFrontage, na.rm = T),lwd = 1)

#LotArea:

Variabile Quantitativa che indica l'area della proprietà. Ha un range di valori molto alto 
dovuto principalmente alla presenza di valori estremi che arrivano ad un massimo di 2.152450e+05. 

La gran parte delle osservazioni rimane sotto i 25000 metri quadrati con un alta concentrazione
tra 7500 e 11600. Media e mediana molto vicine tra loro e skewness, infatti, molto 
bassa soprattutto rispetto ai valori che assume "LotArea". Si nota che i valori 
estremi che va variabile assume sono riflessi in una varianza e deviazione standard elevati.

---
display_summary_and_var(case$LotArea)
hist(case$LotArea, probability = T, breaks = c(1250*0:176)+1000,col = "gray", main = "Area della Proprietà")
abline(v = median(case$LotArea, na.rm = T),lwd = 1, col = "red")
abline(v = mean(case$LotArea, na.rm = T),lwd = 1)
---
#LotArea: Senza valori estremi 
Area <- case[case$LotArea < 100000,]
hist(Area$LotArea, probability = T, breaks = c(1250*0:60)+1000,col = "gray", main = "Area della Proprietà")
abline(v = median(Area$LotArea, na.rm = T),lwd = 1, col = "red")
abline(v = mean(Area$LotArea, na.rm = T),lwd = 1)

#Street: 

Variabile Qualitativa che indica il tipo di strada di accesso alla proprietà.
Sono presenti solo due tipi di Strada di accesso e il 99.59% delle osservazioni
è del tipo "Pave".
---
case$Street <- factor(replace(case$Street, is.na(case$Street), "Non Presente"))
display_table(case$Street, "Tipo di strada di accesso")

#Alley:

Variabile Qualitativa che indica il tipo del vicolo di accesso alla proprietà.
Vediamo che il 93.77% delle abitazioni non presenta un vicolo di accesso mentre le
restanti lo hanno o pavimentato o in ghiaia.
---

case$Alley <- factor(replace(case$Alley, is.na(case$Alley), "Non Presente"))
display_table(case$Alley, "Tipo di Vicolo di accesso")

#LotShape: 

Variabile Qualitativa che descrive la forma generale della proprietà. Sono presenti
4 tipi di forma che vanno da quella "Regolare" a "IR3", ovvero altamente irregolare.

La forma più comune è quella "Regolare" che viene osservata più spesso delle tre forme 
Irregolari combinate.
---

case$LotShape <- factor(replace(case$LotShape, is.na(case$LotShape), "Non Presente"))
case$LotShape <- ordered(case$LotShape,levels = c("Reg","IR1","IR2","IR3"))
display_table(case$LotShape, "Forma della Proprietà")

#LandContour: 

Variabile Qualitativa che descrive i rilievi presenti sulla proprietà dividendoli 
in 4 possibili categorie.

Questo è un alto caso in cui una di queste categorie comprende un numero estremamente
elevato di osservazioni rispetto alle altre. "Lvl", ovvero "a livello con il terreno",
comprende quasi il 90% delle proprietà osservate.
---

case$LandContour <- factor(replace(case$LandContour, is.na(case$LandContour), "Non Presente"))
case$LandContour <- ordered(case$LandContour,levels = c("Low","Lvl","Bnk","HLS"))
display_table(case$LandContour,"Rilievi")

#Utilities: 

Variabile qualitativa che descrive le utenze domestiche presenti nella proprietà.         
Dei 4 valori della variabile possibili sono solo presenti "AllPub" e "NoSeWa". 
"NoSeWa" è stata inoltre osservata solo una volta (<0.1%)
---

case$Utilities <- factor(replace(case$Utilities, is.na(case$Utilities), "Non Presente"))
display_table(case$Utilities, "Utenze Domestiche")

#LotConfig: 

Variabile Qualitativa che descrive il tipo della particella catastale.
"Inside" è il valore più comune della variabile e corrisponde al 72% delle osservazioni
totali. "FR3", invece, è presente solo 4 volte. 
---

case$LotConfig <- factor(replace(case$LotConfig, is.na(case$LotConfig), "Non Presente"))
case$LotConfig <- ordered(case$LotConfig, levels = c("Inside","Corner","CulDSac","FR2","FR3"))
display_table(case$LotConfig, "Particella catastale")

#LandSlope: 

Variabile Qualitativa che descrive la pendenza del terreno su cui è costruita la 
proprietà dividendola in 3 possibili valori dal Gentile ("Gtl") al Severo ("Sev").
Il valore più visto è "Gtl" con il 94.65% delle osservazioni.
---

case$LandSlope <- factor(replace(case$LandSlope, is.na(case$LandSlope), "Non Presente"))
display_table(case$LandSlope, "Pendenza")

#Neighborhood:

Variabile Qualitativa che descrive la locazione della proprietà in Ames city.
In questo caso abbiamo 25 possibili valori per questa variabile con 
NAmes il più frequente a 15.43% delle osservazioni e Blueste il meno a sole 2 
ossecrazioni (<0.2%).
---

case$Neighborhood <- factor(replace(case$Neighborhood, is.na(case$Neighborhood), "Non Presente"))
display_table(case$Neighborhood,"Locazione")

#Condition1: 

Variabile Qualitativa che indica se la proprietà è vicina a uno tra 8 tipi di 
punti di interesse o se è lontana da tutti essi, questo è il caso "Norm".
Il valore più comune è "Norm" con l'86.3% dei dati osservati.
---

case$Condition1 <- factor(replace(case$Condition1, is.na(case$Condition1), "Non Presente"))
display_table(case$Condition1, "Punti di Interesse 1")

#Condition2: 

Variabile Qualitativa che indica la vicinanza della abitazione a ulteriori punti 
di interesse.
"Norm" rimane il valore della variabile più comune con adesso il 98.97% di tutte 
le osservazioni mentre gli altri valori ne hanno, in totale, solo 15. 
Essedo presenti solo 2 variabili che mi indicano la vicinanza a punti di interesse,
non possiamo sapere se le 15 abitazioni che sono vicine a 2 punti di interesse
non siano vicine anche a 3 o più di essi.
---

case$Condition2 <- factor(replace(case$Condition2, is.na(case$Condition2), "Non Presente"))
display_table(case$Condition2, "Punti di Interesse 2")

#BldgType: 

Variabile Qualitativa che descrive che tipo di abitazione è quella in vendita.
Il gruppo più comune è quello delle abitazioni da Singola Famiglia ("1Fam") con
l'83.56% delle osservazioni totali
---

case$BldgType <- factor(replace(case$BldgType, is.na(case$BldgType), "Non Presente"))
display_table(case$BldgType, "Tipo di Abitazione")

#HouseStyle: 

Variabile Qualitativa che descrive lo stile o tipo dell’edificio.
Vediamo che edifici con mezzi piani sono molto più rari e che gli edifici con 
meno piani sono più numerosi. La moda è infatti il gruppo "1Story", al 49.72% delle 
osservazioni totali, seguito da "2Story", al 30.47%. "2.5Fin" e "2.5Unf" sono invece 
i più rari.
---


case$HouseStyle <- factor(replace(case$HouseStyle, is.na(case$HouseStyle), "Non Presente"))
case$HouseStyle <- ordered(case$HouseStyle, levels = c("1Story","1.5Fin","1.5Unf","2Story","2.5Fin","2.5Unf","SFoyer","SLvl"))
display_table(case$HouseStyle, "Stile della casa")

#OverallQual: 

Variabile Quantitativa che descrive la qualità dei materiali e della finitura della casa 
in una scala di interi che va da 1 a 10. Può anche essere vista come una variabile 
Qualitativa ordinabile divisa in 10 classi.
Si nota che la maggior parte delle case si trova della fascia che va da "Avarege" (5)
a "Good" (7). Con media e mediana entrambe vicino a "Above Avarege" (6). La 
Skewness è infatti bassa a 0.216721.
---

display_summary_and_var(case$OverallQual)
hist(case$OverallQual, probability = T,breaks= c(0:10)+0.5,col = "gray", main = "Qualità della casa")
abline(v = median(case$OverallQual, na.rm = T),lwd = 1, col = "red")
abline(v = mean(case$OverallQual, na.rm = T),lwd = 1)

#OverallCond: 

"OverallCond" è una Variabile Quantitativa che descrive le condizioni in cui si trova l'abitazione
in una scala di interi che va da 1 a 10. Come "OverallQual", può anche essere vista come una variabile 
Qualitativa ordinabile divisa in 10 classi.
Si vede anche dal grafico che la moda è "5" con più del 50%delle osservazioni. Si nota che media e mediana 
non sono veramente vicine, infatti la Skewness è 0.6923552 che, considerato che il range di valori è 1-10,
non è bassa.
----

display_summary_and_var(case$OverallCond)
hist(case$OverallCond, probability = T,breaks= c(0:10)+0.5,col = "gray", main = "Condizione della casa")
abline(v = median(case$OverallCond, na.rm = T),lwd = 1, col = "red")
abline(v = mean(case$OverallCond, na.rm = T),lwd = 1)

#YearBuilt: 

Variabile Quantitativa che descrive l'anno in cui le abitazioni sono state costruite.
Il range va dal 1872 fino il 2010 con una quantità di case costruite maggiore all'avanzare degli anni.
Si nota che il picco è avvenuto nei primi anni del 2000 e che tra il 1980 e 1990,
durante la recessione globale, è avvenuto un forte calo nella costruzione di nuovi
edifici. Questo anche maggiore dei cali avvenuti durante le guerre mondiali.
Media e Mediana sono vicine mentre la Deviazione Standard e Varianza non sono basse.
---


display_summary_and_var(case$YearBuilt)
hist(case$YearBuilt, probability = T,breaks= c(5*0:28)+1870,col = "gray", main = "Anno di Costruzione")
abline(v = median(case$YearBuilt, na.rm = T),lwd = 1, col = "red")
abline(v = mean(case$YearBuilt, na.rm = T),lwd = 1)
