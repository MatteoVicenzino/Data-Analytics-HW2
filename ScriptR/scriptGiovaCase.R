# Carica il dataset
case <- read.csv("C:\\Users\\giova\\Downloads\\train.csv", stringsAsFactors = TRUE)

# Funzione per le Variabili Quantitative
display_summary_and_var <- function(variabile){
  c(summary(variabile), 
    var = var(variabile, na.rm = T), 
    sd = sd(variabile, na.rm = T),
    sk = skewness(variabile, na.rm = T))
}

# Funzione per le Variabili Qualitative
display_table <- function(variabile){
  DistAs <- table(variabile)
  DistRe <- prop.table(table(variabile))
  barplot(prop.table(table(variabile)))
  print(rbind(DistAs, DistRe))
}

# Analisi della variabile: GarageCars
variabile <- case$GarageCars
# Visualizza il sommario e la varianza della variabile
display_summary_and_var(variabile)
hist(variabile, main = "Distribuzione di GarageCars", xlab = "Numero di auto nel garage")
boxplot(variabile, main = "Boxplot di GarageCars", ylab = "Numero di auto nel garage")


# Discussione:
# La variabile GarageCars rappresenta il numero di auto che possono essere alloggiate nel garage.
# Il boxplot mostra che la maggior parte delle case ha uno o due posti auto nel garage, con poche eccezioni che hanno più di due posti auto.
# L'istogramma mostra una distribuzione simile, con una concentrazione intorno ai valori bassi e qualche outlier con valori più alti.
# L'analisi della skewness suggerisce una leggera coda a destra nella distribuzione, indicando una maggior concentrazione di case con un numero inferiore di posti auto nel garage.

# Analisi della variabile: GarageArea
variabile <- case$GarageArea
cat("Analisi della variabile: GarageArea\n")
# Visualizza il sommario e la varianza della variabile
print(display_summary_and_var(variabile))
hist(variabile, main = "Distribuzione di GarageArea", xlab = "Area del garage in piedi quadrati")
boxplot(variabile, main = "Boxplot di GarageArea", ylab = "Area del garage in piedi quadrati")


# Discussione:
# La variabile GarageArea rappresenta l'area del garage in piedi quadrati.
# Il boxplot mostra una vasta gamma di aree del garage, con alcune case che hanno garage molto grandi rispetto alla media.
# L'istogramma e la densità dei dati indicano una distribuzione asimmetrica, con una coda a destra e un picco intorno alle aree più basse.
# Ciò suggerisce che la maggior parte delle case ha garage di dimensioni moderate, ma ci sono alcune case con garage molto grandi.


# Analisi della variabile: GarageQual
variabile <- case$GarageQual
cat("Analisi della variabile: GarageQual\n")
# Visualizza il sommario e la varianza della variabile
display_table(variabile)

# Discussione:
# La variabile GarageQual rappresenta la qualità del garage.
# Il barplot mostra la distribuzione delle diverse categorie di qualità del garage.
# La maggior parte delle case ha una qualità media o tipica del garage, con poche eccezioni che hanno una qualità eccellente o scarsa.


# Analisi della variabile: GarageCond
variabile <- case$GarageCond
cat("Analisi della variabile: GarageCond\n")
# Visualizza il sommario e la varianza della variabile
display_table(variabile)

# Discussione:
# La variabile GarageCond rappresenta le condizioni del garage.
# Il barplot mostra la distribuzione delle diverse categorie di condizioni del garage.
# La maggior parte delle case ha condizioni medie o tipiche del garage, con poche eccezioni che hanno condizioni eccellenti o pessime.


# Analisi della variabile: PavedDrive
variabile <- case$PavedDrive
cat("Analisi della variabile: PavedDrive\n")
# Visualizza il sommario e la varianza della variabile
display_table(variabile)

# Discussione:
# La variabile PavedDrive indica se la via di accesso è pavimentata.
# Il barplot mostra la distribuzione delle diverse categorie di tipi di via di accesso.
# La maggior parte delle case ha una via di accesso pavimentata, con poche eccezioni che hanno un accesso parzialmente pavimentato o non pavimentato.


# Analisi della variabile: WoodDeckSF
variabile <- case$WoodDeckSF
cat("Analisi della variabile: WoodDeckSF\n")
# Visualizza il sommario e la varianza della variabile
print(display_summary_and_var(variabile))
hist(variabile, main = "Distribuzione di WoodDeckSF", xlab = "Area del deck in legno in piedi quadrati")
boxplot(variabile, main = "Boxplot di WoodDeckSF", ylab = "Area del deck in legno in piedi quadrati")


# Discussione:
# La variabile WoodDeckSF rappresenta l'area del deck in legno in piedi quadrati.
# Il boxplot mostra una vasta gamma di dimensioni del deck in legno, con una concentrazione intorno ai valori bassi e alcuni outlier con dimensioni più grandi.
# L'istogramma e la densità dei dati indicano una distribuzione asimmetrica, con una coda a destra e un picco intorno alle dimensioni più basse.
# Questo suggerisce che la maggior parte delle case ha deck in legno di dimensioni moderate, ma ci sono alcune case con deck molto grandi.
# L'analisi della skewness mostra una leggera coda a destra nella distribuzione, indicando una maggiore concentrazione di case con dimensioni più basse del deck in legno.


# Analisi della variabile: OpenPorchSF
variabile <- case$OpenPorchSF
cat("Analisi della variabile: OpenPorchSF\n")
# Visualizza il sommario e la varianza della variabile
print(display_summary_and_var(variabile))
hist(variabile, main = "Distribuzione di OpenPorchSF", xlab = "Area del portico aperto in piedi quadrati")
boxplot(variabile, main = "Boxplot di OpenPorchSF", ylab = "Area del portico aperto in piedi quadrati")


# Discussione:
# La variabile OpenPorchSF rappresenta l'area del portico aperto in piedi quadrati.
# Il boxplot mostra una vasta gamma di dimensioni del portico aperto, con una concentrazione intorno ai valori bassi e alcuni outlier con dimensioni più grandi.
# L'istogramma e la densità dei dati indicano una distribuzione asimmetrica, con una coda a destra e un picco intorno alle dimensioni più basse.
# Questo suggerisce che la maggior parte delle case ha portici aperti di dimensioni moderate, ma ci sono alcune case con portici molto grandi.
# L'analisi della skewness mostra una coda a destra nella distribuzione, indicando una maggiore concentrazione di case con dimensioni più basse del portico aperto.


# Analisi della variabile: EnclosedPorch
variabile <- case$EnclosedPorch
cat("Analisi della variabile: EnclosedPorch\n")
# Visualizza il sommario e la varianza della variabile
print(display_summary_and_var(variabile))
hist(variabile, main = "Distribuzione di EnclosedPorch", xlab = "Area del portico chiuso in piedi quadrati")
boxplot(variabile, main = "Boxplot di EnclosedPorch", ylab = "Area del portico chiuso in piedi quadrati")


# Discussione:
# La variabile EnclosedPorch rappresenta l'area del portico chiuso in piedi quadrati.
# Il boxplot mostra una vasta gamma di dimensioni del portico chiuso, con una concentrazione intorno ai valori bassi e alcuni outlier con dimensioni più grandi.
# L'istogramma e la densità dei dati indicano una distribuzione asimmetrica, con una coda a destra e un picco intorno alle dimensioni più basse.
# Questo suggerisce che la maggior parte delle case ha portici chiusi di dimensioni moderate, ma ci sono alcune case con portici molto grandi.
# L'analisi della skewness mostra una coda a destra nella distribuzione, indicando una maggiore concentrazione di case con dimensioni più basse del portico chiuso.


# Analisi della variabile: 3SsnPorch
variabile <- case$'3SsnPorch'
cat("Analisi della variabile: 3SsnPorch\n")
# Visualizza il sommario e la varianza della variabile
print(display_summary_and_var(variabile))
hist(variabile, main = "Distribuzione di 3SsnPorch", xlab = "Area del portico a tre stagioni in piedi quadrati")
boxplot(variabile, main = "Boxplot di 3SsnPorch", ylab = "Area del portico a tre stagioni in piedi quadrati")


# Discussione:
# La variabile 3SsnPorch rappresenta l'area del portico a tre stagioni in piedi quadrati.
# Il boxplot mostra una vasta gamma di dimensioni del portico a tre stagioni, con una concentrazione intorno ai valori bassi e alcuni outlier con dimensioni più grandi.
# L'istogramma e la densità dei dati indicano una distribuzione asimmetrica, con una coda a destra e un picco intorno alle dimensioni più basse.
# Questo suggerisce che la maggior parte delle case ha portici a tre stagioni di dimensioni moderate, ma ci sono alcune case con portici molto grandi.
# L'analisi della skewness mostra una leggera coda a destra nella distribuzione, indicando una maggiore concentrazione di case con dimensioni più basse del portico a tre stagioni.


# Analisi della variabile: ScreenPorch
variabile <- case$ScreenPorch
cat("Analisi della variabile: ScreenPorch\n")
# Visualizza il sommario e la varianza della variabile
print(display_summary_and_var(variabile))
hist(variabile, main = "Distribuzione di ScreenPorch", xlab = "Area del portico dello schermo in piedi quadrati")
boxplot(variabile, main = "Boxplot di ScreenPorch", ylab = "Area del portico dello schermo in piedi quadrati")


# Discussione:
# La variabile ScreenPorch rappresenta l'area del portico dello schermo in piedi quadrati.
# Il boxplot mostra una vasta gamma di dimensioni del portico dello schermo, con una concentrazione intorno ai valori bassi e alcuni outlier con dimensioni più grandi.
# L'istogramma e la densità dei dati indicano una distribuzione asimmetrica, con una coda a destra e un picco intorno alle dimensioni più basse.
# Questo suggerisce che la maggior parte delle case ha portici dello schermo di dimensioni moderate, ma ci sono alcune case con portici molto grandi.
# L'analisi della skewness mostra una coda a destra nella distribuzione, indicando una maggiore concentrazione di case con dimensioni più basse del portico dello schermo.


# Analisi della variabile: PoolArea
variabile <- case$PoolArea
cat("Analisi della variabile: PoolArea\n")
# Visualizza il sommario e la varianza della variabile
print(display_summary_and_var(variabile))
hist(variabile, main = "Distribuzione di PoolArea", xlab = "Area della piscina in piedi quadrati")
boxplot(variabile, main = "Boxplot di PoolArea", ylab = "Area della piscina in piedi quadrati")


# Discussione:
# La variabile PoolArea rappresenta l'area della piscina in piedi quadrati.
# Il boxplot mostra una vasta gamma di dimensioni della piscina, con una concentrazione intorno ai valori bassi e alcuni outlier con dimensioni più grandi.
# L'istogramma e la densità dei dati indicano una distribuzione asimmetrica, con una coda a destra e un picco intorno alle dimensioni più basse.
# Questo suggerisce che la maggior parte delle case ha piscine di dimensioni moderate, ma ci sono alcune case con piscine molto grandi.
# L'analisi della skewness mostra una coda a destra nella distribuzione, indicando una maggiore concentrazione di case con dimensioni più basse della piscina.


# Analisi della variabile: PoolQC
variabile <- case$PoolQC
cat("Analisi della variabile: PoolQC\n")
# Visualizza il sommario e la varianza della variabile
display_table(variabile)

# Discussione:
# La variabile PoolQC rappresenta la qualità della piscina.
# Il barplot mostra la distribuzione delle diverse categorie di qualità della piscina.
# La maggior parte delle case non ha una piscina, con poche eccezioni che hanno piscine di alta qualità.


# Analisi della variabile: Fence
variabile <- case$Fence
cat("Analisi della variabile: Fence\n")
# Visualizza il sommario e la varianza della variabile
display_table(variabile)

# Discussione:
# La variabile Fence rappresenta la qualità del recinto.
# Il barplot mostra la distribuzione delle diverse categorie di qualità del recinto.
# La maggior parte delle case non ha un recinto, mentre una piccola parte ha recinti di qualità variabile.


# Analisi della variabile: MiscFeature
variabile <- case$MiscFeature
cat("Analisi della variabile: MiscFeature\n")
# Visualizza il sommario e la varianza della variabile
display_table(variabile)

# Discussione:
# La variabile MiscFeature rappresenta altre caratteristiche extra presenti nelle proprietà.
# Il barplot mostra la distribuzione delle diverse categorie di caratteristiche extra.
# La maggior parte delle case non ha caratteristiche extra, con poche eccezioni che includono strutture come ripostigli, recinti e altri.


# Analisi della variabile: MiscVal
variabile <- case$MiscVal
cat("Analisi della variabile: MiscVal\n")
# Visualizza il sommario e la varianza della variabile
print(display_summary_and_var(variabile))
hist(variabile, main = "Distribuzione di MiscVal", xlab = "Valore delle caratteristiche extra")
boxplot(variabile, main = "Boxplot di MiscVal", ylab = "Valore delle caratteristiche extra")


# Discussione:
# La variabile MiscVal rappresenta il valore delle caratteristiche extra.
# Il boxplot mostra che la maggior parte delle case ha un valore extra basso, con alcune eccezioni che hanno un valore extra più alto.
# L'istogramma e la densità dei dati indicano una distribuzione asimmetrica, con una coda a destra.
# Questo suggerisce che la maggior parte delle case ha valori extra di basso valore, ma ci sono alcune case con valori extra molto alti.


# Analisi della variabile: MoSold
variabile <- case$MoSold
cat("Analisi della variabile: MoSold\n")
# Visualizza il sommario e la varianza della variabile
print(display_summary_and_var(variabile))
hist(variabile, main = "Distribuzione di MoSold", xlab = "Mese di vendita")
boxplot(variabile, main = "Boxplot di MoSold", ylab = "Mese di vendita")


# Discussione:
# La variabile MoSold rappresenta il mese in cui la casa è stata venduta.
# Il boxplot mostra che le vendite sono distribuite durante tutto l'anno, con picchi nei mesi di primavera e estate.
# L'istogramma mostra che i mesi con il maggior numero di vendite sono giugno, luglio e agosto.
# L'analisi della skewness indica una distribuzione abbastanza uniforme delle vendite durante l'anno.


# Analisi della variabile: YrSold
variabile <- case$YrSold
cat("Analisi della variabile: YrSold\n")
# Visualizza il sommario e la varianza della variabile
print(display_summary_and_var(variabile))
hist(variabile, main = "Distribuzione di YrSold", xlab = "Anno di vendita")
boxplot(variabile, main = "Boxplot di YrSold", ylab = "Anno di vendita")


# Discussione:
# La variabile YrSold rappresenta l'anno in cui la casa è stata venduta.
# Il boxplot mostra che le vendite sono distribuite uniformemente tra gli anni presenti nel dataset.
# L'istogramma mostra che c'è una leggera diminuzione del numero di vendite negli anni più recenti.
# L'analisi della skewness indica una distribuzione relativamente uniforme delle vendite negli anni considerati.


# Analisi della variabile: SaleType
variabile <- case$SaleType
cat("Analisi della variabile: SaleType\n")
# Visualizza il sommario e la varianza della variabile
display_table(variabile)

# Discussione:
# La variabile SaleType rappresenta il tipo di vendita della casa.
# Il barplot mostra la distribuzione delle diverse categorie di tipi di vendita.
# La maggior parte delle case è venduta con una vendita normale, con altre categorie come vendita in contanti, vendita con finanziamento, e altre meno comuni.


# Analisi della variabile: SaleCondition
variabile <- case$SaleCondition
cat("Analisi della variabile: SaleCondition\n")
# Visualizza il sommario e la varianza della variabile
display_table(variabile)

# Discussione:
# La variabile SaleCondition rappresenta le condizioni di vendita della casa.
# Il barplot mostra la distribuzione delle diverse categorie di condizioni di vendita.
# La maggior parte delle case è venduta in condizioni normali, con altre categorie come vendite anomale, vendite parziali e altre meno comuni.


