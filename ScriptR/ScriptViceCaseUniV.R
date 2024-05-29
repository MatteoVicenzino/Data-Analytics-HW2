library(moments)

case <- read.csv("~/Documents/UNI/2_anno_23_24/Analytics/6_HW2/Dati/train.csv", header=TRUE)

ncol(case)
str(case)
summary(case)


case <- case[, 22:41]

display_summary_and_var <- function(variabile){
  c(summary(variabile), 
    var = var(variabile, na.rm = T), 
    sd = sd(variabile, na.rm = T))
}

# RoofStyle
case$RoofStyle <- factor(case$RoofStyle)
prop.table(table(case$RoofStyle))
barplot(prop.table(table(case$RoofStyle)), main = "Frequenza tipi di tetto")
# variabile qualitativa categoriale con 6 diverse categorie, 
# la maggior parte delle case nel campione presenta un tetto di tipo "Gable"



# RoofMatl
case$RoofMatl <- factor(case$RoofMatl)
prop.table(table(case$RoofMatl))
barplot(prop.table(table(case$RoofMatl)), main = "Frequenza materiali composizione tetto")
# variabile qualitativa categoriale con 8 diverse categorie, 
# la maggior parte delle case (ben il 98.2%) nel campione presenta il tetto in materiale "CompShg"
# con le frequenze percentuali degli altri materiali che non raggiungono nemmeno l'1%



# Exterior1st
case$Exterior1st <- factor(case$Exterior1st)
prop.table(table(case$Exterior1st))
barplot(prop.table(table(case$Exterior1st)), main = "Frequenza materiale esterno")
# variabile qualitativa categoriale con 15 diverse categorie, 
# la maggior parte delle case nel campione presenta una finitura esterna in vinile
# che ha una frequenza di 35.3%



# Exterior2nd
case$Exterior2nd <- factor(case$Exterior2nd)
prop.table(table(case$Exterior2nd))
barplot(prop.table(table(case$Exterior2nd)), main = "Frequenza materiale esterno")
# variabile qualitativa categoriale con 16 diverse categorie, 
# la maggior parte delle case nel campione presenta una seconda finitura esterna in vinile
# che ha una frequenza di 34.5%


# MasVnrType
case$MasVnrType <- factor(case$MasVnrType)
prop.table(table(case$MasVnrType))
barplot(prop.table(table(case$MasVnrType)), main = "Frequenza materiale esterno")
# variabile qualitativa categoriale con 4 diverse categorie, 
# la maggior parte delle case nel campione (59.5%) non ha nessun tipo di rivestimento in  muratura esterno
# mentre il materiale più utilizzato è il Brick Face



# MasVnrArea
display_summary_and_var(case$MasVnrArea)
hist(case$MasVnrArea, freq = F, main = "distribuzione area finitura")

# si nota che la maggior parte delle case non ha una finitura esterna
# i seguenti grafici indicano la distribuzione dell'area del MasVnr delle case che effettivamente hanno una MasVnr 

MasVnrArea0 <- na.omit(case[case$MasVnrArea > 0, "MasVnrArea"])
display_summary_and_var(MasVnrArea0)
hist(MasVnrArea0, breaks = 16, freq = F, main = "distribuzione area finitura non nulla")
boxplot(MasVnrArea0, horizontal = T)
skewness(MasVnrArea0)

# si nota una coda destra piuttosto lunga, con un numero valori outlier elevato
# infatti l'indice di asimmetria è di 2.088



# ExterQual
case$ExterQual <- factor(case$ExterQual, levels = c("Fa","TA","Gd","Ex"))
levels(case$ExterQual) <- c("Fair","Average/Typical","Good","Excellent")
prop.table(table(case$ExterQual))
barplot(prop.table(table(case$ExterQual)), main = "qualità del materiale esterno")
# variabile qualitativa categoriale con 4 diverse categorie, 
# si è scelto di riordinare i fattori nel seguente ordine: "Fair","Average/Typical","Good","Excellent"
# la maggior parte delle case nel campione (62%) ha qualità dei materiali esterni Average/Typical


# ExterCond
case$ExterCond <- factor(case$ExterCond, levels = c("Po","Fa","TA","Gd","Ex"))
levels(case$ExterCond) <- c("Poor","Fair","Average/Typical","Good","Excellent")
prop.table(table(case$ExterCond))
barplot(prop.table(table(case$ExterCond)), main = "qualità del materiale esterno")
# analogamente alle variabili precedenti,
# variabile qualitativa categoriale con 5 diverse categorie, 
# si è scelto di riordinare i levels nel seguente ordine: "Poor","Fair","Average/Typical","Good","Excellent"
# la maggior parte delle case nel campione (87.8%) ha condizione dei materiali esterni Average/Typical



# Foundation
case$Foundation <- factor(case$Foundation)
prop.table(table(case$Foundation))
barplot(prop.table(table(case$Foundation)), main = "Frequenza materiale fondamenta")
# variabile qualitativa categoriale con 6 diverse categorie, 
# la maggior parte delle case nel campione (44.3%) ha le fondamenta in Poured Contrete


# BsmtQual
case$BsmtQual <- factor(case$BsmtQual, levels = c("Po","Fa","TA","Gd","Ex"))
levels(case$BsmtQual) <- c("Poor","Fair","Average/Typical","Good","Excellent")
prop.table(table(case$BsmtQual))
barplot(prop.table(table(case$BsmtQual)), main = "altezza del seminterrato")
# analogamente alle variabili precedenti,
# variabile qualitativa categoriale con 5 diverse categorie, 
# si è scelto di riordinare i levels nel seguente ordine: "Poor","Fair","Average/Typical","Good","Excellent"
# la maggior parte delle case nel campione (45.6%) ha altezza del seminterrato Average/Typical ()


# BsmtCond
case$BsmtCond <- factor(case$BsmtCond, levels = c("Po","Fa","TA","Gd","Ex"))
levels(case$BsmtCond) <- c("Poor","Fair","Average/Typical","Good","Excellent")
prop.table(table(case$BsmtCond))
barplot(prop.table(table(case$BsmtCond)), main = "condizioni del seminterrato")
# analogamente alle variabili precedenti,
# variabile qualitativa categoriale con 5 diverse categorie, 
# si è scelto di riordinare i levels nel seguente ordine: "Poor","Fair","Average/Typical","Good","Excellent"
# la maggior parte delle case nel campione (ben il 92.1%) ha condizione del seminterrato Average/Typical



# BsmtExposure
case$BsmtExposure <- factor(case$BsmtExposure, levels = c("No", "Mn", "Av", "Gd"))
prop.table(table(case$BsmtExposure))
barplot(prop.table(table(case$BsmtExposure)), main = "esposizione del seminterrato")
# variabile qualitativa categoriale con 4 diverse categorie, 
# la maggior parte delle case nel campione (67%) non ha il seminterrato esposte all'esterno



# BsmtFinType1
case$BsmtFinType1 <- factor(case$BsmtFinType1, levels = c("Unf", "LwQ", "Rec", "BLQ", "ALQ", "GLQ"))
prop.table(table(case$BsmtFinType1))
barplot(prop.table(table(case$BsmtFinType1)), main = "Valutazione dell'area del seminterrato")
# variabile qualitativa categoriale con 6 diverse categorie, 
# la maggior parte delle case nel campione (30.2%) ha il piano seminterrato non finito (grezzo)
# seguito da una percentuale del 29.4% di case che hanno il seminterrato abitabile con una Buona qualità.




# BsmtFinSF1
display_summary_and_var(case$BsmtFinSF1)
hist(case$MasVnrArea, freq = F, main = "distribuzione area finitura")

# si nota che la maggior parte delle case ha il seminterrato incompleto
# i seguenti grafici indicano la distribuzione dell'area di seminterrato delle case che effettivamente hanno il seminterrato completo

par(mfrow=c(2,2))
BsmtFinSF1_0 <- na.omit(case[case$BsmtFinSF1 > 0, "BsmtFinSF1"])
display_summary_and_var(BsmtFinSF1_0)
hist(BsmtFinSF1_0, freq = F, main = "distribuzione area seminterrato completo")
boxplot(BsmtFinSF1_0, horizontal = T)
skewness(BsmtFinSF1_0)

# si nota una coda destra piuttosto lunga, con un numero valori outlier elevato
# infatti l'indice di asimmetria è di 2.298795
# in particolare un valore massimo è di molto superiore alla media
# togliendo il valore elevato si comprende che la distribuzione segue l'andmento di una gaussiana

BsmtFinSF1_0 <- na.omit(case[case$BsmtFinSF1 > 0 & case$BsmtFinSF1 < max(case$BsmtFinSF1), "BsmtFinSF1"])
display_summary_and_var(BsmtFinSF1_0)
hist(BsmtFinSF1_0, freq = F, main = "distribuzione senza valore outlier")
curve(dnorm(x,mean(BsmtFinSF1_0), sd(BsmtFinSF1_0)),add = T)
boxplot(BsmtFinSF1_0, horizontal = T)
skewness(BsmtFinSF1_0)

par(mfrow=c(1,1))


## TYPE 2 DA FARE COME TYPE 1



# BsmtUnfSF
display_summary_and_var(case$BsmtUnfSF)
hist(case$BsmtUnfSF, freq = F, main = "distribuzione superficie incompleta seminterrato")
boxplot(case$BsmtUnfSF, horizontal = T)
skewness(case$BsmtUnfSF)

# si nota una coda destra leggermente allungata, con un numero valori outlier elevato
# infatti l'indice di asimmetria è di 0.919



# TotalBsmtSF

par(mfrow=c(1,2))

display_summary_and_var(case$TotalBsmtSF)
hist(case$TotalBsmtSF, freq = F, main = "distribuzione superficie incompleta seminterrato")
boxplot(case$TotalBsmtSF, horizontal = T)
skewness(case$TotalBsmtSF)

# si nota una coda destra piuttosto lunga, con un numero valori outlier elevatoà
# in particolare un valore massimo molto elevato e distante dalla media
# per una migliore visualizzaœone del grafico rimuovo questo valore

superficie0 <- na.omit(case[case$TotalBsmtSF < max(case$TotalBsmtSF), "TotalBsmtSF"])
display_summary_and_var(superficie0)
hist(superficie0, freq = F, main = "distribuzione senza valore outlier")
boxplot(superficie0, horizontal = T)
skewness(superficie0)

par(mfrow=c(1,1))



# Heating
case$Heating <- factor(case$Heating)
prop.table(table(case$Heating))
barplot(prop.table(table(case$Heating)), main = "Frequenza tipologia di riscaldamento")
# variabile qualitativa categoriale con 6 diverse categorie, 
# la maggior parte delle case nel campione (ben il 97.8%) ha utilizza un sistema di riscaldamento di tipo "Gas forced warm air furnace"


# HeatingQC
case$HeatingQC <- factor(case$HeatingQC, levels = c("Po","Fa","TA","Gd","Ex"))
levels(case$HeatingQC) <- c("Poor","Fair","Average/Typical","Good","Excellent")
prop.table(table(case$HeatingQC))
barplot(prop.table(table(case$HeatingQC)), main = "Qualità e condizione del riscaldamento")
# analogamente alle variabili precedenti,
# variabile qualitativa categoriale con 5 diverse categorie, 
# si è scelto di riordinare i levels nel seguente ordine: "Poor","Fair","Average/Typical","Good","Excellent"
# la maggioranza delle case nel campione (50.7%) ha una eccellente qualità e condizione del riscaldamento



