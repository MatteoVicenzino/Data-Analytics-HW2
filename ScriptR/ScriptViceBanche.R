library(moments)

banche <- read.csv("~/Documents/UNI/2_anno_23_24/Analytics/6_HW2/Dati/BankChurners.csv", header=TRUE)

ncol(banche)
str(banche)
summary(banche)

# funzione che prende in input una variabile quantitativa e stampa summary + varianza e dev. standared in una riga
display_summary_and_var <- function(variabile){
  c(summary(variabile), 
    var = var(variabile, na.rm = T), 
    sd = sd(variabile, na.rm = T))
}


# si nota che la prima colonna indica il numero identificativo del cliente
# quindi non verrà analizzata



# Attrition_Flag
banche$Attrition_Flag <- factor(banche$Attrition_Flag)
prop.table(table(banche$Attrition_Flag))
barplot(matrix(prop.table(table(banche$Attrition_Flag))))
# si nota che l'83.9% dei dati riguarda clienti esistenti della banca
# mentre il 16.1% dei dati riguarda clienti persi / clienti passati



# Customer_Age
display_summary_and_var(banche$Customer_Age)
hist(banche$Customer_Age, freq = F, main = "distribuzione età")
skewness(banche$Customer_Age)
boxplot(banche$Customer_Age, horizontal = T)
# la variabile giguardante l'età dei clienti è una variabile quantitativa,
# si nota che l'età media del campione è 46.33 e la mediana è 46.00, i valori sono abbastanza vicini
# infati se si calcola l'indice di asimmetria attraverso la fnuzione skewness() si ottiene 
# un numero prossimo allo zero
# infine dal boxplot si evince che 2 clienti hanno come età dei valori outliers



# Gender
banche$Gender <- factor(banche$Gender)
prop.table(table(banche$Gender))
barplot(prop.table(table(banche$Gender)))
# La variabile Gender è una variabile qualitativa che presenta le seguenti frequenze relative:
# si osserva che la frequenza delle donne è leggermente maggiore



# Dependent_count
display_summary_and_var(banche$Dependent_count)
barplot(prop.table(table(banche$Dependent_count)))

# è una variabile quantitativa discreta, che ha come minimo 0 e come massimo 5
# la media dei valori è 2.346



# Education_level
banche$Education_Level <- factor(banche$Education_Level, levels = c("Unknown","Uneducated","High School", "College", "Graduate", "Post-Graduate", "Doctorate") )
prop.table(table(banche$Education_Level))
barplot(prop.table(table(banche$Education_Level)))
# La variabile Gender è una variabile qualitativa che presenta le seguenti frequenze relative:
# si osserva che la frequenza maggiore è quella relativa ai clienti "Graduate", del 30.8% 
# e si osserva inoltre che non si conoscono i dati di circa il 15% del nostro campione


