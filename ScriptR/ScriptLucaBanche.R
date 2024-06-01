library(moments)

banche <- read.csv("BankChurners.csv", header=TRUE)

#Funzione per le Variabili Quantitative
display_summary_and_var <- function(variabile){
  c(summary(variabile), 
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

#-----------------------------------------------------------------------------------------------------

#Marital_Status: Variabile Character che indica lo stato di relazione della
#                persona che possiede il conto in banca.
#                Vediamo che le categorie più popolari sono "Merried" e "single"

Marital_Status_F <- factor(banche$Marital_Status)
Marital_Status_F <- ordered(Marital_Status_F, levels = c("Single", "Married", "Divorced", "Unknown"))
display_table(Marital_Status_F)


#Income_Category: anche se la variabile potrebbe essere vista come una variabile
#                 Numerica che rappresenta il reddito diviso in classi, questa 
#                 ci viene invece fornita come variabile Character che verrà
#                 ordinata a mano.
#                 La "Classe" più comune è "Less than $40K" e la più rara 
#                 "$120K +".

Income_Category_F <- factor(banche$Income_Category)
Income_Category_F <- ordered(Income_Category_F, levels = c("Less than $40K","$40K - $60K","$60K - $80K","$80K - $120K","$120K +","Unknown"))
display_table(Income_Category_F)


#Card_Category: è una variabile Character che indica la categoria di carta del
#               della persona che possiede il conto. Considerando che i "livelli"
#               di una carta di credito si spostano generalmente da bronzo fino
#               a platino si riordiniamo i livelli prima di continuare le 
#               osservazioni (nel nostro caso il "Bronzo" corrisponderà al "Blue")
#               Le carte rilasciate sono quasi esclusivamente del tipo "Blue".

Card_Category_F <- factor(banche$Card_Category)
Card_Category_F <- ordered(Card_Category_F, levels = c("Blue","Silver","Gold","Platinum"))
display_table(Card_Category_F)


#Months_on_book: variabile di tipo Quantitativo che indica il numero di mesi che
#                che un cliente ha passato come cliente della Banca
#                Vediamo che è presente un picco tra 33 e 36 che va ben sopra 
#                la gaussiana costruita con Media e SD della distribuzione.
#                Moda e Mediana sono estremamente vicine.

display_summary_and_var(banche$Months_on_book)
hist(banche$Months_on_book, breaks = c(3*0:20), probability = T)
curve(dnorm(x,mean(banche$Months_on_book, na.rm = T), sd(banche$Months_on_book, na.rm = T)),add = T)
abline(v = median(banche$Months_on_book, na.rm = T),lwd = 2, col = "red")
abline(v = mean(banche$Months_on_book, na.rm = T),lwd = 2, col = "green")


#Total_Relationship_Count: è una variabile numerica che rappresenta il numero
#                          totale di prodotti della banca posseduto dal utente.
#                          Raramente i clienti possiedono solo una o due carte e  
#                          si nota che, anche se la moda è 3, sia media che mediana
#                          sono vicine al 4

display_summary_and_var(banche$Total_Relationship_Count)
hist(banche$Total_Relationship_Count, breaks = c(0:7)+0.5,ylim= c(0,0.26), probability = T)
abline(v = median(banche$Total_Relationship_Count, na.rm = T),lwd = 2, col = "red")
abline(v = mean(banche$Total_Relationship_Count, na.rm = T),lwd = 2, col = "green")
