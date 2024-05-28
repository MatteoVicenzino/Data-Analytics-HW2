banche <- read.csv("~/Documents/UNI/2_anno_23_24/Analytics/6_HW2/Dati/BankChurners.csv", header=TRUE)

ncol(banche)

str(banche)
summary(banche)

library(moments)


# Attrition_Flag
banche$Attrition_Flag <- factor(banche$Attrition_Flag)
prop.table(table(banche$Attrition_Flag))
barplot(matrix(prop.table(table(banche$Attrition_Flag))))



# Customer_Age
summary(na.omit(banche$Customer_Age))
var(banche$Customer_Age)
sd(banche$Customer_Age)
skewness(banche$Customer_Age)
hist(banche$Customer_Age, freq = F, main = "distribuzione età")
boxplot(banche$Customer_Age, horizontal = T)


# Gender
banche$Gender <- factor(banche$Gender)
prop.table(table(banche$Gender))
barplot(prop.table(table(banche$Gender)))

# La variabile Gender è una variabile qualitativa che presenta le seguenti frequenze relative:
# si osserva che la frequenza delle donne è leggermente maggiore



# Dependent_count
summary(na.omit(banche$Dependent_count))
var(banche$Dependent_count)
sd(banche$Dependent_count)
skewness(banche$Dependent_count)
hist(banche$Dependent_count, freq = T, main = "distribuzione conto")


