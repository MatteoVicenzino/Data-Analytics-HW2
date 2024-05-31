library(moments)
library(ggplot2)
case <- read.csv("C:\\Users\\giova\\Downloads\\train.csv", stringsAsFactors = TRUE)

# bivariata
# sales_price variabile target

#sales_price vs MSSubClass
ggplot(case, aes(x = as.factor(MSSubClass), y = SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# fai grafico a violino
ggplot(case, aes(x = as.factor(MSSubClass), y = SalePrice)) + geom_violin() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

cor(case$MSSubClass, case$SalePrice)
modello <- lm(SalePrice ~ MSSubClass, data = case)
summary(modello)
ggplot(case, aes(x = MSSubClass, y = SalePrice)) + geom_point() + geom_smooth(method = "lm")

# aggiungi qualche commento sulle informazioni ottenute
# la variabile MSSubClass non sembra avere un impatto significativo sul prezzo di vendita

#sales_price vs MSZoning
ggplot(case, aes(x = as.factor(MSZoning), y = SalePrice)) + geom_boxplot() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# fai grafico a violino
ggplot(case, aes(x = as.factor(MSZoning), y = SalePrice)) + geom_violin() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

modello <- lm(SalePrice ~ MSZoning, data = case)
summary(modello)
deviance(modello)
ggplot(case, aes(x = MSZoning, y = SalePrice)) + geom_point() + geom_smooth(method = "lm")

# aggiungi qualche commento sulle informazioni ottenute
# la variabile MSZoning sembra avere un impatto significativo sul prezzo di vendita
