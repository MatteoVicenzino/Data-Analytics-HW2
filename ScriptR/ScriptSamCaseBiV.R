case <- train[, 42:61]

case$SalePrice <- train$SalePrice

library('ggplot2')

# aria centralizzata

case$CentralAir <- factor(case$CentralAir)
ggplot(case, aes(x = as.factor(x = CentralAir), y = SalePrice, fill = as.factor(x =CentralAir))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = case$CentralAir)

# il sistema di aria centralizzato non sembra avere una grande influenza sul prezzo, si osserva che le
# mediane fra i due gruppi sono simili. Si osserva inoltre una maggiore dispersione nelle case del gruppo yes

# sistema eletrico

case$Electrical <- factor(case$Electrical)
ggplot(case, aes(x = as.factor(x = Electrical), y = SalePrice, fill = as.factor(x = Electrical))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$Electrical))

# il tipo di sistema elettrico non sembra avere influenza sul prezzo, le mediane dei gruppi sono simili, il gruppo SBrkr cioè il sistema elettrico standard ha la varianza maggiore, bisogna però notare che è il gruppo con il numero maggiore di elementi

# superficie primo piano

ggplot(data = case , aes(x=X1stFlrSF, y=SalePrice)) +
  geom_point(shape=1) 
calcolo_cov_cor(case$X1stFlrSF)

lmodel <- lm(data = case, formula = (X1stFlrSF~SalePrice))
summary(lmodel)

ggplot(data = case , aes(x=log(X1stFlrSF), y=log(SalePrice))) +
  geom_point(shape=1) + geom_smooth(method = 'lm', se = F) 

# abbiamo un coefficiente di correlazione lineare di circa 0.6, dal modello lineare 
# osserviamo un valore di R^2 di circa 0.366.

# superficie secondo piano
ggplot(data = case , aes(x=X2ndFlrSF, y=SalePrice)) +
  geom_point(shape=1) 

ggplot(data = subset(case, X2ndFlrSF != 0) , aes(x=X2ndFlrSF, y=SalePrice)) +
  geom_point(shape=1)

calcolo_cov_cor(case$X2ndFlrSF)

lmodel <- lm(data = case, formula = (X1stFlrSF~SalePrice))
summary(lmodel)


lmodel_case_con_2_piano <- lm(data = subset(case, X2ndFlrSF != 0), formula = (X2ndFlrSF~SalePrice))
summary(lmodel_case_con_2_piano)

ggplot(data = subset(case, X2ndFlrSF != 0) , aes(x=log(X2ndFlrSF), y=log(SalePrice))) +
  geom_point(shape=1) + geom_smooth(method = 'lm', se = F) 

# osserviamo che vi è un discreto numero di case che non ha un secondo piano, plottando i punti vi è una concertazione di punti nell'ascissa
# 0. Facendo il modello di regresione lineare sia sul set completo che sul subset senza gli zeri si not ache in quest'ultimo il valore di R^2 aumenta  
# quasi del 10%. 

# lowqualityfinsf

case$LowQualFinSF<- factor(replace(case$LowQualFinSF, (case$LowQualFinSF) > 0, "Non zero"))
calcola_devianza(case$SalePrice, case$LowQualFinSF)
ggplot(case, aes(x = LowQualFinSF, y = SalePrice, fill = as.factor(x = LowQualFinSF))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = F)


# la maggior parte dei valori è 0, divido in due gruppi uno in cui il valore è diverso da zero e uno in cui è uguale. Dal grafico e dell'analisi della devianza si osserva che le mediane dei due gruppi soo molto vivine tra loro


# Above grade (ground) living area square feet

ggplot(data = case , aes(x=GrLivArea, y=SalePrice)) +
  geom_point(shape=1) 
calcolo_cov_cor(case$GrLivArea)

lmodel <- lm(data = case, formula = (GrLivArea~SalePrice))
summary(lmodel)

ggplot(data = case , aes(x=log(GrLivArea), y=log(SalePrice))) +
  geom_point(shape=1) + geom_smooth(method = 'lm', se = F) 

# R^2 vale 0.50 e il coefficiente di correlazione lineare 0.70 dunque vi è una forte correlazione lineare tra le variabili.

# Basement full bathrooms

ggplot(case, aes(x = as.factor(x = BsmtFullBath), y = SalePrice, fill = as.factor(x = BsmtFullBath))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$BsmtFullBath))

# la devianza tra i gruppi è bassa. Le mediane dei gruppi sono simili mentra la devianza entro i gruppi, dal grafico si nota che le case con 0 e 1 bagno hanno una varianza maggiore

# Basement half bathrooms

ggplot(case, aes(x = as.factor(x = BsmtHalfBath), y = SalePrice, fill = as.factor(x = BsmtHalfBath))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + labs(x = 'numero half bath')
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$BsmtHalfBath))

#  la devianza tra i gruppi è bassa. Le mediane dei gruppi sono simili mentre la devianza entro i gruppi, dal grafico si nota che le case con 0 e 1 bagno hanno una varianza maggiore

# Full bathrooms above grade

ggplot(case, aes(x = as.factor(x = FullBath), y = SalePrice, fill = as.factor(x = FullBath))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + labs(x = 'numero full bath')
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$FullBath))
# la devianza tra i gruppi è alta. Dal grafico si nota che il prezzo è influenzato dal numero di full bath above grade, anche la devianza entro i gruppi è alta. 

#Half baths above grade

ggplot(case, aes(x = as.factor(x = HalfBath), y = SalePrice, fill = as.factor(x = HalfBath))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE) + labs(x = 'numero half bath above grade')
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$HalfBath))
#  la devianza entro i gruppi è maggiore della devianza tra i gruppi, il coefficiente eta^2 è basso. Le case nel gruppo 1 sono quelle che presentano una maggiore varianza.

#Bedroom above grade

ggplot(case, aes(x = as.factor(x = BedroomAbvGr), y = SalePrice, fill = as.factor(x = BedroomAbvGr))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$BedroomAbvGr))

# Le case con 4 camere da letto sono quelle che presentano la maggiore varianza. La devianza entro i gruppi è maggiore di quella tra i gruppi. il coefficienta eta^2 è basso.

#Kitchens above grade

ggplot(case, aes(x = as.factor(x = KitchenAbvGr), y = SalePrice, fill = as.factor(x = KitchenAbvGr))) + 
 geom_boxplot() + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$KitchenAbvGr))

# la devianza tra i gruppi è bassa, il coefficiente eta^2 vale circa 0.0199. La correlazione è bassa.

#Kitchen quality

ggplot(case, aes(x = as.factor(x = KitchenQual), y = SalePrice, fill = as.factor(x = KitchenQual))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$KitchenQual))

#il coefficiente eta^2 vale circa 0.457 e dal grafico si nota una correlazione tra il prezzo della casa e la qualità dellla cucina. Notiamo una maggiore varianza nelle case che hanno una cicina eccellente.

#Total rooms above grade

ggplot(case, aes(x = as.factor(x = TotRmsAbvGrd), y = SalePrice, fill = as.factor(x = TotRmsAbvGrd))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$TotRmsAbvGrd))
# il coefficiente eta^2 vale 0.2991 graficamente osserviamo una chiara correlazione tra il numero delle stanza above grade e il prezzo.

#Home functionality

ggplot(case, aes(x = as.factor(x = Functional), y = SalePrice, fill = as.factor(x = Functional))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$Functional))

# il coefficiente eta^2 vale circa 0.165 e la devianza tra i gruppi e molto più bassa della devianza entro i gruppi. Non sembra esserci una correlazione tra l'appartenere ad un gruppo e il prezzo.

#Number of fireplaces

ggplot(case, aes(x = as.factor(x = Fireplaces), y = SalePrice, fill = as.factor(x = Fireplaces))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$Fireplaces))

# il coefficiente eta^2 vale 0.2322 la devianza entro i gruppi e la devianza tra i gruppi hhanno lo stesso ordine di grandezza 10^12, sembra esserci correlazione tra le variabili.

#Fireplace quality

ggplot(case, aes(x = as.factor(x = FireplaceQu), y = SalePrice, fill = as.factor(x = FireplaceQu))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$FireplaceQu))

#Dal grafico si evince una correlazione tra Fireplace quality e il prezzo. 

#Garage location

ggplot(case, aes(x = as.factor(x = GarageType), y = SalePrice, fill = as.factor(x = GarageType))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$GarageType))

# il coefficiente eta^2 è circa  0.3234, sembra esserci correlazione tra le due variabili. IL gruppo che presenta la maggiore varianza è il gruppo delle case con il garage di tipo Attached , si ricordi che questas il gruppo con la numerosità maggiore. 

#Year garage was built

ggplot(data = case , aes(x=GarageYrBlt, y=SalePrice)) +
  geom_point(shape=1) 
calcolo_cov_cor(case$X1stFlrSF)

lmodel <- lm(data = case, formula = (X1stFlrSF~SalePrice))
summary(lmodel)

ggplot(data = case , aes(x=log(X1stFlrSF), y=log(SalePrice))) +
  geom_point(shape=1) + geom_smooth(method = 'lm', se = F) 

#il coefficiente di correlazione lineare vale 0.6, dal modello di regressione lineare osserviamo un valore di  R^2 pari a 0.366.

# Interior finish of the garage

ggplot(case, aes(x = as.factor(x = GarageFinish), y = SalePrice, fill = as.factor(x = GarageFinish))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$GarageFinish))

#il valore di eta^2 è circa 0.3, sembra esserci correlazione tra le variabili. Nel gruppo di case che hanno il garage ultimato si riscontra lamaggiore varianza.

