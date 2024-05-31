train <- read.csv('homework/homework 2/house-prices-advanced-regression-techniques/train.csv')
case <- train[, 42:61]
case$SalePrice <- train$SalePrice

#funzione che calcola covarianza e indice di correlazione lineare
calcolo_cov_cor <- function (variabile_numerica){
  c(cov = cov(variabile_numerica, case$SalePrice), cor = cor(variabile_numerica, case$SalePrice))}

#sistema eletrico

case$Electrical <- factor(case$Electrical)
#plot violino colorato
ggplot(case, aes(x = as.factor(x = Electrical), y = SalePrice, fill = as.factor(x = Electrical))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = case$Electrical)

#superficie primo piano

#scatter plo tdei punti
ggplot(data = case , aes(x=X1stFlrSF, y=SalePrice)) +
  geom_point(shape=1) 
calcolo_cov_cor(case$X1stFlrSF)
#modello di regressione lineare
lmodel <- lm(data = case, formula = (X1stFlrSF~SalePrice))
summary(lmodel)
#plot della retta dei minimi quadrati 
ggplot(data = case , aes(x=log(X1stFlrSF), y=log(SalePrice))) +
  geom_point(shape=1) + geom_smooth(method = 'lm', se = F) 

#superficie secondo piano

ggplot(data = case , aes(x=X2ndFlrSF, y=SalePrice)) +
  geom_point(shape=1) 
#plot delle sole case che hanno un secondo piano
ggplot(data = subset(case, X2ndFlrSF != 0) , aes(x=X2ndFlrSF, y=SalePrice)) +
  geom_point(shape=1)

calcolo_cov_cor(case$X2ndFlrSF)

lmodel <- lm(data = case, formula = (X1stFlrSF~SalePrice))
summary(lmodel)

lmodel_case_con_2_piano <- lm(data = subset(case, X2ndFlrSF != 0), formula = (X2ndFlrSF~SalePrice))
summary(lmodel_case_con_2_piano)

ggplot(data = subset(case, X2ndFlrSF != 0) , aes(x=log(X2ndFlrSF), y=log(SalePrice))) +
  geom_point(shape=1) + geom_smooth(method = 'lm', se = F) 

#Low quality finished square feet (all floors)

#siccome sono quasi tutti zero verifico se vi è una differenza tra l'essere ugale a zero e l'essere diverso

case$LowQualFinSF <- factor(case$LowQualFinSF, levels = c(0, 'non zero')); 
replace(case$LowQualFinSF,  is.na(case$LowQualFinSF), 'non zero')

ggplot(case, aes(x = LowQualFinSF, y = SalePrice, fill = as.factor(x = LowQualFinSF))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = F)


# Above grade (ground) living area square feet

ggplot(data = case , aes(x=GrLivArea, y=SalePrice)) +
  geom_point(shape=1) 
calcolo_cov_cor(case$GrLivArea)

lmodel <- lm(data = case, formula = (GrLivArea~SalePrice))
summary(lmodel)

ggplot(data = case , aes(x=log(GrLivArea), y=log(SalePrice))) +
  geom_point(shape=1) + geom_smooth(method = 'lm', se = F) 

#Basement full bathrooms

ggplot(case, aes(x = as.factor(x = BsmtFullBath), y = SalePrice, fill = as.factor(x = BsmtFullBath))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$BsmtFullBath))

#Basement half bathrooms

ggplot(case, aes(x = as.factor(x = BsmtHalfBath), y = SalePrice, fill = as.factor(x = BsmtHalfBath))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$BsmtHalfBath))


#Full bathrooms above grade

ggplot(case, aes(x = as.factor(x = FullBath), y = SalePrice, fill = as.factor(x = FullBath))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$FullBath))


#Half baths above grade

ggplot(case, aes(x = as.factor(x = HalfBath), y = SalePrice, fill = as.factor(x = HalfBath))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$HalfBath))


#Bedroom above grade

ggplot(case, aes(x = as.factor(x = BedroomAbvGr), y = SalePrice, fill = as.factor(x = BedroomAbvGr))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$BedroomAbvGr))

#Kitchens above grade

ggplot(case, aes(x = as.factor(x = KitchenAbvGr), y = SalePrice, fill = as.factor(x = KitchenAbvGr))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$KitchenAbvGr))


#Kitchen quality

ggplot(case, aes(x = as.factor(x = KitchenQual), y = SalePrice, fill = as.factor(x = KitchenQual))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$KitchenQual))


#Total rooms above grade

ggplot(case, aes(x = as.factor(x = TotRmsAbvGrd), y = SalePrice, fill = as.factor(x = TotRmsAbvGrd))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$TotRmsAbvGrd))


#Home functionality

ggplot(case, aes(x = as.factor(x = Functional), y = SalePrice, fill = as.factor(x = Functional))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$Functional))


#Number of fireplaces

ggplot(case, aes(x = as.factor(x = Fireplaces), y = SalePrice, fill = as.factor(x = Fireplaces))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$Fireplaces))


#Fireplace quality

case$FireplaceQu
ggplot(case, aes(x = as.factor(x = FireplaceQu), y = SalePrice, fill = as.factor(x = FireplaceQu))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$FireplaceQu))


#Garage location

case$GarageType
ggplot(case, aes(x = as.factor(x = GarageType), y = SalePrice, fill = as.factor(x = GarageType))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$GarageType))

#Year garage was built

ggplot(data = case , aes(x=GarageYrBlt, y=SalePrice)) +
  geom_point(shape=1) 
calcolo_cov_cor(case$X1stFlrSF)

lmodel <- lm(data = case, formula = (X1stFlrSF~SalePrice))
summary(lmodel)

ggplot(data = case , aes(x=log(X1stFlrSF), y=log(SalePrice))) +
  geom_point(shape=1) + geom_smooth(method = 'lm', se = F) 


# Interior finish of the garage


case$GarageFinish
ggplot(case, aes(x = as.factor(x = GarageFinish), y = SalePrice, fill = as.factor(x = GarageFinish))) + 
  geom_violin() + geom_boxplot(width=0.2, alpha=1/5) + guides(fill = FALSE)
calcola_devianza(numerical_var = case$SalePrice, categorical_var = as.factor(case$GarageFinish))


