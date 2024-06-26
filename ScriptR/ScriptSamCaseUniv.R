
case <- train[, 42:61]


#variabile che rappresenta l'assenza o la presenza del central air conditioning

case$CentralAir <- factor(case$CentralAir)
display_table(case$CentralAir, titolo = 'central air conditioning')

#si osserva che il 93% circa delle case ha il central air conditioning


#variabile che rappresenta il sistema elettrico

case$Electrical <- factor(case$Electrical)
display_table(case$Electrical, titolo = 'sistema elettrico')

#vi sono 4 tipi diversi, il 91% delle case ha il circuito standard, il restante 9% si dovide negli altri tre tipi
#il 6% è rappresentato dal tipo FuseA

#superficie del primo piano

display_summary_and_var(case$X1stFlrSF)
hist(case$X1stFlrSF, probability = T, main = 'superficie primo piano')

#la maggior parte delle case si trova nel range 800-1200 piedi quadrati

#superficie secondo piano

display_summary_and_var(case$X2ndFlrSF)
hist(case$X2ndFlrSF, probability = T, main = 'superficie secondo piano')

#la maggior parte delle case (il 56 %) non possiede un secondo piano, prendendo in considerazione solo quelle che lo hanno
#osserviamo una media di 776 ft^2 e una mediana di 776 ft^2

display_summary_and_var(subset(case$X2ndFlrSF, case$X2ndFlrSF != 0))
hist(subset(case$X2ndFlrSF, case$X2ndFlrSF != 0), main = 'istogramma case con un secondo piano', probability = T)

#low quality finished square feet (all floors)

display_summary_and_var(case$LowQualFinSF)
hist(case$LowQualFinSF, prob = T, main = 'low quality finished square feet')
length(case$LowQualFinSF[case$LowQualFinSF == 0])

#si osserva che 1434 case su 1460 hanno 0 low quality finished square feet

#metri quadri sopra il terreno

display_summary_and_var(case$GrLivArea)
hist(case$GrLivArea, probability = T)
boxplot(case$GrLivArea)
points(mean(case$GrLivArea), col = 'red')

#usando il boxplot vediamo che la media è vicina alla mediana (intorno a 1500) e che vi è una bassa varianza, tuttavia sono presenti 
#molti outliers che hanno più 3000 piedi quadrati di superficie abitablie sopra il suolo

#numero basement full bathroom

case$BsmtFullBath <- factor(case$BsmtFullBath)
display_table(case$BsmtFullBath, titolo = 'Basement full bath')

#presenta solo i caratteri 0, 1 , 2 e 3. il 59% delle case ha 0, il 40% 1

#numero basement half bathroom

case$BsmtHalfBath <- factor(case$BsmtHalfBath)
display_table(case$BsmtHalfBath, titolo = 'Basement half bath')

#presenta i caratteri 0, 1 e 2, il 94% delle case ha 0, il 5% 1

#numero camere sopra il livello del suolo, non include le camerdel basement.

case$BedroomAbvGr <- factor(case$BedroomAbvGr)
display_table(case$BedroomAbvGr, titolo = 'Bedroom above grade')

#il 55% delle case ne ha 3. 

#numero di cucine 

case$KitchenAbvGr <- factor(case$KitchenAbvGr)
display_table(case$KitchenAbvGr, 'Kitchen above grade')

#il 95% delle case ne ha 1

#qualità delle cucine

case$KitchenQual <- factor(case$KitchenQual, levels = c('Ex', 'Fa', 'Gd', 'TA', 'Po'))
display_table(case$KitchenQual, titolo = 'Kitchen quality')

#il 50% delle cucine è a un livello Average e il 40% a un livelo good

#totale stanze above grade, non include i bagni

display_summary_and_var(case$TotRmsAbvGrd)
rbind(tot = table(case$TotRmsAbvGrd),
      prop = prop.table(table(case$TotRmsAbvGrd)))
boxplot(case$TotRmsAbvGrd)

#il 28% ha 6 stanze, #il 22% 7

#home functionality

case$Functional <- factor(case$Functional)
display_table(case$Functional, titolo = 'home functionality')

#il 93% delle case presenta typical functionality

#fire places

case$Fireplaces <- factor(case$Fireplaces)
display_table(case$Fireplaces, titolo = 'Fireplaces')

#il 47% delle case ha 0, il 44% ha 1

#qualità fire place

case$FireplaceQu <- factor(case$FireplaceQu)
display_table(case$FireplaceQu, titolo = 'fireplace quality')

#il 41% ha una qualità average, il 49% ha una qualità good

#posizione garage

case$GarageType <- factor(case$GarageType)
display_table(case$GarageType, titolo = 'Garage type')

#il 63% ha il garage affiancato alla casa

#stato garage

case$GarageFinish <- factor(replace(case$GarageFinish, is.na(case$MSSubClass), "Non Presente"), 
                            levels = c('Fin', 'RFn', 'Unf', 'NA'))
display_table(case$GarageFinish, titolo = 'Garage finish')

#il 43% delle case ha il garage non finito


#anno costruzione garage

hist(case$GarageYrBlt, probability = T,breaks= c(5*0:28)+1870,  main = 'anno costruzione garage')
abline(v = median(case$GarageYrBlt, na.rm = T),lwd = 1, col = "red")
abline(v = mean(case$GarageYrBlt, na.rm = T),lwd = 1)

# si not aun calo tra il 1980 e il 1990 e un picco nei primi anni del 2000
