  library(cluster)
  library(ggplot2)
  library(ggcorrplot)
  library(psych)
  library(nortest)
  
  library(caret) #machine learning
  
  setwd("/Users/ccastillo/OneDrive - Universitat Oberta de Catalunya/Tipología y Ciclo de Vida de los Datos/Practica2")
  
  wine_red <- read.csv("winequality-red.csv", header=TRUE, sep =",", dec=".")
  wine_white <- read.csv("winequality-white.csv", header=TRUE, sep =";", dec=".")
  wine_red$type_wine = 1
  wine_white$type_wine = 0
  
  wine <- rbind(wine_red,wine_white)
  
  
  head(wine)
  summary(wine)
  
  sapply(wine,class)

#correlacion

corr<-cor(wine)
ggcorrplot(corr, hc.order = TRUE, type = "lower",lab = TRUE)
ggcorrplot(corr)
corr

wine <- wine[,-6] #eliminamos free.sulfur.dioxide
wine <- wine[,-7] #eliminamos density




# buscamos variables y registros con valores perdidos
# la columna/s que tienen valores perdidos
colnames(wine)[colSums(is.na(wine)) > 0]
# la fila/s y n? de columna/s que tienen valores perdidos
which(is.na(wine), arr.ind=TRUE)





# outliers

#first, just find and graph the outliers

d2 <- outlier(wine)
sat.d2 <- data.frame(wine,d2)
outlier(wine, plot = TRUE, bad = 5,na.rm = TRUE)

d2_order<-sat.d2[order(-sat.d2$d2),]
head(d2_order,5) # para ver como va quedando.



#borramos outliers

wine <- d2_order[d2_order$d2<170,]
wine <- wine[,-12]# borramos columna d2




boxplot(wine$fixed.acidity,ylab='Fixed Acidity')
boxplot.stats(wine$fixed.acidity)$out
boxplot(fixed.acidity ~ quality, data=wine, ylab='Fixed Acidity',main='Fixed Acidity by Quality',col='skyblue')
boxplot(fixed.acidity ~ type_wine, data=wine, ylab='Fixed Acidity',main='Fixed Acidity by Type Wine',col='skyblue')


boxplot(wine$volatile.acidity,ylab='Volatile Acidity')
boxplot.stats(wine$volatile.acidity)$out
boxplot(volatile.acidity ~ quality, data=wine, ylab='Volatile Acidity',main='Volatile Acidity by Quality',col='skyblue')
boxplot(volatile.acidity ~ type_wine, data=wine, ylab='Volatile Acidity',main='Volatile Acidity by Type Wine',col='skyblue')


boxplot(wine$citric.acid,ylab='Citric Acid')
boxplot.stats(wine$citric.acid)$out
boxplot(citric.acid ~ quality, data=wine, ylab='Citric Acid',main='Citric Acid by Quality',col='skyblue')
boxplot(citric.acid ~ type_wine, data=wine, ylab='Citric Acid',main='Citric Acid by Type Wine',col='skyblue')


boxplot(wine$residual.sugar,ylab='Residual Sugar')
boxplot.stats(wine$residual.sugar)$out
boxplot(residual.sugar ~ quality, data=wine, ylab='Residual Sugar',main='Residual Sugar by Quality',col='skyblue')
boxplot(residual.sugar ~ type_wine, data=wine, ylab='Residual Sugar',main='Residual Sugar by Type Wine',col='skyblue')


boxplot(wine$chlorides,ylab='Chlorides')
boxplot.stats(wine$chlorides)$out
boxplot(chlorides ~ quality, data=wine, ylab='Chlorides',main='Chlorides by Quality',col='skyblue')
boxplot(chlorides ~ type_wine, data=wine, ylab='Chlorides',main='Chlorides by Type Wine',col='skyblue')


#boxplot(wine$free.sulfur.dioxide,ylab='Free Sulfur Dioxide')
#boxplot.stats(wine$free.sulfur.dioxide)$out
#boxplot(free.sulfur.dioxide ~ quality, data=wine, ylab='Free Sulfur Dioxide',main='Free Sulfur Dioxide by Quality',col='skyblue')
#boxplot(free.sulfur.dioxide ~ type_wine, data=wine, ylab='Free Sulfur Dioxide',main='Free Sulfur Dioxide by Type Wine',col='skyblue')




boxplot(wine$total.sulfur.dioxide,ylab='Total Sulfur Dioxide')
boxplot.stats(wine$total.sulfur.dioxide)$out
boxplot(total.sulfur.dioxide ~ quality, data=wine, ylab='Total Sulfur Dioxide',main='Total Sulfur Dioxide by Quality',col='skyblue')
boxplot(total.sulfur.dioxide ~ type_wine, data=wine, ylab='Total Sulfur Dioxide',main='Total Sulfur Dioxide by Type Wine',col='skyblue')


#boxplot(wine$density,ylab='Density')
#boxplot.stats(wine$density)$out
#boxplot(density ~ quality, data=wine, ylab='Density',main='Density by Quality',col='skyblue')
#boxplot(density ~ type_wine, data=wine, ylab='Density',main='Density by Type Wine',col='skyblue')


boxplot(wine$pH,ylab='pH')
boxplot.stats(wine$pH)$out
boxplot(pH ~ quality, data=wine, ylab='pH',main='pH by Quality',col='skyblue')
boxplot(pH ~ type_wine, data=wine, ylab='pH',main='pH by Type Wine',col='skyblue')


boxplot(wine$sulphates,ylab='Sulphates')
boxplot.stats(wine$sulphates)$out
boxplot(sulphates ~ quality, data=wine, ylab='Sulphates',main='Sulphates by Quality',col='skyblue')
boxplot(sulphates ~ type_wine, data=wine, ylab='Sulphates',main='Sulphates by Type Wine',col='skyblue')


boxplot(wine$alcohol,ylab='Alcohol')
boxplot.stats(wine$alcohol)$out
boxplot(alcohol ~ quality, data=wine, ylab='Alcohol',main='Alcohol by Quality',col='skyblue')
boxplot(alcohol ~ type_wine, data=wine, ylab='Alcohol',main='Alcohol by Type Wine',col='skyblue')

boxplot(wine$quality,ylab='Quality')
boxplot.stats(wine$quality)$out
boxplot(quality ~ type_wine, data=wine, ylab='Quality',main='Quality by Type Wine',col='skyblue')


hist(wine$quality ) 



#Homogeneidad de la varianza

bartlett.test(wine$fixed.acidity,wine$quality)
bartlett.test(wine$volatile.acidity,wine$quality)
bartlett.test(wine$citric.acid,wine$quality)
bartlett.test(wine$residual.sugar,wine$quality)
bartlett.test(wine$chlorides,wine$quality)
#bartlett.test(wine$free.sulfur.dioxide,wine$quality)
bartlett.test(wine$total.sulfur.dioxide,wine$quality)
#bartlett.test(wine$density,wine$quality)
bartlett.test(wine$pH,wine$quality)
bartlett.test(wine$sulphates,wine$quality)
bartlett.test(wine$alcohol,wine$quality)
bartlett.test(wine$type_wine,wine$quality)


lillie.test(wine$fixed.acidity)
lillie.test(wine$volatile.acidity)
lillie.test(wine$citric.acid)
lillie.test(wine$residual.sugar)
lillie.test(wine$chlorides)
#lillie.test(wine$free.sulfur.dioxide)
lillie.test(wine$total.sulfur.dioxide)
#lillie.test(wine$density)
lillie.test(wine$pH)
lillie.test(wine$sulphates)
lillie.test(wine$alcohol)

#aplicamos el modelo


set.seed(92)



trainIndexRed <- createDataPartition(wine_red$quality, p = .7,
                                  list = FALSE,
                                  times = 1)
trainIndexWhite <- createDataPartition(wine_white$quality, p = .23,
                                     list = FALSE,
                                     times = 1)
trainRed <- wine_red[trainIndexRed,]
testRed <- wine_red[-trainIndexRed,]

trainWhite <- wine_white[trainIndexWhite,]
testWhite <- wine_white[-trainIndexWhite,]

train <- rbind(trainWhite,trainRed)
test <- rbind(testWhite,testRed)

Model_lm<- lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+total.sulfur.dioxide+pH+alcohol+sulphates, 
              data=train )
summary(Model_lm)



Model_lm_wine <- predict(Model_lm, test, interval= c("confidence"))
Mwine <- data.frame(test,Model_lm_wine)
Mwine$fit <- round(Mwine$fit)

positiveMwine <- nrow(Mwine[Mwine$quality==Mwine$fit,])
total <- nrow(Mwine)

positiveMwine/total

# Aplicamos el modelo para test Red y test White
Model_lm_red <- predict(Model_lm, testRed, interval= c("confidence"))

Mred <- data.frame(testRed,Model_lm_red)
Mred$fit <- round(Mred$fit)

positiveMred <- nrow(Mred[Mred$quality==Mred$fit,])
total <- nrow(Mred)
positiveMred/total

Model_lm_white <- predict(Model_lm, testWhite, interval= c("confidence"))
Mwhite <- data.frame(testWhite,Model_lm_white)
Mwhite$fit <- round(Mwhite$fit)

positiveMwhite <- nrow(Mwhite[Mwhite$quality==Mwhite$fit,])
total <- nrow(Mwhite)
positiveMwhite/total



Model_lmWhite2<- lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+total.sulfur.dioxide+pH+alcohol+sulphates, 
              data=trainWhite )
summary(Model_lmWhite)

Model_lm_white <- predict(Model_lmWhite2, testWhite, interval= c("confidence"))
Mwhite <- data.frame(testWhite,Model_lm_white)
Mwhite$fit <- round(Mwhite$fit)

positiveMwhite <- nrow(Mwhite[Mwhite$quality==Mwhite$fit,])
total <- nrow(Mwhite)
positiveMwhite/total

#matriz de correlación con quality
wine['quality'] <- lapply(wine['quality'], as.integer)

cor(y=wine$quality,x=wine$fixed.acidity,method="spearman")
cor(y=wine$quality,x=wine$volatile.acidity,method="spearman")
cor(y=wine$quality,x=wine$citric.acid,method="spearman")
cor(y=wine$quality,x=wine$residual.sugar,method="spearman")
cor(y=wine$quality,x=wine$chlorides,method="spearman")
cor(y=wine$quality,x=wine$total.sulfur.dioxide,method="spearman")
cor(y=wine$quality,x=wine$pH,method="spearman")
cor(y=wine$quality,x=wine$alcohol,method="spearman")
cor(y=wine$quality,x=wine$sulphates,method="spearman")

corr_matrix <- matrix(nc = 1, nr = 0) 
colnames(corr_matrix) <- c("estimate")
# Calcular el coeficiente de correlación para cada variable 
#cuantitativa con respecto al campo "quality"
for (i in 1:(ncol(wine) - 2)) {
  if (is.integer(wine[,i]) | is.numeric(wine[,i])) { 
    spearman_test = cor(wine[,i],
                        wine$quality,
                        method = "spearman")
    corr_coef = spearman_test
    #p_val = spearman_test$p.value
    # Add row to matrix
    pair = matrix(ncol = 1, nrow = 1)
    pair[1][1] = corr_coef
    #pair[2][1] = 1#p_val
    corr_matrix <- rbind(corr_matrix, pair) 
    rownames(corr_matrix)[nrow(corr_matrix)] <- colnames(wine)[i]
  } }

corr_matrix
###############################

wine.red.quality   <-
  wine[wine$type_wine == 0,]$quality
wine.white.quality   <-
  wine[wine$type_wine == 1,]$quality

t.test(wine.red.quality, wine.white.quality, alternative = "less")



#graficas


hist(wine$quality,freq=FALSE,col="lightsalmon",main="Histograma Quality Wine")
hist(wine_red$quality,freq=FALSE,col="lightsalmon",main="Histograma Quality Red Wine")
hist(wine_white$quality,freq=FALSE,col="lightsalmon",main="Histograma Quality White Wine")

#fichero de salida

write.csv(wine, file = "/Users/ccastillo/OneDrive - Universitat Oberta de Catalunya/Tipología y Ciclo de Vida de los Datos/Practica2/wine_out.csv")
