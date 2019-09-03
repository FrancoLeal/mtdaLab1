require('mclust')
require('ggplot2')
require('reshape2')
require('Hmisc')
#require('corrplot')
require('fitdistrplus')

getmode <- function(x){
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x,uniqv)))]
}

#path = "~/Documentos/2-2019/TMDA/Lab 1/breast-cancer-wisconsin.data"
path = "~/Escritorio/USACH/Topicos/Taller de mineria de datos avanzada/tmdaLab1/breast-cancer-wisconsin.data"
data =  read.table(path,sep=",", na.strings = c("?"))

names = c('ID','shape','size',
          'shape','adhesion','epithelial',
          'nuclei','chromatin','nucleoli','mitoses','Class')
colnames(data) = names
data$Class[data$Class == "2"] = "Benigno"
data$Class[data$Class == "4"] = "Maligno"
summary(data)

#Se eliminan los datos con campos vacíos
data.without.na = na.omit(data)

#Se eliminan las columnas ID y la clase de cada sujeto
data.2 = data.without.na[,2:10]

#Cálculo de medidas de tendencia y variación
means <- sapply(data.2,mean)
# Medias
#       CT   UCSize  UCShape       MA     SECS       BN       BC       NN        M 
# 4.442167 3.150805 3.215227 2.830161 3.234261 3.544656 3.445095 2.869693 1.603221 
medians <- sapply(data.2,median)
# Medianas
# CT  UCSize UCShape      MA    SECS      BN      BC      NN       M 
#  4       1       1       1       2       1       3       1       1 
modes <-sapply(data.2,getmode)
# Modas
# CT  UCSize UCShape      MA    SECS      BN      BC      NN       M 
#  1       1       1       1       2       1       3       1       1 
vars <- sapply(data.2,var)
# Varianzas
#       CT    UCSize   UCShape        MA      SECS        BN        BC        NN 
# 7.956694  9.395113  8.931615  8.205717  4.942109 13.277695  6.001013  9.318772 
#        M 
# 3.002160 

show(sqrt(vars)/means)

#Coeficientes de variación
#       CT    UCSize   UCShape        MA      SECS        BN        BC        NN 
# 0.6349967 0.9728132 0.9295085 1.0121552 0.6873551 1.0279861 0.7110679 1.0637608 
#         M 
# 1.0807456

data.benigno <- data.without.na[which(data.without.na$Class == "Benigno"),]
data.maligno <- data.without.na[which(data.without.na$Class == "Maligno"),]

means.benigno <- sapply(data.benigno,mean)
medians.benigno <- sapply(data.benigno,median)
modes.benigno <- sapply(data.benigno,mode)
vars.benigno <- sapply(data.benigno,var)

means.maligno <- sapply(data.maligno,mean)
medians.maligno <- sapply(data.maligno,median)
modes.maligno <- sapply(data.maligno,mode)
vars.maligno <- sapply(data.maligno,var)


dat.m = melt(data.without.na, id=c('ID','Class'))
p = ggplot(dat.m) + geom_boxplot(aes(x=variable, y=value)) + facet_grid(.~Class)

print(p)

#Se aplica el método de agrupamiento basado en modelos
BIC=mclustBIC(data.2[,1:9], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(BIC)
summary(BIC)
#Best BIC values:
#             VVV,4       VVV,5       VVI,8
#BIC      -14753.29 -14979.3212 -15312.4484
#BIC diff      0.00   -226.0263   -559.1535

modelo = Mclust(data.2[,1:9],x=BIC)
summary(modelo)
#---------------------------------------------------- 
#  Gaussian finite mixture model fitted by EM algorithm 
#---------------------------------------------------- 
  
#  Mclust VVV (ellipsoidal, varying volume, shape, and orientation) model with 4
# components: 
  
#  Prior: defaultPrior(shrinkage = 0.1) 

# log-likelihood   n  df       BIC       ICL
# -6661.996 683 219 -14753.29 -14756.45

# Clustering table:
#  1   2   3   4 
#115 274 235  59 
plot(modelo,what = "classification")


corr = rcorr(as.matrix(data.2))
show(corr)

#           CT UCSize UCShape   MA SECS   BN   BC   NN    M
# CT      1.00   0.64    0.65 0.49 0.52 0.59 0.55 0.53 0.35
# UCSize  0.64   1.00    0.91 0.71 0.75 0.69 0.76 0.72 0.46
# UCShape 0.65   0.91    1.00 0.69 0.72 0.71 0.74 0.72 0.44
# MA      0.49   0.71    0.69 1.00 0.59 0.67 0.67 0.60 0.42
# SECS    0.52   0.75    0.72 0.59 1.00 0.59 0.62 0.63 0.48
# BN      0.59   0.69    0.71 0.67 0.59 1.00 0.68 0.58 0.34
# BC      0.55   0.76    0.74 0.67 0.62 0.68 1.00 0.67 0.35
# NN      0.53   0.72    0.72 0.60 0.63 0.58 0.67 1.00 0.43
# M       0.35   0.46    0.44 0.42 0.48 0.34 0.35 0.43 1.00

# Como es posible observar, la correlación entre UCSize y UCShape es igual a 0.91,
# lo que nos indica que estas variables entregan una información muy similar.
# Debido a esto, se procede a eliminar ese atributo y proceder con el clustering
# con los 8 atributos restantes.

data.3 = subset(data.2,select=-UCSize)

BIC2=mclustBIC(data.3[,1:8], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(BIC2)

summary(BIC)

# Best BIC values:
#              VVV,4       VVV,5       VVI,8
# BIC      -14753.29 -14979.3212 -15312.4484
# BIC diff      0.00   -226.0263   -559.1535

modelo = Mclust(data.3[,1:8],x=BIC)
summary(modelo)
# ---------------------------------------------------- 
# Gaussian finite mixture model fitted by EM algorithm 
# ---------------------------------------------------- 
  
#  Mclust VVV (ellipsoidal, varying volume, shape, and orientation) model with 4
# components: 
  
#  Prior: defaultPrior(shrinkage = 0.1) 

# log-likelihood   n  df       BIC       ICL
#       -6343.372 683 179 -13854.99 -13859.88

# Clustering table:
#  1   2   3   4 
# 94 276 244  69 
plot(modelo,what = "classification")

#m = cor(data.3)
#corrplot(m,method="number")

#coef de variacion desv est/media

