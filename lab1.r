require('mclust')
require('ggplot2')
require('reshape2')
require('Hmisc')
#require('corrplot')
#require('fitdistrplus')

getmode <- function(x){
  uniqv <- unique(x)
  uniqv[which.max(tabulate(match(x,uniqv)))]
}

path = "~/Documentos/2-2019/TMDA/tmdaLab1/breast-cancer-wisconsin.data"
#path = "~/Escritorio/USACH/Topicos/Taller de mineria de datos avanzada/tmdaLab1/breast-cancer-wisconsin.data"
data =  read.table(path,sep=",", na.strings = c("?"))

names = c('ID','clump','size',
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
means
# Medias
#    shape     size  shape.1  adhesion epithelial  nuclei chromatin   nucleoli    mitoses  
# 4.442167 3.150805 3.215227 2.830161   3.234261 3.544656 3.445095   2.869693    1.603221 
medians <- sapply(data.2,median)
# Medianas
# shape     size shape.1  adhesion epithelial   nuclei  chromatin   nucleoli    mitoses 
#     4        1       1         1         2         1          3        1           1 
modes <-sapply(data.2,getmode)
# Modas
# shape    size shape.1 adhesion epithelial   nuclei  chromatin   nucleoli    mitoses 
#     1       1       1       1       2           1         3         1             1 
vars <- sapply(data.2,var)
# Varianzas
#   shape       size   shape.1  adhesion epithelial   nuclei  chromatin nucleoli    mitoses
# 7.956694  9.395113  8.931615  8.205717  4.942109 13.277695  6.001013  9.318772   3.002160 

show(sqrt(vars)/means)

#Coeficientes de variación
#     shape     size    shape.1 adhesion  epithelial   nuclei chromatin  nucleoli    mitoses
# 0.6349967 0.9728132 0.9295085 1.0121552 0.6873551 1.0279861 0.7110679 1.0637608  1.0807456

data.benigno <- data.without.na[which(data.without.na$Class == "Benigno"),]
data.maligno <- data.without.na[which(data.without.na$Class == "Maligno"),]

means.benigno <- sapply(data.benigno,mean)
#        shape         size        shape     adhesion   epithelial       nuclei    chromatin 
# 2.963964e+00 1.306306e+00 1.414414e+00 1.346847e+00 2.108108e+00 1.346847e+00 2.083333e+00 
#     nucleoli      mitoses 
# 1.261261e+00 1.065315e+00
medians.benigno <- sapply(data.benigno,median)
# shape       size      shape   adhesion epithelial     nuclei  chromatin   nucleoli    mitoses 
#     3          1          1          1          2          1          2          1          1 
modes.benigno <- sapply(data.benigno,getmode)
# shape       size      shape   adhesion epithelial     nuclei  chromatin   nucleoli    mitoses 
#   "1"        "1"        "1"        "1"        "2"        "1"        "2"        "1"        "1" 
vars.benigno <- sapply(data.benigno,var)
#        shape         size        shape     adhesion   epithelial       nuclei    chromatin 
# 2.797796e+00 7.321498e-01 9.159091e-01 8.410510e-01 7.693246e-01 1.387326e+00 1.128480e+00 
#     nucleoli      mitoses 
# 9.112724e-01 2.598326e-01

means.maligno <- sapply(data.maligno,mean)
#        shape         size        shape     adhesion   epithelial       nuclei    chromatin 
# 7.188285e+00 6.577406e+00 6.560669e+00 5.585774e+00 5.326360e+00 7.627615e+00 5.974895e+00 
#     nucleoli      mitoses 
# 5.857741e+00 2.602510e+00
medians.maligno <- sapply(data.maligno,median)
# shape       size      shape   adhesion epithelial     nuclei  chromatin   nucleoli    mitoses 
#   "8"        "6"        "6"        "5"        "5"       "10"        "7"        "6"        "1" 
modes.maligno <- sapply(data.maligno,getmode)
# shape       size      shape   adhesion epithelial     nuclei  chromatin   nucleoli    mitoses 
#  "10"       "10"       "10"       "10"        "3"       "10"        "7"       "10"        "1" 
vars.maligno <- sapply(data.maligno,var)
#       shape         size        shape     adhesion   epithelial       nuclei    chromatin 
# 5.943392e+00 7.421504e+00 6.600295e+00 1.021845e+01 5.968672e+00 9.713688e+00 5.209451e+00 
#     nucleoli      mitoses
# 1.121497e+01 6.576632e+00

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

#            shape size shape.1 adhesion epithelial nuclei chromatin nucleoli mitoses
# shape       1.00 0.64    0.65     0.49       0.52   0.59      0.55     0.53    0.35
# size        0.64 1.00    0.91     0.71       0.75   0.69      0.76     0.72    0.46
# shape.1     0.65 0.91    1.00     0.69       0.72   0.71      0.74     0.72    0.44
# adhesion    0.49 0.71    0.69     1.00       0.59   0.67      0.67     0.60    0.42
# epithelial  0.52 0.75    0.72     0.59       1.00   0.59      0.62     0.63    0.48
# nuclei      0.59 0.69    0.71     0.67       0.59   1.00      0.68     0.58    0.34
# chromatin   0.55 0.76    0.74     0.67       0.62   0.68      1.00     0.67    0.35
# nucleoli    0.53 0.72    0.72     0.60       0.63   0.58      0.67     1.00    0.43
# mitoses     0.35 0.46    0.44     0.42       0.48   0.34      0.35     0.43    1.00

# Como es posible observar, la correlación entre size y shape es igual a 0.91,
# lo que nos indica que estas variables entregan una información muy similar.
# Debido a esto, se procede a eliminar ese atributo y proceder con el clustering
# con los 8 atributos restantes.

data.3 = subset(data.2,select=-size)

BIC2=mclustBIC(data.3[,1:8], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(BIC2)

summary(BIC2)

# Best BIC values:
# VVV,5        VVI,7        VVI,9
# BIC      -13770.02 -13784.17731 -13827.61844
# BIC diff      0.00    -14.16055    -57.60168

modelo = Mclust(data.3[,1:8],x=BIC2)
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

corr = rcorr(as.matrix(data.3))
show(corr)
#m = cor(data.3)
#corrplot(m,method="number")
