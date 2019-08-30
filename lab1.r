require('mclust')
require('ggplot2')
require('reshape2')

path = "~/Documentos/2-2019/TMDA/Lab 1/breast-cancer-wisconsin.data"
#path = "~/Escritorio/USACH/Topicos/Taller de mineria de datos avanzada/tmdaLab1/breast-cancer-wisconsin.data"
data =  read.table(path,sep=",", na.strings = c("?"))

names = c('ID','CT','UCSize',
          'UCShape','MA','SECS',
          'BN','BC','NN','M','Class')
colnames(data) = names

summary(data)

data.without.na = na.omit(data)

means <- sapply(data.without.na,mean)
medians <- sapply(data.without.na,median)
modes <- sapply(data.without.na,mode)
vars <- sapply(data.without.na,var)


dat.m = melt(data.without.na, id=c('ID','Class'))
p = ggplot(dat.m) + geom_boxplot(aes(x=variable, y=value)) + facet_grid(.~Class)

print(p)

data.2 = data.without.na[,2:10]

BIC=mclustBIC(data.2[,1:9], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(BIC)

summary(BIC)

show(vars)

data.3 = subset(data.2, select=-M)

BIC=mclustBIC(data.3[,1:8], prior = priorControl(functionName="defaultPrior", shrinkage=0.1))
plot(BIC)

summary(BIC)

modelo = Mclust(data.3[,1:8],x=BIC)
summary(modelo)
plot(modelo,what = "classification")
#coef de variacion desv est/media