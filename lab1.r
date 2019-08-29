require('mclust')
require('reshape2')

#path = "~/Documentos/2-2019/TMDA/Lab 1/breast-cancer-wisconsin.data
path = "~/Escritorio/USACH/Topicos/Taller de mineria de datos avanzada/tmdaLab1/breast-cancer-wisconsin.data"
data =  read.table(path,sep=",", na.strings = c("?"))

names = c('ID','Clump Thickness','Uniformity of Cell Size',
          'Uniformity of Cell Shape','Marginal Adhesion','Single Epithelial Cell Size',
          'Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses','Class')
colnames(data) = names

summary(data)


dat.m = melt(data, id=c('ID','Class'))

library(ggplot2)
p = ggplot(dat.m) + geom_boxplot(aes(x=variable, y=value)) + facet_grid(.~Class)

print(p)