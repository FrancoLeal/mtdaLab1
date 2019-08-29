require('mclust')

#path = "F:/Google Drive/USACH/Nivel 8/Analisis de datos/lab3/hepatitis.data"
path = "~/Documentos/2-2019/TMDA/Lab 1/breast-cancer-wisconsin.data"

data =  read.table(path,sep=",", na.strings = c("?"))

names = c('ID','Clump Thickness','Uniformity of Cell Size',
          'Uniformity of Cell Shape','Marginal Adhesion','Single Epithelial Cell Size',
          'Bare Nuclei','Bland Chromatin','Normal Nucleoli','Mitoses','Class')
colnames(data) = names
show(data)

summary(data)
