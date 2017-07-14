# https://mukom.mondragon.edu/ict/mineria-de-textos-para-la-clasificacion-de-documentos-en-espanol-con-r/
install.packages("SnowballC")


library(tm) # para Text Mining
library(SnowballC) # para Stemming
library(slam) # estructuras y algoritmos para arrays y matrices
library(Matrix) # para trabajar con matrices


# Eliminar urls
sentence <- "some text http://idontwantthis.com same problem again http://pleaseremoveme.com"

removeURLS <- function(x) gsub('http[[:alun:]]*','', x)
removeURL(sentence)

