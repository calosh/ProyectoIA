
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)), 
             error = function(e) FALSE)
  })
}

areColors(c(NA, "black", "blackk", "1", "#00", "#0C5290","#D5E046"))



# Grafica 2

# Pie Chart with Percentages
slices <- c(758, 702, 361, 344) 
lbls <- c("POS Moreno", "NEG Moreno", "POS Lasso", "NEG Lasso")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
#pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Tweets POS y NEG sobre Candidatos")

pie(slices,labels = lbls, col = c("#D7E23E","#3A4C02", "#053679","#273C6A") , main="Tweets POS y NEG sobre Candidatos")


# Grafica 3

# Pie Chart with Percentages
slices <- c(1102, 1063)
lbls <- c("Moreno", "Lasso")
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels 
lbls <- paste(lbls,"%",sep="") # ad % to labels 
#pie(slices,labels = lbls, col=rainbow(length(lbls)),main="Tweets POS y NEG sobre Candidatos")

pie(slices,labels = lbls, col = c("#D7E23E", "#053679") , main="Estimación Intención voto")
