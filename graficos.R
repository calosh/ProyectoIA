# library
library(ggplot2)

#Grafica 1
# create a dataset
specie=c(rep("Moreno" , 2) , rep("Lasso" , 2))
condition=rep(c("positivo" , "negativo" ) , 2)
#value=abs(rnorm(4 , 0 , 15))
value=c(758,702,361,344)
data=data.frame(specie,condition,value)

# Grouped
ggplot(data, aes(fill=condition, y=value, x=specie)) + 
  geom_bar(position="dodge", stat="identity")+
  scale_fill_manual(values = c("#BDBDBD","#81F7F3") )




