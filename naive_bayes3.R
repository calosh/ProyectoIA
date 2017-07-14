install.packages("tm") # Text Mining
install.packages("SnowballC")
install.packages("caTools")
install.packages("caret")
install.packages("e1071")
install.packages("wordcloud")

library(stringr)
library(tm) # text mining
library(SnowballC)  # Raiz palabras


library(caret)
library(e1071)
library(plyr)

library(caTools)



getwd()

tweets = read.csv("extraccioniaLasso2b.csv", sep = "," , encoding="UTF-8", stringsAsFactors=FALSE)
table(tweets$sentimiento)

corpus = Corpus(VectorSource(tweets$tweet))

print(corpus)

length(corpus)

content(corpus[[13]])

corpus = tm_map(corpus, tolower) # texto a minusculas

corpus = tm_map(corpus, PlainTextDocument) # 

corpus <- tm_map(corpus, removePunctuation)

content(corpus[[13]])

stopwords("es")[1:10]

corpus = tm_map(corpus, removeWords, c(stopwords("es"),"elecciones2017ec"))
corpus = tm_map(corpus, stripWhitespace)

content(corpus[[13]])


corpus = tm_map(corpus, stemDocument, language="es")
content(corpus[[13]])


frequencies = DocumentTermMatrix(corpus)

frequencies
#inspect(frequencies[50:60, 100:200])

# Terminos mas frecuentes
findFreqTerms(frequencies, lowfreq = 50)

# Eliminar palabras poco repetidas
frequencies
sparse <- removeSparseTerms(frequencies, sparse = 0.995)
sparse
findFreqTerms(sparse, lowfreq = 70)

tweetsSparse <- as.data.frame(as.matrix(sparse))

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

tweetsSparse$sentimiento <- tweets$sentimiento


set.seed(12)


split <- sample.split(tweetsSparse$sentimiento, SplitRatio = 0.8)


trainSparse = subset(tweetsSparse, split=TRUE)
testSparse = subset(tweetsSparse, split=FALSE)

table(testSparse$sentimiento)

# Evaluar el modelo


# Entrenar modelo SVM
SVM <- svm(as.factor(sentimiento)~ ., data = trainSparse)
summary(SVM)
trainSparse

predictSVM <- predict(SVM, newdata = testSparse)
testSparse
table(predictSVM)
confusionMatrix(predictSVM, testSparse$sentimiento)


# Entrenar el modelo Naive bayes
Naive <- naiveBayes(as.factor(sentimiento)~ ., data = trainSparse)
Naive
predictNaive <- predict(Naive, newdata = testSparse)
table(predictNaive)
confusionMatrix(predictNaive, testSparse$sentimiento)

#
install.packages("C50")
install.packages("rpart")
install.packages("rpart.plot")

library(C50)
library(rpart)
library(rpart.plot) 
ModeloArbol<-rpart(as.factor(sentimiento)~ ., data = trainSparse)
Prediccion <- predict(ModeloArbol, testSparse,type="class") # Prediccción en Test
table(Prediccion)
confusionMatrix(Prediccion, testSparse$sentimiento)

rpart.plot(ModeloArbol, type=1, extra=100,cex = .7,
           box.col=c("gray99", "gray88")[ModeloArbol$frame$yval])


# Cross Validation

# load the library
library(caret)
# load the iris dataset
# define training control
train_control <- trainControl(method="repeatedcv", number=4, repeats=2)
# train the model
model <- train(as.factor(sentimiento)~ ., data = trainSparse, trControl=train_control, method="nb")
# summarize results
print(model)

kmeans5<- kmeans(frequencies, 5)
kw_with_cluster <- as.data.frame(cbind('gan lenn lass', kmeans5$cluster))
names(kw_with_cluster) <- c("keyword", "kmeans5")

#Make df for each cluster result, quickly "eyeball" results
cluster1 <- subset(kw_with_cluster, subset=kmeans5 == 1)
cluster2 <- subset(kw_with_cluster, subset=kmeans5 == 2)
cluster3 <- subset(kw_with_cluster, subset=kmeans5 == 3)
cluster4 <- subset(kw_with_cluster, subset=kmeans5 == 4)
cluster5 <- subset(kw_with_cluster, subset=kmeans5 == 5)

table(cluster1)
table(cluster2)
table(cluster3)
table(cluster4)
table(cluster5)



# Nubes de palabras
library(wordcloud)
positive <- subset(tweetsSparse, tweetsSparse$sentimiento==1)
positive$sentimiento <- NULL
positivas <- as.data.frame(colSums(positive))
positivas$words <- row.names(positivas)
colnames(positivas) <- c("freq","word")
wordcloud(positivas$word, positivas$freq, random.order = FALSE, colors = brewer.pal(8 , "Dark2"), max.words = 20, res=100)







