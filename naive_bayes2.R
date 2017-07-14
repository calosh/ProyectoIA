install.packages("tm") # Text Mining
install.packages("SnowballC")
install.packages("caTools")
install.packages("caret")
install.packages("e1071")
install.packages("wordcloud")

library(stringr)
library(tm) # text mining
library(SnowballC)  # Raiz palabras


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

library(caTools)
set.seed(12)

split <- sample.split(tweetsSparse$sentimiento, SplitRatio = 0.8)

trainSparse = subset(tweetsSparse, split=TRUE)
testSparse = subset(tweetsSparse, split=FALSE)

table(testSparse$sentimiento)
testSparse

# Evaluar el modelo

library(caret)
library(e1071)
library(plyr)

# Entrenar modelo
SVM <- svm(as.factor(sentimiento)~ ., data = trainSparse)
summary(SVM)
trainSparse

predictSVM <- predict(SVM, newdata = testSparse)
testSparse
table(predictSVM)
confusionMatrix(predictSVM, testSparse$sentimiento)






# Prueba
tweets2 = read.csv("extraccioniaLasso2.csv", sep = "," , encoding="UTF-8", stringsAsFactors=FALSE)
table(tweets2$sentimiento)

corpus2 = Corpus(VectorSource(tweets2$tweet))

print(corpus2)

length(corpus2)

content(corpus2[[13]])

corpus2 = tm_map(corpus2, tolower) # texto a minusculas

corpus2 = tm_map(corpus2, PlainTextDocument) # 

corpus2 <- tm_map(corpus2, removePunctuation)

content(corpus2[[13]])

stopwords("es")[1:10]

corpus2 = tm_map(corpus2, removeWords, c(stopwords("es"),"elecciones2017ec"))
corpus2 = tm_map(corpus2, stripWhitespace)

content(corpus2[[13]])


corpus2 = tm_map(corpus2, stemDocument, language="es")
content(corpus2[[13]])


frequencies2 = DocumentTermMatrix(corpus2)

frequencies2
#inspect(frequencies[50:60, 100:200])

# Terminos mas frecuentes
findFreqTerms(frequencies2, lowfreq = 50)

# Eliminar palabras poco repetidas
frequencies2
sparse2 <- removeSparseTerms(frequencies2, sparse = 0.995)
sparse2
findFreqTerms(sparse2, lowfreq = 70)

tweetsSparse2 <- as.data.frame(as.matrix(sparse2))

colnames(tweetsSparse2) = make.names(colnames(tweetsSparse2))

tweetsSparse2$sentimiento <- tweets2$sentimiento

split2 <- sample.split(tweetsSparse2$sentimiento, SplitRatio = 0.9)

trainSparse2 = subset(tweetsSparse2, split2=TRUE)
testSparse2 = subset(tweetsSparse2, split=FALSE)

predictSVM2 <- predict(SVM, newdata = corpus2[[13]])





# Nubes de palabras
library(wordcloud)
positive <- subset(tweetsSparse, tweetsSparse$sentimiento==1)
positive$sentimiento <- NULL
positivas <- as.data.frame(colSums(positive))
positivas$words <- row.names(positivas)
colnames(positivas) <- c("freq","word")
wordcloud(positivas$word, positivas$freq, random.order = FALSE, colors = brewer.pal(8 , "Dark2"), max.words = 20, res=300)







