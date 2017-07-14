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

content(corpus[[12]])
content(corpus[[1500]])


corpus = tm_map(corpus, tolower) # texto a minusculas
corpus = tm_map(corpus, PlainTextDocument) # 
corpus <- tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, c(stopwords("es"),"elecciones2017ec"))
corpus = tm_map(corpus, stripWhitespace)
corpus = tm_map(corpus, stemDocument, language="es")

content(corpus[[12]])
content(corpus[[1500]])

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


# Entrenar modelo
SVM <- svm(as.factor(sentimiento)~ ., data = trainSparse)
summary(SVM)
trainSparse

predictSVM <- predict(SVM, newdata = testSparse)

table(predictSVM)
confusionMatrix(predictSVM, testSparse$sentimiento)



# Nubes de palabras
library(wordcloud)
positive <- subset(tweetsSparse, tweetsSparse$sentimiento==1)
positive$sentimiento <- NULL
positivas <- as.data.frame(colSums(positive))
positivas$words <- row.names(positivas)
colnames(positivas) <- c("freq","word")
wordcloud(positivas$word, positivas$freq, random.order = FALSE, colors = brewer.pal(8 , "Dark2"), max.words = 50, res=100)







