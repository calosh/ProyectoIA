library(caret)
library(tm)

# Training data.

data <- c('Yo apoyo a laso, vamos por el cambio.', 'Yo apoyo a lenin, vamos por mas.')

corpus <- VCorpus(VectorSource(data))

# Create a document term matrix.
tdm <- DocumentTermMatrix(corpus, list(removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))

# Convert to a data.frame for training and assign a classification (factor) to each document.
train <- as.matrix(tdm)
train <- cbind(train, c(1, 2))

colnames(train)[ncol(train)] <- 'y'
train <- as.data.frame(train)
train$y <- as.factor(train$y)

# Train.
fit <- train(y ~ ., data = train, method = 'bayesglm')
#fit <- train(y ~ ., data = train, method = "svmLinear")
warnings()


# Check accuracy on training.
p<- predict(fit, newdata = train)

confusionMatrix(p, c(1, 2))



# Test data.
data2 <- c('Voy a votar por lenin para ir por mas')
corpus <- VCorpus(VectorSource(data2))
tdm <- DocumentTermMatrix(corpus, control = list(dictionary = Terms(tdm), removePunctuation = TRUE, stopwords = TRUE, stemming = TRUE, removeNumbers = TRUE))
test <- as.matrix(tdm)

# Check accuracy on test.
predict(fit, newdata = test)
