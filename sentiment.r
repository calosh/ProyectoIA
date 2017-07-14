install.packages("twitteR")
install.packages("tm")
install.packages("SnowballC")
install.packages("str")
install.packages("stringr")



library(twitteR)
library(ROAuth)
library(httr)
library(stringr)


library(tm) # text mining
library(SnowballC)  # Raiz palabras



#setup_twitter_oauth()

getwd()

tweets = read.csv("femicidios1.csv", sep = "," , encoding="UTF-8", stringsAsFactors=FALSE)

usableText=str_replace_all(tweets$Tweet.Text,"[^[:graph:]]", " " )


print(usableText)
table(tweets$Tweet.Text)




corpus = Corpus(VectorSource(tweets$))

length(corpus)

content(corpus[[13]])


corpus = tm_map(corpus, tolower) # 
corpus = tm_map(corpus, PlainTextDocument) # 


content(corpus[[13]])

corpus <- tm_map(corpus, removePunctuation)

content(corpus[[13]])

stopwords("es")[1:10]

corpus = tm_map(corpus, removeWords, c(stopwords("es"),"femicidio"))
corpus = tm_map(corpus, stripWhitespace)

content(corpus[[13]])


corpus = tm_map(corpus, stemDocument, language="es")
content(corpus[[13]])


