# Nubes de palabras
library(wordcloud)
positive <- subset(tweetsSparse, tweetsSparse$sentimiento==2)
positive$sentimiento <- NULL
positivas <- as.data.frame(colSums(positive))
positivas$words <- row.names(positivas)
colnames(positivas) <- c("freq","word")
wordcloud(positivas$word, positivas$freq, random.order = FALSE, colors = brewer.pal(8 , "Dark2"), max.words = 100)

