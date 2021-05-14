install.packages("ggplot2")
install.packages("tm")  # for text mining
install.packages("SnowballC") # for text stemming
install.packages("wordcloud") # word-cloud generator 
install.packages("RColorBrewer") # color palettes
library("ggplot2")
library("tm")
library("SnowballC")
library("wordcloud")
library("RColorBrewer")

yt <- read.csv("data/ytossz2.csv",TRUE,";")
class(yt)
View(yt)

yt$view_count <- as.factor(yt$view_count)
yt$likes <- as.factor(yt$likes)
yt$dislikes <- as.factor(yt$dislikes)
yt$comment_count <- as.factor(yt$comment_count)

#Kommentelés tiltások aránya

ggplot(yt, aes(x = comments_disabled, fill = categoryName)) + 
  theme_minimal() +
  geom_bar()

#Értékelés tiltások aránya

ggplot(yt, aes(x = ratings_disabled, fill = categoryName)) + 
  theme_minimal() +
  geom_bar()


#Bevétel diagram

ggplot(yt,aes(categoryName,earnings)) + 
  geom_bar(stat = "identity", na.rm=TRUE)

memory.limit(9999999999)

#tag-ek beolvasása

text <- readLines("data/tags.txt")
docs <- Corpus(VectorSource(text))

#Szöveg tisztítás

toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
docs <- tm_map(docs, toSpace, "/")
docs <- tm_map(docs, toSpace, "@")
docs <- tm_map(docs, toSpace, "the")
docs <- tm_map(docs, toSpace, "and")
docs <- tm_map(docs, toSpace, "among")
docs <- tm_map(docs, toSpace, "\\|")
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, removeWords, stopwords("english"))

#Legsûrûbben használt Tag-ek - memória igényesebb

dtm <- TermDocumentMatrix(docs)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data.frame(word = names(v),freq=v)
head(d, 100)

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 1,
          max.words=2000, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))
aggregate(yt$estimatedValueOfEarnings..USD., by=list(Category=yt$categoryName), FUN=sum)
