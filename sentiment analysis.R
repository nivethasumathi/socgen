
data <- file.path("/home/nivetha/Music/moral.txt", "corpus", "txt")

setwd("/home/nivetha/Music/")
data <- file.path(".", "corpus", "txt")
data
length(dir(data))

library(tm)
d <- Corpus(DirSource(data))
d

class(d)

class(d[[1]])

summary(d)
inspect(d[1])

getTransformations()

d <- tm_map(d, content_transformer(tolower))
d <- tm_map(d, removeNumbers)
d <- tm_map(d, removePunctuation)
d <- tm_map(d, removeWords, stopwords("english"))
length(stopwords("english"))
stopwords("english")
d <- tm_map(d, removeWords, c("department", "email"))
d <- tm_map(d, stripWhitespace)
d <- tm_map(d, stemDocument)
library(SnowballC)
dtm <- DocumentTermMatrix(d)
dtm

freq <- colSums(as.matrix(dtm))
length(freq)
ord <- order(freq)
freq[head(ord)]
freq[tail(ord)]
head(table(freq), 15)
tail(table(freq), 15)
m <- as.matrix(dtm)
dim(m)
write.csv(m, file="dtm.csv")
dim(dtm)
dtms <- removeSparseTerms(dtm, 0.1)
dim(dtms)
inspect(dtms)
freq <- colSums(as.matrix(dtms))
freq
table(freq)
findFreqTerms(dtm, lowfreq=1000)
findFreqTerms(dtm, lowfreq=100)
findAssocs(dtm, "data", corlimit=0.6)

##plot(dtm,
     #terms=findFreqTerms(dtm, lowfreq=100)[1:50],
     #corThreshold=0.5)

#library(Rgraphviz)
#require(Rgraphviz)
#install.packages("Rgraphviz")

freq <- sort(colSums(as.matrix(dtm)), decreasing=TRUE)
head(freq, 14)

wf <- data.frame(word=names(freq), freq=freq)
head(wf)

library(wordcloud)
set.seed(123)
wordcloud(names(freq), freq, min.freq=40)
  
set.seed(142)
wordcloud(names(freq), freq, max.words=100)

set.seed(142)
wordcloud(names(freq), freq, min.freq=100)

set.seed(142)
wordcloud(names(freq), freq, min.freq=100, colors=brewer.pal(6, "Dark2"))

set.seed(142)
wordcloud(names(freq), freq, min.freq=100, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))

