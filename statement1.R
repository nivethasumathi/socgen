

data<- file.path("/home/nivetha/Music/moral.txt", "corpus", "txt")

setwd("/home/nivetha/Music/")
data <- file.path(".", "corpus", "txt")
data
length(dir(data))
library(tm)
d <- Corpus(DirSource(data))
d

my=tm_map(d,PlainTextDocument)
tdm=TermDocumentMatrix(d)
findAssocs(tdm,terms="said",corlimit=.2)
d=tm_map(d, PlainTextDocument)
class(d)

class(d[[1]])

summary(d)
inspect(d[1])
getTransformations()

d<- tm_map(d, content_transformer(tolower))
d <- tm_map(d, removeNumbers)
d <- tm_map(d, removePunctuation)
d <- tm_map(d, removeWords, stopwords("english"))
length(stopwords("english"))
stopwords("english")
d <- tm_map(d, removeWords, c("department", "exit"))
d <- tm_map(d, stripWhitespace)
d <- tm_map(d, stemDocument)
library(SnowballC)
dtm <- DocumentTermMatrix(d)
dtm

tdm=TermDocumentMatrix(d)
findFreqTerms(tdm, 100)
findAssocs(tdm,"mass",corlimit=.5)
library(wordcloud)
m <- as.matrix(tdm)
v <- sort(rowSums(m), decreasing=TRUE)
myNames <- names(v)
k <- which(names(v)=="mass")
myNames[k] <- "before_mass"
d <- data.frame(word=myNames, freq=v)
pal=brewer.pal(8,"Dark2")
wordcloud(d$word, d$freq,color=pal,min.freq=200,rot.per=.3)
length(ma)

library(cluster)
td=DocumentTermMatrix(my)
d <- dist(t(td), method="euclidian")   
fit <- hclust(d=d, method="ward")   
fit   
plot(fit,hang=-1)
inspect(d[10])
inspect(tdm)
s=inspect(d[1])
tdm=as.matrix(tdm)
m=as.character(d)
rownames(tdm)


term.freq=rowSums(as.matrix(tdm))
termfreq=subset(term.freq,term.freq>=100)
df=data.frame(term=names(term.freq),freq=term.freq)
as=subset(df,df[,2]>50)
head(term.freq)
x=as.matrix(term.freq)
colnames(x)
head(x)
x[,1]
x[1,1]
d=findFreqTerms(tdm,100)
as.matrix(x[s,])
length(d)
dtm=DocumentTermMatrix(my[1:1000])
inspect(dtm)
library(lda)
install.packages("lda")
lda=LDA(dtm,k=10)
term=terms(lda,10)
term
d[14]


library(Rgraphviz)
freq.term=findFreqTerms(tdm,140)
plot(tdm, term = freq.term, corThreshold = 0.12, weighting = T)
freq.term














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




