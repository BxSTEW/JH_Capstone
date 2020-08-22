blogs <- (VectorSource(blogs))
news <- (VectorSource(news))
twitter <- (VectorSource(twitter))

blogs <- Corpus(blogs)
news <- Corpus(news)
twitter <- Corpus(twitter)

library(stringi)
library(ngram)
twitterwords <- wordcount(twitter, sep = ' ', count.function = sum)
blogswords <- wordcount(blogs, sep = ' ', count.function = sum)
newswords <- wordcount(news, sep = ' ', count.function = sum)

nchar_twitter<-sum(nchar(twitter))
nchar_blogs<-sum(nchar(blogs))
nchar_news<-sum(nchar(news))

text_df <- data.frame("File Name" = c("twitter", "blogs", "news"),
                      "num.lines" = c(length(twitter),length(blogs), length(news)),
                      "num.words" = c(sum(blogswords), sum(newswords), sum(twitterwords)),
                      "Num of character"=c(nchar_blogs,nchar_news,nchar_twitter))


set.seed(10000)
blogs_c<-iconv(blogs,"latin1","ASCII",sub="")
news_c<-iconv(news,"latin1","ASCII",sub="")
twitter_c<-iconv(twitter,"latin1","ASCII",sub="")

library(NLP)
sampledata<-c(sample(twitter_c,length(twitter_c)*0.01),
              sample(blogs_c,length(blogs_c)*0.01),
              sample(news_c,length(news_c)*0.01))

corpus <- VCorpus(VectorSource(sampledata))
toSpace <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, toSpace, "(f|ht)tp(s?)://(.*)[.][a-z]+")
corpus <- tm_map(corpus, toSpace, "@[^\\s]+")
corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removeWords, stopwords("en"))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stripWhitespace)
corpus <- tm_map(corpus, PlainTextDocument)


corpusresult<-data.frame(text=unlist(sapply(corpus,'[',"content")),stringsAsFactors = FALSE)
head(corpusresult)

library(RWeka)
library(wordcloud)
library(text)

unigram <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdm_unigram = TermDocumentMatrix(corpus, control = list(tokenize = unigram))

unigramcorpus<-findFreqTerms(tdm_unigram,lowfreq=80)
unicorpus_sums <- rowSums(as.matrix(tdm_unigram[unigramcorpus,]))
tdm_unigram <- data.frame(Word = names(unicorpus_sums), frequency = unicorpus_sums)
unicorpus_sort <- tdm_unigram[order(-tdm_unigram$frequency),]

ggplot(unicorpus_sort[1:12,],aes(x=reorder(Word,-frequency),y=frequency))+
        geom_bar(stat="identity",fill = I("grey50"))+
        labs(title="Uniigrams",x="Most Words",y="Frequency")+
        theme(axis.text.x=element_text(angle=60, vjust = .5))

wordcloud(tdm_unigram$Word, tdm_unigram$frequency, max.words = 15, random.order = T, colors = tdm_unigram$frequency)



Bigram <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdm_bigram = TermDocumentMatrix(corpus, control = list(tokenize = Bigram))

bigramcorpus<-findFreqTerms(tdm_bigram,lowfreq=80)
bicorpus_sums <- rowSums(as.matrix(tdm_bigram[bigramcorpus,]))
tdm_bigram <- data.frame(Word = names(bicorpus_sums), frequency = bicorpus_sums)
bicorpus_sort <- tdm_bigram[order(-tdm_bigram$frequency),]

ggplot(bicorpus_sort[1:12,],aes(x=reorder(Word,-frequency),y=frequency))+
        geom_bar(stat="identity",fill = I("grey50"))+
        labs(title="Bigrams",x="Most Words",y="Frequency")+
        theme(axis.text.x=element_text(angle=60, vjust = .5))

wordcloud(tdm_bigram$Word, tdm_bigram$frequency, max.words = 15, random.order = T)


trigram <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdm_trigram = TermDocumentMatrix(corpus, control = list(tokenize = trigram))

trigramcorpus<-findFreqTerms(tdm_trigram,lowfreq=10)
tricorpus_sums <- rowSums(as.matrix(tdm_trigram[trigramcorpus,]))
tdm_trigram <- data.frame(Word = names(tricorpus_sums), frequency = tricorpus_sums)
tricorpus_sort <- tdm_trigram[order(-tdm_trigram$frequency),]

ggplot(tricorpus_sort[1:10,],aes(x=reorder(Word,-frequency),y=frequency))+
        geom_bar(stat="identity",fill = I("grey50"))+
        labs(title="Bigrams",x="Most Words",y="Frequency")+
        theme(axis.text.x=element_text(angle=60, vjust = .5))

wordcloud(tdm_trigram$Word, tdm_trigram$frequency, max.words = 15, random.order = T)
