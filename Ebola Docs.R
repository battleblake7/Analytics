rm(list=ls())
library(tm)
library(wordcloud)
library(topicmodels)

# 1. Load the documents and create the corpus
mycorpus <- Corpus(DirSource( "/Users/ShujingSun/Dropbox/JSOM_Teaching/2021_Spring/Assignment/hw4/ebola" ))

# 2. pre-processing the corpus
mystop <- c('ebola', 'replacemovietitlewithebola', 'http', 'https', '“', '”', 'amp', 'via', 'don', 'dont')
dtm <- DocumentTermMatrix(mycorpus, control=list(tolower=T, removePunctuation=T, removeNumbers=T, stripWhitespace=T, 
                          stopwords=c(mystop, stopwords("english"), stopwords("spanish"))))
# 3. remove sparse terms
# NOTE. You will not see any reduction in the dimension if you set the cutoff = 0.99 in this case. But this is totally fine as
# this suggest few sparse terms in the document. If you change the cutoff to a lower number, say 0.8, you will notice significant reduction in the number of terms
dtm = removeSparseTerms(dtm,0.8)

# 4. Plot a word cloud of the 30 most frequently used terms in the corpus
set.seed(123)
freq <- colSums( as.matrix(dtm) )  # sum of occurrences of a term across all documents
wordcloud(names(freq), freq, max.words=30, colors=brewer.pal(6, "Dark2"))

# 5. Estimate the LDA model
# NOTE. use "control=list(seed=0)" for REPRODUCIBILITY of the result from LDA model
lda.model = LDA(dtm, control=list(seed=0), k=10)
myposterior <- posterior(lda.model) # get the posterior of the model
# topic distribution of each document, one row per document, one column per topic
topics = myposterior$topics 
# term distribution of each topic, one row per topic, one column per term
terms = myposterior$terms


# 5.a Identify dominant topic for a specific document
docid <- 1  # document id 
# note. you can also directly check the distribution of topic in "topics = myposterior$topics" 
barplot(topics[docid,], xlab="topic id", ylab = "topic probability") # plot topic distribution of specific documents
dev.off() # clean the canvas

# 5.b Plot word cloud for a specific topic
tid <- 6  # topic id
freq <- terms[tid, ] # the probability of each term in a given topic
set.seed(123)
wordcloud(names(freq), freq, max.words=30, colors=brewer.pal(6,"Dark2"))


###### check the sum of probabilities across topics for each document ###### 
topics = as.data.frame(topics)
topics$sum = rowSums(topics[, c(1:10)])
summary(topics$sum)

###### check the sum of probabilities across terms/vocabulary for each topic ###### 
terms = as.data.frame(terms)
terms$sum = rowSums(terms[, c(1:ncol(terms))])
summary(terms$sum)


