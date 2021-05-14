
library(tm)  
library(e1071) 
library(SnowballC) 
library(caret)
mycorpus <- Corpus(VectorSource(TX_Accidents_TMCodesadded$TMC.Description))
dtm.full <- DocumentTermMatrix(mycorpus,dtm.control) 
dtm.full <- DocumentTermMatrix(mycorpus, control=dtm.control) 


mydtm = DocumentTermMatrix(mycorpus)
inspect(mydtm)

findFreqTerms(dtm.full, lowfreq = 100)


#Based on the lab
corp <- Corpus(VectorSource(TX_Accidents_TMCodesadded$TMC.Description)) 
dtm <- DocumentTermMatrix(corp)  

inspect(dtm)


mydtm = DocumentTermMatrix(mycorpus, dtm.control)
inspect(mydtm)
findFreqTerms(dtm, lowfreq = 100)


freq <- colSums( as.matrix(dtm.full) )  
freq.sorted <- sort( freq, decreasing = TRUE )
freq.sorted[1:20] 


library(wordcloud)
Bmystop <- c("Accident","accident", "due", "or", "traffic", "ln", "ave", "accid", "blvd", "(q)")  
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T,
                   stopwords=c(stopwords("english"), Bmystop),
                   stripWhitespace=T, stemming=T)
dtm.full <- DocumentTermMatrix(mycorpus, control=dtm.control) 
wordcloud(names(freq), freq, min.feq=500)

docs <- mycorpus
docs[[1]]$content 
tdm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, 
                   stemming=T)
tdm <- TermDocumentMatrix(docs, control=tdm.control)
dim(tdm)
tdm <- removeSparseTerms(tdm,0.99)
dim(tdm)

myLSA <- lsa(tdm, dims=2) 

library(ggplot2)
doc.plot <- data.frame(x=myLSA$dk[,1], y=myLSA$dk[,2])
# d-(dentist); p-(pediatrician)
doc.plot$docnames <- substr(colnames(tdm), 1,1) # extract the doctor type
ggplot(doc.plot, aes(x,y)) + geom_point(aes(color = docnames))


mystop = c("accident(s)","(q)", "accid")
dtm.control = list(tolower=T, removePunctuation=T, removeNumbers=T, 
                   stopwords=c(Bmystop),
                   stemming=T)
newdtm <- DocumentTermMatrix(docs, control=dtm.control)
dim(newdtm)

lda.model = LDA(newdtm[1:100,], 10) 
View(lda.model)

freqterms <- terms(lda.model, 5)
View(freqterms)

myposterior <- posterior(lda.model) 

topics = myposterior$topics 
terms = myposterior$terms



