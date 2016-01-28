#install.packages("LDAvis")
library(LDAvis)
#install.packages("devtools")
library(devtools)
devtools::install_github("cpsievert/LDAvis")
help(createJSON, package = "LDAvis")
devtools::install_github("cpsievert/LDAvisData")
library(LDAvisData)

setwd("~/deep_learning")
incidentdt<-read.csv("incident_dt.csv")
incidenttl<-incidentdt$Title


#use tm for text mining purposes
#install.packages("tm")
library(tm)

#build the corpus
incidentcorp <- Corpus(VectorSource(incidenttl))


incidentcorp = tm_map(incidentcorp, PlainTextDocument)
incidentcorp = tm_map(incidentcorp,content_transformer(tolower))
incidentcorp =tm_map(incidentcorp,removePunctuation)
#incidentcorp =tm_filter(incidentcorp,FUN = function(x) any(grep("msft", content(x))))
incidentcorp = tm_map(incidentcorp,removeNumbers)
incidentcorp = tm_map(incidentcorp, removeWords, stopwords("english"))
incidentcorp = tm_map(incidentcorp, removeWords, c("msft","two"))

#incidentcorp = tm_map(incidentcorp, stemDocument)

dataframe<-data.frame(text=sapply(incidentcorp, `[[`, "content" ), stringsAsFactors=F)
dataframelist<-unlist(dataframe)

#frequencies = DocumentTermMatrix(incidentcorp)
#tdm <- TermDocumentMatrix(gsc_corpus)








#use snowballpackage for steming





doc.list<-strsplit(as.character(dataframelist),"[[:space:]]+")
a<-unlist(doc.list)
b<-a[a!='']
term.table<-table(b)
term.table<-sort(term.table,decreasing=TRUE)
vocab<-names(term.table)
#vocab<-b


get.terms<-function(x) {
  index<-match(x,vocab)
  index<-index[(!is.na(index))]
  rbind(as.integer(index-1),as.integer(rep(1,length(index))))
}

documents<-lapply(doc.list,get.terms)
D<-length(documents)
W<-length(vocab)
doc.length<-sapply(documents,function(x) sum(x[2,]))
N<-sum(doc.length)
term.frequency<-as.integer(term.table)

K<-6
G<-1000
alpha<-0.02
eta<-0.02

#install.packages("lda")
library(lda)
set.seed(1234)

fit<-lda.collapsed.gibbs.sampler(documents=documents,K=K,vocab=vocab,num.iterations=G,alpha=alpha,eta=eta,initial=NULL,burnin=0,compute.log.likelihood=TRUE)

theta<-t(apply(fit$document_sum+alpha,2,function(x) x/sum(x)))
phi<-t(apply(t(fit$topics+alpha)+eta,2,function(x) x/sum(x)))

test<-list(phi=phi,theta=theta,doc.length=doc.length,vocab=as.character(vocab),term.frequency=term.frequency)
json<-createJSON(phi=test$phi,theta=test$theta,doc.length=test$doc.length,vocab=test$vocab,term.frequency=test$term.frequency)
serVis(json,out.dir='vis',open.browser=TRUE)


#install.packages("servr")
#library(servr)






