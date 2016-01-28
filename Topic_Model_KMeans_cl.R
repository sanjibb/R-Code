gsc<-read.csv("incident_dt.csv")

gscom<-gsc$Title

summary(gscom)
str(gscom)
 

#use tm for text mining purposes
#install.packages("tm")
library(tm)

#build the corpus
gsc_corpus <- Corpus(VectorSource(gscom))

#use snowballpackage for steming

#install.packages("SnowballC")
#library(SnowballC)

# Look at corpus
gsc_corpus[[1]]

gsc_corpus = tm_map(gsc_corpus, PlainTextDocument)

gsc_corpus =tm_map(gsc_corpus,removePunctuation)

gsc_corpus =tm_map(gsc_corpus,content_transformer(tolower))

gsc_corpus = tm_map(gsc_corpus, removeWords, c('ms','msft'))
          

gsc_corpus = tm_map(gsc_corpus,removeNumbers)
gsc_corpus = tm_map(gsc_corpus, removeWords, stopwords("english"))

gsc_corpus = tm_map(gsc_corpus, stemDocument)

as.character(gsc_corpus[[1]])
 

wordDTM = DocumentTermMatrix(gsc_corpus)
tdm <- TermDocumentMatrix(gsc_corpus)
#inspect frequencies

#inspect(frequencies[100:105,505:515])

# Check for sparsity

findFreqTerms(wordDTM, lowfreq=20)

# Remove sparse terms

sparse = removeSparseTerms(wordDTM, 0.995)

# Convert to a data frame

sparsedf = as.data.frame(as.matrix(sparse))

#gsccorpdf = as.data.frame((gsc_corpus))

# Make all variable names R-friendly

#colnames(wordSparse) = make.names(colnames(wordSparse))

#c1corpus <- Corpus(VectorSource(c1))

#K Means
set.seed(1000)
KMC = kmeans(sparsedf,centers=6,iter.max=1000)
KMC
str(KMC)

 
 
 

#View word Cloud and Cluster
clustercloud1 <- sparsedf[which(KMC$cluster == 1),]

#nstall.packages("wordcloud")
require(wordcloud)
dim(clustercloud1)
wordcloud(colnames(clustercloud1), colSums(clustercloud1))

#Cluster 2
c2 <- sparsedf[which(KMC$cluster == 2),]
dim(c2)
wordcloud(colnames(c2), colSums(c2))

#Cluster 3
c3 <- sparsedf[which(KMC$cluster == 3),]
dim(c3)
wordcloud(colnames(c3), colSums(c3))

#Cluster 4
c4 <- sparsedf[which(KMC$cluster == 4),]
dim(c4)
wordcloud(colnames(c4), colSums(c4))

#Cluster 5
c5 <- sparsedf[which(KMC$cluster == 5),]
dim(c5)
wordcloud(colnames(c5), colSums(c5))

#Cluster 6
c6 <- sparsedf[which(KMC$cluster == 6),]
dim(c6)
wordcloud(colnames(c6), colSums(c6))
 
c2csv$count<-as.data.frame(colSums(as.matrix(c2)))
c2col<-colSums(as.matrix(c2))
c2csv$name<-attributes(c2col)

write.csv(c2csv,"c2csv.csv")


c3csv<-as.data.frame(colSums(as.matrix(c3)))
write.csv(c3csv,"c3csv.csv")




# healthyClusters = KMC$Cluster
# KMC$centers[2]
# install.packages("useful")
# require(useful)
# plot(KMC,data=tweetsSparse)
# install.packages("cluster")
# library(cluster)
# KMC <- pam(x=trainSparse,k=4,keep.diss=TRUE,keep.data=TRUE)
# plot(KMC,which.plots=2,main="")
# set.seed(12345)
# KMCbest <-FitKMeans(tweetsSparse,max.cluster=20,nstart=25,seed=12345)
# 
# #starting another code
# dtm_tfidf = weightTfIdf(frequencies)
# 
# m <- as.matrix(dtm_tfidf)
# 
# rownames(m) <- 1:nrow(m)
# 
# #norm_eucl <- function (m)
# #  m/apply(m,1, function(x) sum(x^2)^.5))
# 
# 
# 
# 
# clusnum <- 1:5
# 
# for (i in clusnum)
# { cat("Cluster": i,":",findFrequentTerms(frequencies[KMC$Cluster==i],2),"\n\n")}
# 
# 
# 
# 
# library(caTools)
# split = sample.split(tweetsSparse, SplitRatio = 0.1)
# trainSparse = subset(tweetsSparse, split==TRUE)
# testSparse = subset(tweetsSparse, split==FALSE)
# 
# 
# 
# 
#to get nonzero sparse matrix column names 
 


