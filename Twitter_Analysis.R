# Collected Twitter data and
gsc<-read.csv("asktargetfinalv2.csv")

gscom <-gsc$text
summary(gscom)
str(gscom)
gscomuniq<-unique(gscom)

#use tm for text mining purposes
library(tm)

#build the corpus
gsc_corpus <- Corpus(VectorSource(gscom))

#use snowballpackage for steming

#install.packages("SnowballC")
library(SnowballC)

# Look at corpus
gsc_corpus[[1]]

gsc_corpus = tm_map(gsc_corpus, tolower)

gsc_corpus =tm_map(gsc_corpus,removePunctuation)
#gsc_corpus = tm_map(gsc_corpus, removeWords, 
#            c(stopwords("english"),"target","store","blackfriday","friday","shop","buy",
#              "black","deal","blackfridayd","thanksgiving"))

gsc_corpus = tm_map(gsc_corpus, removeWords, 
                    c(stopwords("english"),"asktarget"))

gsc_corpus = tm_map(gsc_corpus, stemDocument)

# Create matrix

frequencies = DocumentTermMatrix(gsc_corpus)

#inspect frequencies

inspect(frequencies[100:105,505:515])

# Check for sparsity

findFreqTerms(frequencies, lowfreq=20)

# Remove sparse terms

sparse = removeSparseTerms(frequencies, 0.995)

# Convert to a data frame

tweetsSparse = as.data.frame(as.matrix(sparse))

# Make all variable names R-friendly

colnames(tweetsSparse) = make.names(colnames(tweetsSparse))

#not working
tweet.dist <- dist(tweetsSparse, method = "euclidean")
tweet.fit <- hclust(tweet.dist, method="ward")
plot(tweet.fit, main="Cluster - Big Data")
groups <- cutree(tweet.fit, k=3)
rect.hclust(tweet.fit, k=3, border="blue")
plot(groups, main="Cluster - Big Data")
#
#K Means
set.seed(999)
KMC = kmeans(tweetsSparse,centers=5,iter.max=1000)
KMC
str(KMC)

df <- scale(tweetsSparse[-1])
wssplot(df)

#use NbClust for determining number of cluster
install.packages("NbClust")
library(NbClust)
set.seed(1234)
nc <- NbClust(tweetsSparse, min.nc=2, max.nc=15, method="kmeans")
nc <- NbClust(tweetsSparse, min.nc=2, max.nc=15, method="kmeans")


#View word Cloud and Cluster
c1 <- tweetsSparse[which(KMC$cluster == 1),]

require(wordcloud)
dim(c1)
wordcloud(colnames(c1), colSums(c1))

#Cluster 2
c2 <- tweetsSparse[which(KMC$cluster == 2),]
dim(c2)
wordcloud(colnames(c2), colSums(c2))

#Cluster 3
c3 <- tweetsSparse[which(KMC$cluster == 3),]
dim(c3)
wordcloud(colnames(c3), colSums(c3))

#Cluster 4
c4 <- tweetsSparse[which(KMC$cluster == 4),]
dim(c4)
wordcloud(colnames(c4), colSums(c4))

#Cluster 5
c5 <- tweetsSparse[which(KMC$cluster == 5),]
dim(c5)
wordcloud(colnames(c5), colSums(c5))

#Cluster 6
c6 <- tweetsSparse[which(KMC$cluster == 6),]
dim(c6)
wordcloud(colnames(c6), colSums(c6))

#Cluster 7
c7 <- tweetsSparse[which(KMC$cluster == 7),]
dim(c7)
wordcloud(colnames(c7), colSums(c7))

c8 <- tweetsSparse[which(KMC$cluster == 8),]
dim(c8)
wordcloud(colnames(c8), colSums(c8))

c9 <- tweetsSparse[which(KMC$cluster == 9),]
dim(c9)
wordcloud(colnames(c9), colSums(c9))

c10 <- tweetsSparse[which(KMC$cluster == 10),]
dim(c10)
wordcloud(colnames(c10), colSums(c10))


gsc2<-as.data.frame(gsc$text)

gsc2[rownames(c2),]
x<-as.data.frame(colSums(c2))
write.csv(x, "C:/users/z013sf1/documents/cluster2.csv")
# 
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
