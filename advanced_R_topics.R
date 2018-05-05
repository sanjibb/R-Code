data("diamonds", package = "ggplot2")
head(diamonds)

#agreegate function
aggregate(diamonds$price,by=list(diamonds$cut),mean)
aggregate(price ~ cut,diamonds,mean)
#tapply
tapply(diamonds$price, INDEX=diamonds$cut,mean) #returns a vector

#sapply

require("plyr")
##ddply data frame to data frame
##llply list to list
#dlply dataframe to list
#adply array to dataframe

ddply(diamonds,"cut",summarize,cutprice=mean(price))
daply(diamonds,"cut",summarize,cutprice=mean(price))

require(doParallel)

remove.packages("data.table")
install.packages("data.table")
require(data.table)
diamondtable <- data.table(diamonds)
diamondtable[, mean(price), by=cut]


require(plyr)
require(dplyr)

diamonds %>% head  #pipe
diamonds %>% dim 

diamonds %>% group_by(cut) %>% dplyr::summarize(Price=mean(price))


download.file("http://www.jaredlander.com/data/diamonds.db",destfile="diamonds.db",method="curl")

require(dplyr)
install.packages("RSQLite")
diaDBSource  <- src_sqlite("diamonds.db")
dia <- tbl(diaDBSource,"diamonds") #diamonds is the name of the table

install.packages("ggvis")
require(ggvis)

data("cocaine")
head(cocaine)
require(ggplot2)

ggplot(cocaine,aes(x=weight,y=price)) + geom_point()

cocaine %>% ggvis(x=~weight, y=~price, fill= ~potency) %>% layer_points()











  