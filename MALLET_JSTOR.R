        ## Modelling with MALLET in order to get the topic change over time

# write one text file for each document in the dtm
n <- length(wordcounts$dimnames$Docs)

#######################

id <- wordcounts$dimnames$Docs
y<-list()
for(i in 1:n){
        tmp <- rep(wordcounts$dimnames$Terms, as.matrix(wordcounts[i,,]))
        tmp <- paste(tmp, collapse = " ")
        y[id[i]] <- tmp
        }

text.df <- do.call(rbind, y)
name.m =as.data.frame(id)
text.m.word = cbind(name.m,text.df)
text.m.word<-as.data.frame(text.m.word, stringsAsFactors=F)
colnames(text.m.word)=c("id","text")
text.m.word$id=as.character(text.m.word$id)
text.m.word$text=as.character(text.m.word$text)
# topic analysis using the mallet package
library(mallet)

mallet.instances <- mallet.import(text.m.word$id, text.m.word$text,
                                  "/Users/jaimeortiz/Dropbox/english.csv",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")

## Create a topic trainer object. 

topic.model <- MalletLDA(num.topics=50)
topic.model$loadDocuments(mallet.instances)
topic.model$setAlphaOptimization(20, 50) #optimization of model
topic.model$train(1000) #topic iterations

doc.top.fm = mallet.doc.topics(topic.model,normalized=TRUE, smoothed=TRUE)
topic.words = mallet.topic.words(topic.model, normalized=TRUE, smoothed=TRUE)
topic.labels=mallet.topic.labels(topic.model, topic.words, 3)
topic.labels

topics.frame <- as.data.frame(doc.top.fm)
names(topics.frame) <- paste("topic",sep="",1:length(topics.frame))
doc.topics<-cbind(topics.frame,id=name.m,stringsAsFactors=FALSE)
        
        # add the ids again, but on the right 
doc.topics<-cbind(topics.frame,id=name.m,stringsAsFactors=FALSE)

###################
keys.frame=list()
for (i in 1:50) {
        a<-mallet.top.words(topic.model, topic.words[i,], 5)
        a<-a$words
        keys.frame[[i]] <- a
}       


abs.l <- lapply(keys.frame, paste, collapse=" ") #convert word vector to string

topic.keywords <-do.call(rbind,abs.l)

####################

## first, get meta.frame
meta.frame <- unpack1grams$bibliodata ## read in citations.CSV
meta.frame$id <- meta.frame$x 
meta.frame$pubdate <- as.numeric(substr(meta.frame$issue, 1,4))

# merge and
# clumsily reorder to ensure that subsetting result to nth column
# will give topic n
merged <- merge(doc.topics,meta.frame,by="id")
ids <- merged$id
topic.model.df <- cbind(subset(merged,select=-id),id=ids)  
df=topic.model.df
###=======

topic.shortnames = topic.labels

## Andrew Goldstone's function (slightly modified)
# Nice R: vectorized in topics and years, gives overall proportion
# Unnice R: yrs.table, being a table, is indexable by labels, not numbers
# even though it is a table of numbers
#topic.years.proportion <- function(topic,yrs,df,
yrs=unique(df$year)                                   
yrs.table = table(df$pubdate)

s=list()
for (i in 1:50) {
        c<-sum(df[,i])
        d<-sum(yrs.table[as.character(yrs[i])])
        s[[i]] <- c/d
}    

topic.years.proportion <-do.call(rbind,s)
colnames(topic.years.proportion)=c("topic")

## Andrew Goldstone's function (slightly modified)
# The moving window for averages is 2w + 1 years
# (not exactly smoothing, since it doesn't just average averages but
# calculates the average over the whole time-interval)
# returns a two-column matrix, with the years covered in the first column
# and the averaged values in the second column

topic.proportions.by.year1 <- function(topic,df,smoothing.window=0) {
        yi <- range(df$pubdate)
        w <- smoothing.window
        
        years <- seq.int(yi[1]+w,yi[2]-w) 
        result <- matrix(nrow=length(years),ncol=2)
        result[,1] <- years
        result[,2] <- sapply(years, function (y)
                (topic.years.proportion(topic,(y-w):(y+w),df)))
        result
}

## Andrew Goldstone's function 
plot.many.topics.yearly <- function(topics,df,keys.frame,w=2) {
        n <- length(topics) * length(df$id)
        
        to.plot.list <- lapply(as.list(topics), function (i) { 
                to.add <- as.data.frame(topic.proportions.by.year1(i,df,w))
                names(to.add) <- c("year","proportion")
                # The facets will be sorted in alphabetical order
                # so, until I learn how to order them,
                # let's just do this kludge, works for n.topics < 1000
                tnum <- sprintf("%03d",i)
                to.add$topic <- paste(tnum,topic.shortnames(i,keys.frame))
                to.add$alpha <- keys.frame$alpha[i]
                to.add
        }
        )
        to.plot <- do.call(rbind,to.plot.list)
        (ggplot(to.plot, aes(year,proportion,color=alpha)) +
                geom_line() +
                facet_wrap( ~ topic , scales="free_y"))
        
        #(qplot(year,proportion,data=to.plot,facets= ~ topic, color=alpha, geom="line"))
}

### make it so
require(ggplot2)
print(plot.many.topics.yearly(topics,  topic.model.df, keys.frame))
return(topic.model.df)


}



