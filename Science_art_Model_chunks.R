### I will need to cut each scientific article in chunks of 1000 words to corresponde with corpus size of the other datasets

library("dfrtopics")
library("dplyr")
library("ggplot2")
library("lubridate")
library("stringr")

### First read the wordcounts from dfr

path = "~/Dropbox/2016-3"
metadata_file <- file.path(path, "citations.tsv")
meta <- read_dfr_metadata(metadata_file)
gal_art_only<-read.csv("~/Dropbox/2016-3//gal_articles_only.csv",header=T)

id_list<-as.character(gal_art_only$id)

doc_name <- gsub("/","_",id_list)
doc_name <- paste("wordcounts",doc_name,sep="_")
doc_name <- paste(doc_name,"CSV",sep=".")

myData <-  lapply(doc_name, read.csv)

names(myData) <- doc_name

myUntabledData <- sapply(1:length(myData),
                         function(x) {rep(myData[[x]]$WORDCOUNTS, times = myData[[x]]$WEIGHT)})


names(myUntabledData) <- doc_name
sapply(doc_name,
       function (x) write.table(myUntabledData[x], file=paste(x, "txt", sep="."),
                                quote = FALSE, row.names = FALSE,col.names=F, eol = " " ))


# Look in the working directory to find the txt files


files.v <- dir("~/Dropbox/2016-3/text_files", pattern=".*txt")
chunk.size <- 1000 # number of words per chunk
## code from Mathew Jockers
make.file.word.v.l <- function(files.v, input.dir,chunk.size=1000, percentage=FALSE){
        #set up an empty container
        text.word.vector.l <- list()
        topic.m <- NULL
        
        for(i in 1:length(files.v)){
                # read the file in (notice that it is here that we need to know the input
                # directory
                text.v <- scan(paste(input.dir, files.v[i], sep="/"),
                               what="character", sep="\n")
                #convert to single string
                text.v <- paste(text.v, collapse=" ")
                #lowercase and split on non-word characters
                text.lower.v <- tolower(text.v)
                text.words.v <- strsplit(text.lower.v, "\\W")
                
                text.words.v <- unlist(text.words.v)
                #remove the blanks
                text.words.v <- text.words.v[which(text.words.v!="")]
                x <- seq_along(text.words.v)
                if(percentage){
                        max.length <- length(text.words.v)/chunk.size
                        chunks.l <- split(text.words.v, ceiling(x/max.length))
                } else {
                        chunks.l <- split(text.words.v, ceiling(x/chunk.size))
                        #deal with small chunks at the end
                        if(length(chunks.l[[length(chunks.l)]]) <=
                           length(chunks.l[[length(chunks.l)]])/2){
                                chunks.l[[length(chunks.l)-1]] <-
                                        c(chunks.l[[length(chunks.l)-1]],
                                          chunks.l[[length(chunks.l)]])
                                chunks.l[[length(chunks.l)]] <- NULL
                        }
                }
                
                chunks.l <- lapply(chunks.l, paste, collapse=" ")
                chunks.df <- do.call(rbind, chunks.l)
                
                textname <- (meta$id[i])
                date <- as.character(meta$pubdate[i])
                segments.m <- cbind(paste(textname,
                                          segment=1:nrow(chunks.df), sep="_"), chunks.df,date)
              
                topic.m <- rbind(topic.m, segments.m)
                
        }
        return(topic.m)
}


## read new text files

path = "~/Dropbox/2016-3/text_files"
my.corpus <- make.file.word.v.l(files.v, input.dir=path,chunk.size=1000)  

documents.sci <- as.data.frame(my.corpus, stringsAsFactors=F)
colnames(documents.sci) <- c("id", "text","date")

###function from: https://gist.github.com/atbradley/5902375
wordcount <- function(str) {
        sapply(gregexpr("\\b\\W+\\b", str, perl=TRUE), function(x) sum(x>0) ) + 1 
} 

doc.length.sci<-wordcount(documents.sci$text)


library(lubridate)
meta<-NULL

meta<- cbind(documents.sci$id,documents.sci$date)
meta<- as.data.frame(meta)
colnames(meta)<- c("id","pubdate")
meta$author<- meta$id
meta$journaltitle<-meta$id
meta$volume<-documents.sci$id
meta$issue<-meta$volume
meta$title <- meta$id

meta$pagerange<-meta$volume


#replace plural words for their singular and other words with similar meaning


replace_words<-read.csv("~/Dropbox/2016//word_replace.csv",header=F)
replace_words$V1<-as.character(replace_words$V1)
replace_words$V2<-as.character(replace_words$V2)


for (a in 1:length(replace_words$V1)){
        j<-c()
        k<-c()
        j<-replace_words$V1[a]
        k<-replace_words$V2[a]
        
        documents.sci$text<- gsub(paste0('\\<',j,'\\>'),k,documents.sci$text)
}




#run mallet model

## clear space

rm(my.corpus)
rm(a)
rm(myUntabledData)
rm(myData)

options(java.parameters="-Xmx4g")
library(mallet)

mallet.instances.sci <- mallet.import(documents.sci$id, documents.sci$text,
                                  "~/Dropbox//stop_list5.txt",
                                  FALSE,
                                  token.regexp="[\\p{L}']+")





##use dfrtopics package


options(java.parameters="-Xmx4g")   # optional, but more memory for Java helps
library("dfrtopics")
library("dplyr")
library("ggplot2")
library("lubridate")
library("stringr")


m <- train_model(mallet.instances.sci, n_topics=30,
                      n_iters=1000,
                      seed=125,       # "reproducibility"
                      metadata=meta, # optional but handy later
                      #alpha_sum=,
                      beta=0.1,
                      n_hyper_iters = 10, #according to mallet user page www.mallet.cs.umass.edu/topics.php
                      n_burn_in=20 #same as above
                      
                      # many more parameters...
)


m2 <- train_model(mallet.instances.sci, n_topics=15,
                 n_iters=1000,
                 seed=125,       # "reproducibility"
                 metadata=meta, # optional but handy later
                 #alpha_sum=,
                 beta=0.01,
                 n_hyper_iters = 10, #according to mallet user page www.mallet.cs.umass.edu/topics.php
                 n_burn_in=20 #same as above
                 
                 # many more parameters...
)


m3 <- train_model(mallet.instances.sci, n_topics=10,
                  n_iters=1000,
                  seed=125,       # "reproducibility"
                  metadata=meta, # optional but handy later
                  #alpha_sum=,
                  beta=0.01,
                  n_hyper_iters = 10, #according to mallet user page www.mallet.cs.umass.edu/topics.php
                  n_burn_in=20 #same as above
                  
                  # many more parameters...
)



m4 <- train_model(mallet.instances.sci, n_topics=12,
                  n_iters=1000,
                  seed=125,       # "reproducibility"
                  metadata=meta, # optional but handy later
                  #alpha_sum=,
                  beta=0.1,
                  n_hyper_iters = 10, #according to mallet user page www.mallet.cs.umass.edu/topics.php
                  n_burn_in=20 #same as above
                  
                  # many more parameters...
)


m5 <- train_model(mallet.instances.sci, n_topics=25,
                  n_iters=1000,
                  seed=125,       # "reproducibility"
                  metadata=meta, # optional but handy later
                  #alpha_sum=,
                  beta=0.1,
                  n_hyper_iters = 10, #according to mallet user page www.mallet.cs.umass.edu/topics.php
                  n_burn_in=20 #same as above
                  
                  # many more parameters...
)

#the default write_mallet_model function takes the results of train_model and outputs a directory of files.

write_mallet_model(m5, "modeling_results_science_chunk_T6")

d <- read_diagnostics(file.path("modeling_results_science_chunk_T6", "diagnostics.xml"))
#which.min(d$topics$corpus_dist)

b<-d$topics$tokens

c<-sum(b)

co_ocurrence<-d$topics$coherence

f<-as.data.frame(c(1:6))
co_ocurrence<-as.data.frame(co_ocurrence)
h<-cbind(f,co_ocurrence)
colnames(h)<-c("topic","score")

topic_size <- b/c
co_ocurrence<-d$topics$coherence
xmax<-max(co_ocurrence)
xmin<-min(co_ocurrence)
ymax<-max(topic_size)
ymin<-min(topic_size)
plot(co_ocurrence,topic_size,xlim=c(xmax, xmin),ylim=c(ymax,ymin),main="Topic Quality in Management Plans")
text(co_ocurrence,topic_size, h$topic, cex=0.6, pos=4, col="red")


minN <- function(x, N){
        
        a<-c()
        j<-sort(d$topics$corpus_dist)[N]
        for(i in 1:length(N)){
                c<-which(d$topics$corpus_dist==j[i])    
                a[i]<-c       
        }
        a 
}

minN(b,1:5)


dfr_browser(m)


##good results with T=5 and using stoplist 5


#Given information about the dissimilarities among topics across a set of models, this function attempts to identify groups of similar topics from each model.
?align_topics

dists.sci <- model_distances(list(m, m2, m3,m4,m5,m.notgal), n_words=10)
clusters.sci <- align_topics(dists.sci, threshold=0.5)
# data frame readout
af.sci<-alignment_frame(clusters.sci)
