#this is the analysis run using the dfrtopics package developed by agoldst
#https://github.com/agoldst/dfrtopics
#this uses the mallet package in R in comparison to the JSTORr package from Ben Marwick which uses MALLET in UNIX

devtools::install_github("agoldst/dfrtopics")
devtools::install_github("agoldst/dfrtopics", dependencies=T)

options(java.parameters="-Xmx2g")   # optional, but more memory for Java helps
library("dfrtopics")
library("dplyr")
library("ggplot2")
library("lubridate")
library("stringr")

path = "~/Dropbox/2016-3"
metadata_file <- file.path(path, "citations.tsv")
meta <- read_dfr_metadata(metadata_file)

counts <- read_wordcounts(list.files(file.path(path, "wordcounts"),
                                     full.names=T))

#discard any document less than 300 words long:
        
counts <- counts %>%group_by(id) %>% filter(sum(weight) > 300)

#replace plural words for their singular and other words with similar meaning

replace_words<-read.csv("~/Dropbox/2016//word_replace.csv",header=F)
replace_words$V1<-as.character(replace_words$V1)
replace_words$V2<-as.character(replace_words$V2)


for (a in 1:62){
        j<-c()
        k<-c()
        j<-replace_words$V1[a]
        k<-replace_words$V2[a]
        
        counts$word<- gsub(j,k,counts$word)
}



#stoplist

stoplist_file <- "~/Dropbox/stoplist_islands.txt"
stoplist <- readLines(stoplist_file)
counts %>%
        group_by(id) %>%
        summarize(total=sum(weight),
                  stopped=sum(weight[word %in% stoplist]))
#also can use
counts <- counts %>% wordcounts_remove_stopwords(stoplist)

#eliminate all but roughly the 20,000 most frequent features:OCR text includes some noise

counts <- counts %>% wordcounts_remove_rare(35000)

#eliminate features with low total frequency:
        
counts <- counts %>%group_by(word) %>% filter(sum(weight) > 3)

#create the MALLET-ready input, which is called an InstanceList, we use:
 
ilist <- wordcounts_instances(counts)

#launch the LDA algorithm with:
        
 m <- train_model(ilist, n_topics=25,
                         n_iters=1000,
                         seed=125,       # "reproducibility"
                         metadata=meta, # optional but handy later
                        alpha_sum=25,
                        beta=1,
                        n_hyper_iters = 10, #according to mallet user page www.mallet.cs.umass.edu/topics.php
                        n_burn_in=20  #same as above
                         
                  # many more parameters...
        )


#the default write_mallet_model function takes the results of train_model and outputs a directory of files.
 
write_mallet_model(m, "results_noislands_2016-3")

#derive the three “top” documents for topic 35, which we labeled topic_labels(m)[35]:

dd <- top_docs(m, n=3)        
ids <- doc_ids(m)[dd$doc[dd$topic == 35]]
metadata(m) %>%
        filter(id %in% ids) %>%
        cite_articles()


#topic over years
srs <- topic_series(m, breaks="years")

#visualization
top_words(m, n=15) %>%plot_top_words(topic=13)

#To place the topics in a two-dimensional space

topic_scaled_2d(m, n_words=2000) %>%
        plot_topic_scaled(labels=topic_labels(m, n=3))

#get plots of top words and time series of each topic
#this is my own version, which plots only the 10 top words in comparison than 20 as the original function from golds

topic_report_JO <- function (m,
                          output_dir="topic_report",
                          topics=1:n_topics(m),
                          breaks="years",
                          w=1200, h=800) {
        if (!requireNamespace("ggplot2", quietly=TRUE)) {
                stop("Plotting functions require the ggplot2 package.")
        }
        
        if (!file.exists(output_dir)) {
                dir.create(output_dir)
        }
        
        m <- load_top_words(m, 10)
        series <- topic_series(m, breaks)
        
        for (topic in topics) {
                filename <- file.path(output_dir,
                                      sprintf("%03d.png", topic))
                png(file=filename, width=w, height=h)
                
                if (getOption("dfrtopics.verbose"))
                        message("Saving ", filename)
                
                grid::grid.newpage()
                grid::pushViewport(grid::viewport(layout=grid::grid.layout(1, 2)))
                
                print(plot_top_words(top_words(m, 10), topic),
                      vp=grid::viewport(layout.pos.row=1, layout.pos.col=1))
                
                print(plot_series(series[series$topic == topic, ]),
                      vp=grid::viewport(layout.pos.row=1, layout.pos.col=2))
                
                dev.off()
        }
}




topic_report_JO(m,"plots")


##Diagnostics

d <- read_diagnostics(file.path("results_noislands_2016-3", "diagnostics.xml"))
#which.min(d$topics$corpus_dist)

b<-d$topics$tokens

c<-sum(b)

co_ocurrence<-d$topics$coherence

f<-as.data.frame(c(1:25))
co_ocurrence<-as.data.frame(co_ocurrence)
h<-cbind(f,co_ocurrence)
colnames(h)<-c("topic","score")

topic_size <- b/c
co_ocurrence<-d$topics$coherence
xmax<-max(co_ocurrence)
xmin<-min(co_ocurrence)
ymax<-max(topic_size)
ymin<-min(topic_size)
plot(co_ocurrence,topic_size,xlim=c(xmax, xmin),ylim=c(ymax,ymin))
text(co_ocurrence,topic_size, h$topic, cex=0.6, pos=4, col="red")

#axis(4,at=topic_size)


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
b[minN(b,1:5)]

?mi_check
?align_topics

dfr_browser(m)



        
