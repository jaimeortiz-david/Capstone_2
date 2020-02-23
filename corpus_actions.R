## plot prioritization of species by corpus

### analize the word usage in each corpus
##scientific articles


w <- "restoration"
##we use the sample state of the model
topic_docs <- topic_docs_word(m.ss, w)

doc_years <- metadata(m.ss)$pubdate %>%
        cut.Date(breaks="years")

series <- sum_col_groups(topic_docs, doc_years)

total_series <- t(doc_topics(m.ss)) %>%
        sum_col_groups(doc_years) %>%
        colSums()

series <- series %>%
        rescale_cols(1 / total_series)

series_frame <- series %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame <- semi_join(series_frame,
                          words_top_topics(m.ss, 1) %>%
                                  filter(word == w),
                          by="topic")

plot.science.term1<- series_frame[series_frame$year>="1960-01-01"&series_frame$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))


plot.science.term1$term<- rep(c("restoration"))


w <- "control"
##we use the sample state of the model
topic_docs <- topic_docs_word(m.ss, w)

doc_years <- metadata(m.ss)$pubdate %>%
        cut.Date(breaks="years")

series <- sum_col_groups(topic_docs, doc_years)

total_series <- t(doc_topics(m.ss)) %>%
        sum_col_groups(doc_years) %>%
        colSums()

series <- series %>%
        rescale_cols(1 / total_series)

series_frame <- series %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame <- semi_join(series_frame,
                          words_top_topics(m.ss, 1) %>%
                                  filter(word == w),
                          by="topic")

plot.science.term2<- series_frame[series_frame$year>="1960-01-01"&series_frame$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))


plot.science.term2$term<- rep(c("control"))


w <- "eradicate"
##we use the sample state of the model
topic_docs <- topic_docs_word(m.ss, w)

doc_years <- metadata(m.ss)$pubdate %>%
        cut.Date(breaks="years")

series <- sum_col_groups(topic_docs, doc_years)

total_series <- t(doc_topics(m.ss)) %>%
        sum_col_groups(doc_years) %>%
        colSums()

series <- series %>%
        rescale_cols(1 / total_series)

series_frame <- series %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame <- semi_join(series_frame,
                          words_top_topics(m.ss, 1) %>%
                                  filter(word == w),
                          by="topic")

plot.science.term3<- series_frame[series_frame$year>="1960-01-01"&series_frame$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))


plot.science.term3$term<- rep(c("eradicate"))


w <- "monitor"
##we use the sample state of the model
topic_docs <- topic_docs_word(m.ss, w)

doc_years <- metadata(m.ss)$pubdate %>%
        cut.Date(breaks="years")

series <- sum_col_groups(topic_docs, doc_years)

total_series <- t(doc_topics(m.ss)) %>%
        sum_col_groups(doc_years) %>%
        colSums()

series <- series %>%
        rescale_cols(1 / total_series)

series_frame <- series %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame <- semi_join(series_frame,
                          words_top_topics(m.ss, 1) %>%
                                  filter(word == w),
                          by="topic")

plot.science.term4<- series_frame[series_frame$year>="1960-01-01"&series_frame$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))


plot.science.term4$term<- rep(c("monitor"))



corpus.plot.sci.action<- rbind(plot.science.term1,plot.science.term2,plot.science.term3,plot.science.term4)
corpus.plot.sci.action$corpus <- rep("Scientific Articles")

#####Management Plans


w.grey <- "restauracion"
##we use the sample state of the model
topic_docs.grey <- topic_docs_word(m5.ss, w.grey)

doc_years.grey <- metadata(m5.ss)$pubdate %>%
        cut.Date(breaks="years")

series.grey <- sum_col_groups(topic_docs.grey, doc_years.grey)

total_series.grey <- t(doc_topics(m5.ss)) %>%
        sum_col_groups(doc_years.grey) %>%
        colSums()

series.grey <- series.grey %>%
        rescale_cols(1 / total_series.grey)

series_frame.grey <- series.grey %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.grey <- semi_join(series_frame.grey,
                               words_top_topics(m5.ss, 1) %>%
                                       filter(word == w.grey),
                               by="topic")

plot.grey.term1<- series_frame.grey[series_frame.grey$weight>0,] %>%
        mutate(topic=factor(topic_labels(m5.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)

plot.grey.term1$term<- rep(c("restoration"))


w.grey <- "control"
##we use the sample state of the model
topic_docs.grey <- topic_docs_word(m5.ss, w.grey)

doc_years.grey <- metadata(m5.ss)$pubdate %>%
        cut.Date(breaks="years")

series.grey <- sum_col_groups(topic_docs.grey, doc_years.grey)

total_series.grey <- t(doc_topics(m5.ss)) %>%
        sum_col_groups(doc_years.grey) %>%
        colSums()

series.grey <- series.grey %>%
        rescale_cols(1 / total_series.grey)

series_frame.grey <- series.grey %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.grey <- semi_join(series_frame.grey,
                               words_top_topics(m5.ss, 1) %>%
                                       filter(word == w.grey),
                               by="topic")

plot.grey.term2<- series_frame.grey[series_frame.grey$weight>0,] %>%
        mutate(topic=factor(topic_labels(m5.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)

plot.grey.term2$term<- rep(c("control"))

w.grey <- "erradicacion"
##we use the sample state of the model
topic_docs.grey <- topic_docs_word(m5.ss, w.grey)

doc_years.grey <- metadata(m5.ss)$pubdate %>%
        cut.Date(breaks="years")

series.grey <- sum_col_groups(topic_docs.grey, doc_years.grey)

total_series.grey <- t(doc_topics(m5.ss)) %>%
        sum_col_groups(doc_years.grey) %>%
        colSums()

series.grey <- series.grey %>%
        rescale_cols(1 / total_series.grey)

series_frame.grey <- series.grey %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.grey <- semi_join(series_frame.grey,
                               words_top_topics(m5.ss, 1) %>%
                                       filter(word == w.grey),
                               by="topic")

plot.grey.term3<- series_frame.grey[series_frame.grey$weight>0,] %>%
        mutate(topic=factor(topic_labels(m5.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)

plot.grey.term3$term<- rep(c("eradicate"))


w.grey <- "monitoreo"
##we use the sample state of the model
topic_docs.grey <- topic_docs_word(m5.ss, w.grey)

doc_years.grey <- metadata(m5.ss)$pubdate %>%
        cut.Date(breaks="years")

series.grey <- sum_col_groups(topic_docs.grey, doc_years.grey)

total_series.grey <- t(doc_topics(m5.ss)) %>%
        sum_col_groups(doc_years.grey) %>%
        colSums()

series.grey <- series.grey %>%
        rescale_cols(1 / total_series.grey)

series_frame.grey <- series.grey %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.grey <- semi_join(series_frame.grey,
                               words_top_topics(m5.ss, 1) %>%
                                       filter(word == w.grey),
                               by="topic")

plot.grey.term4<- series_frame.grey[series_frame.grey$weight>0,] %>%
        mutate(topic=factor(topic_labels(m5.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)



plot.grey.term4$term<- rep(c("monitor"))



corpus.plot.grey.action<- rbind(plot.grey.term1,plot.grey.term2,plot.grey.term3,plot.grey.term4)
corpus.plot.grey.action$corpus <- rep("Management Plans")

####Noticias de Galapagos

w.notgal <- "restoration"
##we use the sample state of the model
topic_docs.notgal <- topic_docs_word(m.notgal.ss, w.notgal)

doc_years.notgal <- metadata(m.notgal.ss)$pubdate %>%
        cut.Date(breaks="years")

series.notgal <- sum_col_groups(topic_docs.notgal, doc_years.notgal)

total_series.notgal <- t(doc_topics(m.notgal.ss)) %>%
        sum_col_groups(doc_years.notgal) %>%
        colSums()

series.notgal <- series.notgal %>%
        rescale_cols(1 / total_series.notgal)

series_frame.notgal <- series.notgal %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.notgal <- semi_join(series_frame.notgal,
                                 words_top_topics(m.notgal.ss, 1) %>%
                                         filter(word == w.notgal),
                                 by="topic")

plot.notgal.term1<- series_frame.notgal[series_frame.notgal$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.notgal.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)

plot.notgal.term1$term<- rep(c("restoration"))


w.notgal <- "control"
##we use the sample state of the model
topic_docs.notgal <- topic_docs_word(m.notgal.ss, w.notgal)

doc_years.notgal <- metadata(m.notgal.ss)$pubdate %>%
        cut.Date(breaks="years")

series.notgal <- sum_col_groups(topic_docs.notgal, doc_years.notgal)

total_series.notgal <- t(doc_topics(m.notgal.ss)) %>%
        sum_col_groups(doc_years.notgal) %>%
        colSums()

series.notgal <- series.notgal %>%
        rescale_cols(1 / total_series.notgal)

series_frame.notgal <- series.notgal %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.notgal <- semi_join(series_frame.notgal,
                                 words_top_topics(m.notgal.ss, 1) %>%
                                         filter(word == w.notgal),
                                 by="topic")

plot.notgal.term2<- series_frame.notgal[series_frame.notgal$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.notgal.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)

plot.notgal.term2$term<- rep(c("control"))

w.notgal <- "eradicate"
##we use the sample state of the model
topic_docs.notgal <- topic_docs_word(m.notgal.ss, w.notgal)

doc_years.notgal <- metadata(m.notgal.ss)$pubdate %>%
        cut.Date(breaks="years")

series.notgal <- sum_col_groups(topic_docs.notgal, doc_years.notgal)

total_series.notgal <- t(doc_topics(m.notgal.ss)) %>%
        sum_col_groups(doc_years.notgal) %>%
        colSums()

series.notgal <- series.notgal %>%
        rescale_cols(1 / total_series.notgal)

series_frame.notgal <- series.notgal %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.notgal <- semi_join(series_frame.notgal,
                                 words_top_topics(m.notgal.ss, 1) %>%
                                         filter(word == w.notgal),
                                 by="topic")

plot.notgal.term3<- series_frame.notgal[series_frame.notgal$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.notgal.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)

plot.notgal.term3$term<- rep(c("eradicate"))


w.notgal <- "monitor"
##we use the sample state of the model
topic_docs.notgal <- topic_docs_word(m.notgal.ss, w.notgal)

doc_years.notgal <- metadata(m.notgal.ss)$pubdate %>%
        cut.Date(breaks="years")

series.notgal <- sum_col_groups(topic_docs.notgal, doc_years.notgal)

total_series.notgal <- t(doc_topics(m.notgal.ss)) %>%
        sum_col_groups(doc_years.notgal) %>%
        colSums()

series.notgal <- series.notgal %>%
        rescale_cols(1 / total_series.notgal)

series_frame.notgal <- series.notgal %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.notgal <- semi_join(series_frame.notgal,
                                 words_top_topics(m.notgal.ss, 1) %>%
                                         filter(word == w.notgal),
                                 by="topic")

plot.notgal.term4<- series_frame.notgal[series_frame.notgal$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.notgal.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)



plot.notgal.term4$term<- rep(c("monitor"))



corpus.plotnotgal.action<- rbind(plot.notgal.term1,plot.notgal.term2,plot.notgal.term3,plot.notgal.term4)
corpus.plotnotgal.action$corpus <- rep("CDF Journal")


###Galapagos report

w.galreport <- "restoration"
##we use the sample state of the model
topic_docs.galreport <- topic_docs_word(m.galreport.ss, w.galreport)

doc_years.galreport <- metadata(m.galreport.ss)$pubdate %>%
        cut.Date(breaks="years")

series.galreport <- sum_col_groups(topic_docs.galreport, doc_years.galreport)

total_series.galreport <- t(doc_topics(m.galreport.ss)) %>%
        sum_col_groups(doc_years.galreport) %>%
        colSums()

series.galreport <- series.galreport %>%
        rescale_cols(1 / total_series.galreport)

series_frame.galreport <- series.galreport %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.galreport <- semi_join(series_frame.galreport,
                                    words_top_topics(m.galreport.ss, 1) %>%
                                            filter(word == w.galreport),
                                    by="topic")

plot.galreport.term1<- series_frame.galreport[series_frame.galreport$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.galreport.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year)) 

plot.galreport.term1$term<- rep(c("restoration"))


w.galreport <- "control"
##we use the sample state of the model
topic_docs.galreport <- topic_docs_word(m.galreport.ss, w.galreport)

doc_years.galreport <- metadata(m.galreport.ss)$pubdate %>%
        cut.Date(breaks="years")

series.galreport <- sum_col_groups(topic_docs.galreport, doc_years.galreport)

total_series.galreport <- t(doc_topics(m.galreport.ss)) %>%
        sum_col_groups(doc_years.galreport) %>%
        colSums()

series.galreport <- series.galreport %>%
        rescale_cols(1 / total_series.galreport)

series_frame.galreport <- series.galreport %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.galreport <- semi_join(series_frame.galreport,
                                    words_top_topics(m.galreport.ss, 1) %>%
                                            filter(word == w.galreport),
                                    by="topic")

plot.galreport.term2<- series_frame.galreport[series_frame.galreport$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.galreport.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)

plot.galreport.term2$term<- rep(c("control"))



w.galreport <- "eradicate"
##we use the sample state of the model
topic_docs.galreport <- topic_docs_word(m.galreport.ss, w.galreport)

doc_years.galreport <- metadata(m.galreport.ss)$pubdate %>%
        cut.Date(breaks="years")

series.galreport <- sum_col_groups(topic_docs.galreport, doc_years.galreport)

total_series.galreport <- t(doc_topics(m.galreport.ss)) %>%
        sum_col_groups(doc_years.galreport) %>%
        colSums()

series.galreport <- series.galreport %>%
        rescale_cols(1 / total_series.galreport)

series_frame.galreport <- series.galreport %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.galreport <- semi_join(series_frame.galreport,
                                    words_top_topics(m.galreport.ss, 1) %>%
                                            filter(word == w.galreport),
                                    by="topic")

plot.galreport.term3<- series_frame.galreport[series_frame.galreport$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.galreport.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)

plot.galreport.term3$term<- rep(c("eradicate"))


w.galreport <- "monitor"
##we use the sample state of the model
topic_docs.galreport <- topic_docs_word(m.galreport.ss, w.galreport)

doc_years.galreport <- metadata(m.galreport.ss)$pubdate %>%
        cut.Date(breaks="years")

series.galreport <- sum_col_groups(topic_docs.galreport, doc_years.galreport)

total_series.galreport <- t(doc_topics(m.galreport.ss)) %>%
        sum_col_groups(doc_years.galreport) %>%
        colSums()

series.galreport <- series.galreport %>%
        rescale_cols(1 / total_series.galreport)

series_frame.galreport <- series.galreport %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.galreport <- semi_join(series_frame.galreport,
                                    words_top_topics(m.galreport.ss, 1) %>%
                                            filter(word == w.galreport),
                                    by="topic")

plot.galreport.term4<- series_frame.galreport[series_frame.galreport$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.galreport.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)



plot.galreport.term4$term<- rep(c("monitor"))



corpus.plot.galreport.action<- rbind(plot.galreport.term1,plot.galreport.term2,plot.galreport.term3,plot.galreport.term4)
corpus.plot.galreport.action$corpus <- rep("Galapagos Report")




### make plots without smoothing

library(scales)
p.corpus.sci<-ggplot(corpus.plot.sci.action) + geom_line(aes(x=year, y=weight, colour=term,linetype=term)) +
        scale_colour_manual(name="Legend",values=c("black","red","green","blue"))+
        scale_linetype_manual(name="Legend",values=c(1,2,3,4))+
        geom_point(aes(x=year, y=weight, colour=term,shape=term))+
        scale_shape_manual(name="Legend",values=c(1,2,3,4)) +ylim(0,0.04)+ scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
        labs(x="year",y="fraction of corpus",title=str_c('a) Scientific Articles'))+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        theme(legend.position = c(0.15,0.7))
p.corpus.sci


p.corpus.grey<-ggplot(corpus.plot.grey.action) + geom_line(aes(x=year, y=weight, colour=term,linetype=term)) +
        scale_colour_manual(name="Legend",values=c("black","red","green","blue"))+
        scale_linetype_manual(name="Legend",values=c(1,2,3,4))+
        geom_point(aes(x=year, y=weight, colour=term,shape=term))+
        scale_shape_manual(name="Legend",values=c(1,2,3,4))+ylim(0,0.04)+scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
        labs(x="year",y="fraction of corpus",title=str_c('b) Management Plans'))+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+ theme(legend.position = "none")

p.corpus.notgal<-ggplot(corpus.plotnotgal.action) + geom_line(aes(x=year, y=weight, colour=term,linetype=term)) +
        scale_colour_manual(name="Legend",values=c("black","red","green","blue"))+
        scale_linetype_manual(name="Legend",values=c(1,2,3,4))+
        geom_point(aes(x=year, y=weight, colour=term,shape=term))+
        scale_shape_manual(name="Legend",values=c(1,2,3,4))+ylim(0,0.04)+scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
        labs(x="year",y="fraction of corpus",title=str_c('c) CDF Journal'))+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+theme(legend.position = "none")
#p.corpus.notgal

p.corpus.galreport<-ggplot(corpus.plot.galreport.action) + geom_line(aes(x=year, y=weight, colour=term,linetype=term)) +
        scale_colour_manual(name="Legend",values=c("black","red","green","blue"))+
        scale_linetype_manual(name="Legend",values=c(1,2,3,4))+
        geom_point(aes(x=year, y=weight, colour=term,shape=term))+
        scale_shape_manual(name="Legend",values=c(1,2,3,4))+ylim(0,0.04)+scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
        labs(x="year",y="fraction of corpus",title=str_c('d) Galapagos Report'))+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+theme(legend.position = "none")

#p.corpus.galreport

#library(gridExtra)

grid.arrange(p.corpus.sci,p.corpus.grey,p.corpus.notgal,p.corpus.galreport,ncol=2,widths=c(5,5))

### plot smooth



p.corpus.sci<- ggplot() +
        geom_point(data=corpus.plot.sci, aes(x=year,y= weight,group=term,color=term,shape=term),size=1) +
        labs(x="year",
             y="fraction of corpus",
             title=str_c('a)'))+
        geom_smooth(data=corpus.plot.sci,method="loess", fill="grey60", aes(x=year,y= weight,group=term,color=term),
                    se=F)+ theme(legend.position = "none")
p.corpus.sci



p.corpus.grey<- ggplot() +
        geom_point(data=corpus.plot.grey, aes(x=year,y= weight,group=term,color=term,shape=term),size=1) +
        labs(x="year",
             y="fraction of corpus",
             title=str_c('b)')) +
        geom_smooth(data=corpus.plot.grey,method="loess", fill="grey60", aes(x=year,y= weight,group=term,color=term),
                    se=F)+ theme(legend.position = "none")
p.corpus.grey


p.corpus.notgal<- ggplot() +
        geom_point(data=corpus.plotnotgal, aes(x=year,y= weight,group=term,color=term,shape=term),size=1) +
        labs(x="year",
             y="fraction of corpus",
             title=str_c('c)')) +
        
        geom_smooth(data=corpus.plotnotgal,method="loess", fill="grey60", aes(x=year,y= weight,group=term,color=term),
                    se=F)+ theme(legend.position = "none")
p.corpus.notgal


p.corpus.galreport<- ggplot() +
        geom_point(data=corpus.plot.galreport, aes(x=year,y= weight,group=term,color=term,shape=term),size=1) +
        labs(x="year",
             y="fraction of corpus",
             title=str_c('d)'))+
        geom_smooth(data=corpus.plot.galreport,method="loess", fill="grey60", aes(x=year,y= weight,group=term,color=term),
                    se=F)+ theme(legend.position = "none")

p.corpus.galreport

###Multiplot
ggplot2.multiplot(p.corpus.sci,p.corpus.grey,p.corpus.notgal,p.corpus.galreport, cols=2)


## Facet grid

plot.corpus.action.df <- rbind(corpus.plot.sci.action,corpus.plot.grey.action,corpus.plotnotgal.action,corpus.plot.galreport.action)

p <- ggplot(plot.corpus.action.df, aes(year,weight))+geom_line()
p+ facet_grid(corpus ~ term)







