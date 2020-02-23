### analize the word usage in each topic
##scientific articles


w <- "invasive"
##we use the sample state of the model
topic_docs <- topic_docs_word(m.sci.ss, w)

doc_years <- as.Date(metadata(m.sci.ss)$pubdate) %>%
        cut.Date(breaks="years")

series <- sum_col_groups(topic_docs, doc_years)

total_series <- t(doc_topics(m.sci.ss)) %>%
        sum_col_groups(doc_years) %>%
        colSums()

series <- series %>%
        rescale_cols(1 / total_series)

series_frame <- series %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame <- semi_join(series_frame,
                          words_top_topics(m.sci.ss, 5) %>%
                                  filter(word == w),
                          by="topic")
      
       


plot.science<- series_frame[series_frame$year>="1960-01-01"&series_frame$weight>0,] %>%
        #mutate(topic=factor(topic_labels(m.sci.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))

plot.science$topic<- NULL
series_sci <- plot.science %>% group_by(year)%>%summarise(sum(weight))
summarise(group_by(plot.science,year),sum(weight))

series_sci$corpus <- rep(c("Scientific Articles"))

#plot.science$corpus<- rep(c("Scientific Articles"))



###analysis for management plans

w.grey <- "invasivas"
##we use the sample state of the model
topic_docs.grey <- topic_docs_word(m.grey.ss, w.grey)

doc_years.grey <- metadata(m.grey.ss)$pubdate %>%
        cut.Date(breaks="years")

series.grey <- sum_col_groups(topic_docs.grey, doc_years.grey)

total_series.grey <- t(doc_topics(m.grey.ss)) %>%
        sum_col_groups(doc_years.grey) %>%
        colSums()

series.grey <- series.grey %>%
        rescale_cols(1 / total_series.grey)

series_frame.grey <- series.grey %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.grey <- semi_join(series_frame.grey,
                          words_top_topics(m.grey.ss, 5) %>%
                                  filter(word == w.grey),
                          by="topic")

plot.grey<- series_frame.grey[series_frame.grey$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.grey.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)


series_grey <- plot.grey %>% group_by(year)%>%summarise(sum(weight))

series_grey$corpus <- rep(c("Management Plans"))


#plot.grey$corpus<- rep(c("Management Plans"))

###analysis for noticias de galapagos

w.notgal <- "invasive"
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
                                 words_top_topics(m.notgal.ss, 5) %>%
                                         filter(word == w.notgal),
                                 by="topic")

plot.notgal<- series_frame.notgal[series_frame.notgal$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.notgal.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)


series_notgal <- plot.notgal %>% group_by(year)%>%summarise(sum(weight))

series_notgal$corpus <- rep(c("CDF Journal"))

#plot.notgal$corpus<- rep(c("CDF Journal"))



topic.plot.invasive<- rbind(series_notgal,series_grey,series_sci)

#######


w <- "alien"
##we use the sample state of the model
topic_docs <- topic_docs_word(m.sci.ss, w)

doc_years <- as.Date(metadata(m.sci.ss)$pubdate) %>%
        cut.Date(breaks="years")

series <- sum_col_groups(topic_docs, doc_years)

total_series <- t(doc_topics(m.sci.ss)) %>%
        sum_col_groups(doc_years) %>%
        colSums()

series <- series %>%
        rescale_cols(1 / total_series)

series_frame <- series %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame <- semi_join(series_frame,
                          words_top_topics(m.sci.ss, 5) %>%
                                  filter(word == w),
                          by="topic")


plot.science<- series_frame[series_frame$year>="1960-01-01"&series_frame$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.sci.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))
series_sci <- plot.science %>% group_by(year)%>%summarise(sum(weight))

series_sci$corpus <- rep(c("Scientific Articles"))

#plot.science$corpus<- rep(c("Scientific Articles"))



###analysis for management plans

w.grey <- "alien"
##we use the sample state of the model
topic_docs.grey <- topic_docs_word(m.grey.ss, w.grey)

doc_years.grey <- metadata(m.grey.ss)$pubdate %>%
        cut.Date(breaks="years")

series.grey <- sum_col_groups(topic_docs.grey, doc_years.grey)

total_series.grey <- t(doc_topics(m.grey.ss)) %>%
        sum_col_groups(doc_years.grey) %>%
        colSums()

series.grey <- series.grey %>%
        rescale_cols(1 / total_series.grey)

series_frame.grey <- series.grey %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.grey <- semi_join(series_frame.grey,
                               words_top_topics(m.grey.ss, 5) %>%
                                       filter(word == w.grey),
                               by="topic")

plot.grey<- series_frame.grey[series_frame.grey$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.grey.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)


series_grey <- plot.grey %>% group_by(year)%>%summarise(sum(weight))

series_grey$corpus <- rep(c("Management Plans"))


#plot.grey$corpus<- rep(c("Management Plans"))

###analysis for noticias de galapagos

w.notgal <- "alien"
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
                                 words_top_topics(m.notgal.ss, 5) %>%
                                         filter(word == w.notgal),
                                 by="topic")

plot.notgal<- series_frame.notgal[series_frame.notgal$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.notgal.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)


series_notgal <- plot.notgal %>% group_by(year)%>%summarise(sum(weight))

series_notgal$corpus <- rep(c("CDF Journal"))

#plot.notgal$corpus<- rep(c("CDF Journal"))



topic.plot.alien<- rbind(series_notgal,series_grey,series_sci)

#####


w <- "exotic"
##we use the sample state of the model
topic_docs <- topic_docs_word(m.sci.ss, w)

doc_years <- as.Date(metadata(m.sci.ss)$pubdate) %>%
        cut.Date(breaks="years")

series <- sum_col_groups(topic_docs, doc_years)

total_series <- t(doc_topics(m.sci.ss)) %>%
        sum_col_groups(doc_years) %>%
        colSums()

series <- series %>%
        rescale_cols(1 / total_series)

series_frame <- series %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame <- semi_join(series_frame,
                          words_top_topics(m.sci.ss, 5) %>%
                                  filter(word == w),
                          by="topic")


plot.science<- series_frame[series_frame$year>="1960-01-01"&series_frame$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.sci.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))
series_sci <- plot.science %>% group_by(year)%>%summarise(sum(weight))

series_sci$corpus <- rep(c("Scientific Articles"))

#plot.science$corpus<- rep(c("Scientific Articles"))



###analysis for management plans

w.grey <- "exoticas"
##we use the sample state of the model
topic_docs.grey <- topic_docs_word(m.grey.ss, w.grey)

doc_years.grey <- metadata(m.grey.ss)$pubdate %>%
        cut.Date(breaks="years")

series.grey <- sum_col_groups(topic_docs.grey, doc_years.grey)

total_series.grey <- t(doc_topics(m.grey.ss)) %>%
        sum_col_groups(doc_years.grey) %>%
        colSums()

series.grey <- series.grey %>%
        rescale_cols(1 / total_series.grey)

series_frame.grey <- series.grey %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.grey <- semi_join(series_frame.grey,
                               words_top_topics(m.grey.ss, 5) %>%
                                       filter(word == w.grey),
                               by="topic")

plot.grey<- series_frame.grey[series_frame.grey$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.grey.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)


series_grey <- plot.grey %>% group_by(year)%>%summarise(sum(weight))

series_grey$corpus <- rep(c("Management Plans"))


#plot.grey$corpus<- rep(c("Management Plans"))

###analysis for noticias de galapagos

w.notgal <- "exotic"
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
                                 words_top_topics(m.notgal.ss, 5) %>%
                                         filter(word == w.notgal),
                                 by="topic")

plot.notgal<- series_frame.notgal[series_frame.notgal$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.notgal.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)


series_notgal <- plot.notgal %>% group_by(year)%>%summarise(sum(weight))

series_notgal$corpus <- rep(c("CDF Journal"))

#plot.notgal$corpus<- rep(c("CDF Journal"))



topic.plot.exotic<- rbind(series_notgal,series_grey,series_sci)

#####


w <- "introduced"
##we use the sample state of the model
topic_docs <- topic_docs_word(m.sci.ss, w)

doc_years <- as.Date(metadata(m.sci.ss)$pubdate) %>%
        cut.Date(breaks="years")

series <- sum_col_groups(topic_docs, doc_years)

total_series <- t(doc_topics(m.sci.ss)) %>%
        sum_col_groups(doc_years) %>%
        colSums()

series <- series %>%
        rescale_cols(1 / total_series)

series_frame <- series %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame <- semi_join(series_frame,
                          words_top_topics(m.sci.ss, 5) %>%
                                  filter(word == w),
                          by="topic")


plot.science<- series_frame[series_frame$year>="1960-01-01"&series_frame$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.sci.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))
series_sci <- plot.science %>% group_by(year)%>%summarise(sum(weight))

series_sci$corpus <- rep(c("Scientific Articles"))

#plot.science$corpus<- rep(c("Scientific Articles"))



###analysis for management plans

w.grey <- "introducidas"
##we use the sample state of the model
topic_docs.grey <- topic_docs_word(m.grey.ss, w.grey)

doc_years.grey <- metadata(m.grey.ss)$pubdate %>%
        cut.Date(breaks="years")

series.grey <- sum_col_groups(topic_docs.grey, doc_years.grey)

total_series.grey <- t(doc_topics(m.grey.ss)) %>%
        sum_col_groups(doc_years.grey) %>%
        colSums()

series.grey <- series.grey %>%
        rescale_cols(1 / total_series.grey)

series_frame.grey <- series.grey %>%
        gather_matrix(col_names=c("topic", "year", "weight"))

series_frame.grey <- semi_join(series_frame.grey,
                               words_top_topics(m.grey.ss, 5) %>%
                                       filter(word == w.grey),
                               by="topic")

plot.grey<- series_frame.grey[series_frame.grey$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.grey.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)


series_grey <- plot.grey %>% group_by(year)%>%summarise(sum(weight))

series_grey$corpus <- rep(c("Management Plans"))


#plot.grey$corpus<- rep(c("Management Plans"))

###analysis for noticias de galapagos

w.notgal <- "introduced"
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

plot.notgal<- series_frame.notgal[series_frame.notgal$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.notgal.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)


series_notgal <- plot.notgal %>% group_by(year)%>%summarise(sum(weight))

series_notgal$corpus <- rep(c("CDF Journal"))

#plot.notgal$corpus<- rep(c("CDF Journal"))



topic.plot.introduced<- rbind(series_notgal,series_grey,series_sci)








###analysis for galapagos report

w.galreport <- "exotic"

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

plot.galreport<- series_frame.galreport[series_frame.galreport$weight>0,] %>%
        mutate(topic=factor(topic_labels(m.galreport.ss, 3)[topic])) %>% 
        mutate(year=as.Date(year))  # restore data type (sigh)

plot.galreport$corpus<- rep(c("Galapagos Report"))



####PLOTS


#get all the datframes together

topic.plot.invasive<- rbind(plot.notgal,plot.grey,plot.science)


        
p.invasive<- ggplot() +
        geom_point(data=topic.plot.invasive, aes(x=year,y= weight,group=corpus,color=corpus,shape=corpus),size=1) +
        labs(x="year",
             y="fraction of corpus",
             title=str_c('a)'))+
        geom_smooth(data=topic.plot.invasive,method="loess", fill="grey60", aes(x=year,y= weight,group=corpus,color=corpus),
                    se=F)+ theme(legend.position = "none")
p.invasive



topic.plot.alien<- rbind(plot.galreport,plot.notgal,plot.grey,plot.science)



p.alien<- ggplot() +
        geom_point(data=topic.plot.alien, aes(x=year,y= weight,group=corpus,color=corpus,shape=corpus),size=1) +
        labs(x="year",
             y="fraction of corpus",
             title=str_c('c)'))+
        geom_smooth(data=topic.plot.alien,method="loess", fill="grey60", aes(x=year,y= weight,group=corpus,color=corpus),
                    se=F)+ theme(legend.position = "none")
p.alien


topic.plot.introduced<- rbind(plot.galreport,plot.notgal,plot.grey,plot.science)



p.introduced<- ggplot() +
        geom_point(data=topic.plot.introduced, aes(x=year,y= weight,group=corpus,color=corpus,shape=corpus),size=1) +
        labs(x="year",
             y="fraction of corpus",
             title=str_c('b)'))+
        geom_smooth(data=topic.plot.introduced,method="loess", fill="grey60", aes(x=year,y= weight,group=corpus,color=corpus),
                    se=F)+ theme(legend.position = "none")
p.introduced


topic.plot.exotic<- rbind(plot.galreport,plot.notgal,plot.grey,plot.science)



p.exotic<- ggplot() +
        geom_point(data=topic.plot.exotic, aes(x=year,y= weight,color=corpus,shape=corpus),size=1) +
        labs(x="year",
             y="fraction of corpus",
             title=str_c('d)'))+
        geom_smooth(data=topic.plot.exotic,method="loess", fill="grey60", aes(x=year,y= weight,color=corpus),
                    se=F)+ theme(legend.position = "none")
p.exotic



#######
####MULTIPLOTS#########
# http://www.sthda.com/english/wiki/ggplot2-multiplot-put-multiple-graphs-on-the-same-page-using-ggplot2


library(devtools)
install_github("easyGgplot2", "kassambara")
library(easyGgplot2)
ggplot2.multiplot(p.invasive,p.introduced,p.alien,p.exotic, cols=2)


##Plot again with same scale for all plots and without smoothing


p.invasive <- ggplot(topic.plot.invasive) + geom_line(aes(x=year, y=weight, colour=corpus,linetype=corpus)) +
                scale_colour_manual(name="Legend",values=c("black","red","dark green","blue"))+
                scale_linetype_manual(name="Legend",values=c(1,2,3,4))+
                geom_point(aes(x=year, y=weight, colour=corpus,shape=corpus))+
                scale_shape_manual(name="Legend",values=c(1,2,3,4)) +ylim(0,0.04)+ scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
                labs(x="year",y="fraction of corpus",title=str_c('a) Invasive'))+
                theme_bw() +
                theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())+
                theme(legend.position = c(0.15,0.7))


p.introduced <- ggplot(topic.plot.introduced) + geom_line(aes(x=year, y=weight, colour=corpus,linetype=corpus)) +
        scale_colour_manual(name="Legend",values=c("black","red","dark green","blue"))+
        scale_linetype_manual(name="Legend",values=c(1,2,3,4))+
        geom_point(aes(x=year, y=weight, colour=corpus,shape=corpus))+
        scale_shape_manual(name="Legend",values=c(1,2,3,4)) +ylim(0,0.04)+ scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
        labs(x="year",y="fraction of corpus",title=str_c('b) Introduced'))+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        theme(legend.position = "none")


p.alien <- ggplot(topic.plot.alien) + geom_line(aes(x=year, y=weight, colour=corpus,linetype=corpus)) +
        scale_colour_manual(name="Legend",values=c("black","red","blue"))+
        scale_linetype_manual(name="Legend",values=c(1,2,4))+
        geom_point(aes(x=year, y=weight, colour=corpus,shape=corpus))+
        scale_shape_manual(name="Legend",values=c(1,2,4)) +ylim(0,0.04)+ scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
        labs(x="year",y="fraction of corpus",title=str_c('c) Alien'))+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        theme(legend.position="none")


p.exotic <- ggplot(topic.plot.exotic) + geom_line(aes(x=year, y=weight, colour=corpus,linetype=corpus)) +
        scale_colour_manual(name="Legend",values=c("black","red","dark green","blue"))+
        scale_linetype_manual(name="Legend",values=c(1,2,3,4))+
        geom_point(aes(x=year, y=weight, colour=corpus,shape=corpus))+
        scale_shape_manual(name="Legend",values=c(1,2,3,4)) +ylim(0,0.04)+ scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
        labs(x="year",y="fraction of corpus",title=str_c('d) Exotic'))+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+
        theme(legend.position = "none")

grid.arrange(p.invasive,p.introduced,p.alien,p.exotic,ncol=2,widths=c(5,5))



### plot word for all corpus


topic.plot.exotic$class<-rep("exotic")
topic.plot.alien$class<-rep("alien")
topic.plot.introduced$class<-rep("introduced")
topic.plot.invasive$class<-rep("invasive")

corpus.plot.sci<- rbind(topic.plot.alien[topic.plot.alien$corpus=="Scientific Articles",],topic.plot.exotic[topic.plot.exotic$corpus=="Scientific Articles",],topic.plot.introduced[topic.plot.introduced$corpus=="Scientific Articles",],topic.plot.invasive[topic.plot.invasive$corpus=="Scientific Articles",])

plot.corpus.sci<- ggplot() +
        geom_point(data=corpus.plot.sci, aes(x=year,y= weight,color=class,shape=class),size=1) + ylim(0,0.04)+
        labs(x="year",
             y="fraction of corpus",
             title=str_c('a)'))+
        geom_smooth(data=corpus.plot.sci,method="loess", fill="grey60", aes(x=year,y= weight,color=class),
                    se=F)+ theme(legend.position = "right")
plot.corpus.sci

##

corpus.plot.grey<- rbind(topic.plot.alien[topic.plot.alien$corpus=="Management Plans",],topic.plot.exotic[topic.plot.exotic$corpus=="Management Plans",],topic.plot.introduced[topic.plot.introduced$corpus=="Management Plans",],topic.plot.invasive[topic.plot.invasive$corpus=="Management Plans",])

plot.corpus.grey<- ggplot() +
        geom_point(data=corpus.plot.grey, aes(x=year,y= weight,color=class,shape=class),size=1) + ylim(0,0.04)+
        labs(x="year",
             y="fraction of corpus",
             title=str_c('b)'))+
        geom_smooth(data=corpus.plot.grey,method="loess", fill="grey60", aes(x=year,y= weight,color=class),
                    se=F)+ theme(legend.position = "right")
plot.corpus.grey

##

corpus.plot.notgal<- rbind(topic.plot.alien[topic.plot.alien$corpus=="CDF Journal",],topic.plot.exotic[topic.plot.exotic$corpus=="CDF Journal",],topic.plot.introduced[topic.plot.introduced$corpus=="CDF Journal",],topic.plot.invasive[topic.plot.invasive$corpus=="CDF Journal",])

plot.corpus.notgal<- ggplot() +
        geom_point(data=corpus.plot.notgal, aes(x=year,y= weight,color=class,shape=class),size=1) + ylim(0,0.04)+
        labs(x="year",
             y="fraction of corpus",
             title=str_c('c)'))+
        geom_smooth(data=corpus.plot.notgal,method="loess", fill="grey60", aes(x=year,y= weight,color=class),
                    se=F)+ theme(legend.position = "right")
plot.corpus.notgal

##

corpus.plot.galreport<- rbind(topic.plot.alien[topic.plot.alien$corpus=="Galapagos Report",],topic.plot.exotic[topic.plot.exotic$corpus=="Galapagos Report",],topic.plot.introduced[topic.plot.introduced$corpus=="Galapagos Report",],topic.plot.invasive[topic.plot.invasive$corpus=="Galapagos Report",])

plot.corpus.galreport<- ggplot() +
        geom_point(data=corpus.plot.galreport, aes(x=year,y= weight,color=class,shape=class),size=1) + ylim(0,0.04)+
        labs(x="year",
             y="fraction of corpus",
             title=str_c('d)'))+
        geom_smooth(data=corpus.plot.galreport,method="loess", fill="grey60", aes(x=year,y= weight,color=class),
                    se=F)+ theme(legend.position = "right")
plot.corpus.galreport



ggplot2.multiplot(plot.corpus.sci,plot.corpus.grey,plot.corpus.notgal,plot.corpus.galreport, cols=2)


### make plots without smoothing 

library(scales)


plot.corpus.sci<-ggplot(corpus.plot.sci) + geom_line(aes(x=mean, y=weight, colour=class,linetype=class)) +
                scale_colour_manual(name="Legend",values=c("black","red","green","blue"))+
                scale_linetype_manual(name="Legend",values=c(1,2,3,4))+
                geom_point(aes(x=mean, y=weight, colour=class,shape=class))+
                scale_shape_manual(name="Legend",values=c(1,2,3,4)) +ylim(0,0.04)+ scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
                labs(x="year",y="fraction of corpus",title=str_c('a)'))+
                theme_bw() +
                theme(axis.line = element_line(colour = "black"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank())+
                theme(legend.position = c(0.15,0.7))
plot.corpus.sci


plot.corpus.grey<-ggplot(corpus.plot.grey) + geom_line(aes(x=mean, y=weight, colour=class,linetype=class)) +
                        scale_colour_manual(name="Legend",values=c("red","green","blue"))+
                        scale_linetype_manual(name="Legend",values=c(2,3,4))+
                        geom_point(aes(x=mean, y=weight, colour=class,shape=class))+
                        scale_shape_manual(name="Legend",values=c(2,3,4))+ylim(0,0.04)+scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
                        labs(x="year",y="fraction of corpus",title=str_c('b)'))+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+ theme(legend.position = "none")
        
plot.corpus.notgal<-ggplot(corpus.plot.notgal) + geom_line(aes(x=mean, y=weight, colour=class,linetype=class)) +
        scale_colour_manual(name="Legend",values=c("black","red","green","blue"))+
        scale_linetype_manual(name="Legend",values=c(1,2,3,4))+
        geom_point(aes(x=year, y=weight, colour=class,shape=class))+
        scale_shape_manual(name="Legend",values=c(1,2,3,4))+ylim(0,0.04)+scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
        labs(x="year",y="fraction of corpus",title=str_c('c)'))+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+theme(legend.position = "none")

plot.corpus.galreport<-ggplot(corpus.plot.galreport) + geom_line(aes(x=year, y=weight, colour=class,linetype=class)) +
        scale_colour_manual(name="Legend",values=c("black","red","green","blue"))+
        scale_linetype_manual(name="Legend",values=c(1,2,3,4))+
        geom_point(aes(x=year, y=weight, colour=class,shape=class))+
        scale_shape_manual(name="Legend",values=c(1,2,3,4))+ylim(0,0.04)+scale_x_date(limits=c(as.Date("1960/1/1"),as.Date("2016/1/1")),breaks=seq(from=as.Date("1960/1/1"),to=as.Date("2016/1/1"),by="5 years"),date_labels = "%Y")+
        labs(x="year",y="fraction of corpus",title=str_c('d)'))+
        theme_bw() +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank())+theme(legend.position = "none")

plot.corpus.galreport

library(gridExtra)

grid.arrange(plot.corpus.sci,plot.corpus.grey,plot.corpus.notgal,ncol=2,widths=c(5,5))

#### facet graph 5 years average

language.plot.df <- rbind(corpus.plot.sci,corpus.plot.grey,corpus.plot.notgal)
language.plot.df$year <- as.Date(as.character(language.plot.df$year),"%Y")

language.plot.df$mean <- as.Date(cut(language.plot.df$year,
                                     breaks = "5 years"))

colnames(language.plot.df)<-c("year","weight","corpus","class","mean")

p <- ggplot(language.plot.df, aes(mean,weight))+stat_summary(fun.y=mean,geom="line")+labs(x="year")
p+ facet_grid(class ~ corpus)



plot.corpus.df <- rbind(topic.plot.alien,topic.plot.exotic,topic.plot.introduced,topic.plot.invasive)
colnames(plot.corpus.df)<-c("year","weight","corpus","class")
p <- ggplot(plot.corpus.df, aes(year,weight))+geom_line()
p+ facet_grid(corpus ~ class)

########
