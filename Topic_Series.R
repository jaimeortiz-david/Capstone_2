##Topic series andrew goldstone package

#first we need to modify the function word_series_matrix in order to obtain trends rather than proportion of topics in years

word_series_matrix2 <- function (tdm, dates, breaks="years") {
        m_g <- sum_col_groups(tdm, cut(dates, breaks=breaks, ordered=TRUE))
        normalize_rows(m_g)
}

topic_series2 <- function (m, breaks="years") {
        tdm <- t(dt_smooth(m)(doc_topics(m)))
        m_s <- word_series_matrix2(tdm, as.Date(metadata(m)$pubdate), breaks)
        result <- gather_matrix(m_s, col_names=c("topic", "pubdate", "weight"))
        result$pubdate <- as.Date(result$pubdate)
        result
}

topic_series3 <- function (m, breaks="years") {
        tdm <- t(dt_smooth(m)(doc_topics(m)))
        m_s <- word_series_matrix(tdm, as.Date(metadata(m)$pubdate), breaks)
        result <- gather_matrix(m_s, col_names=c("topic", "pubdate", "weight"))
        result$pubdate <- as.Date(result$pubdate)
        result
}


topic.series<- topic_series2(m, breaks = "5 years")
#topic.series.df <- gather_matrix(topic.series)
#plot_series(topic.series[topic.series$topic==10&topic.series$pubdate>="1970-01-01",])
topic.series2<- topic_series2(m.grey, breaks = "years")
#plot_series(topic.series2[topic.series2$topic==2,])
topic.series3<- topic_series2(m.notgal,breaks = "5 years")
#topic.series4<- topic_series2(m.galreport, breaks = "years")



####plot both results together




p <- ggplot(topic.series[topic.series$topic==20&topic.series$pubdate>="1960-01-01",],aes(x=pubdate,y=weight)) +
        # blue plot
        geom_point(aes(colour="Scientific Articles")) + 
        geom_smooth(data=topic.series[topic.series$topic==20&topic.series$pubdate>="1960-01-01",],method="loess", fill="grey60", color="blue",
                    se=F) + scale_y_continuous(labels=scales::percent_format()) +
        labs(x="date", y="proportion of topic weight",
                      title= "Topic trend over time") +
        
        geom_point(data = topic.series2[topic.series2$topic==2&topic.series2$weight>0,],aes(colour="Management Plans")) + 
        geom_smooth(data=topic.series2[topic.series2$topic==2&topic.series2$weight>0,],method="loess", fill="grey60", color="red",
                    se=F)
        
       

p

###plot all datasets

p <- ggplot(topic.series[topic.series$topic==20&topic.series$pubdate>="1960-01-01"&topic.series$weight>0,],aes(x=pubdate,y=weight)) +
        # blue plot
        geom_point(aes(colour="Scientific Articles")) + 
        geom_smooth(data=topic.series[topic.series$topic==20&topic.series$pubdate>="1960-01-01"&topic.series$weight>0,],method="loess", fill="grey60", color="purple",
                    se=F) + scale_y_continuous(labels=waiver()) +
        labs(x="date", y="proportion of topic weight",
             title= "Topic trend over time") +
        
        geom_point(data = topic.series2[topic.series2$topic==4&topic.series2$weight>0,],aes(colour="Management Plans")) + 
        geom_smooth(data=topic.series2[topic.series2$topic==4&topic.series2$weight>0,],method="loess", fill="grey60", color="blue",
                    se=F)+

        geom_point(data = topic.series3[topic.series3$topic==10&topic.series3$weight>0,],aes(colour="CDF Journal")) + 
        geom_smooth(data=topic.series3[topic.series3$topic==10&topic.series3$weight>0,],method="loess", fill="grey60", color="red",
                    se=F)
        
        #geom_point(data = topic.series4[topic.series4$topic==1&topic.series4$weight>0,],aes(colour="Galapagos Report")) + 
                #geom_smooth(data=topic.series4[topic.series4$topic==1&topic.series4$weight>0,],method="loess", fill="grey60", color="green",
                           # se=F)



p
