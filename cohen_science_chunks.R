###apply Cohen Vita equation to management plan

## formula for time analysis from Cohen Vita's paper

#get the id and wordcounts of documents



## get bibliodata
d.sci<-metadata(m)
d.sci$year <- as.character(str_extract(documents.sci$date, "[[:digit:]]{4}"))

###term document matrix per topic
topic.sci <- 4 ##choose topic for introduced species

m.sci.ss<-load_sampling_state(m)
tdm.sci <- tdm_topic(m.sci.ss,topic.sci) 
tdm.sci.df<- gather_matrix(tdm.sci)

tdm.sci.df<-tdm.sci.df[tdm.sci.df$value!=0,] #get rid off words with weight equal to 0



###I will do a for loop since I have not been able to divide each element of the sparse matrix tdm.sc

dl.sci<-list()

for (i in 1:length(tdm.sci.df$value)){
        
        a<-doc.length.sci[tdm.sci.df$col_key[i]]
        j<-tdm.sci.df$row_key[i]
        k<-tdm.sci.df$col_key[i]
        
        dl.sci[i]<- tdm.sci.df$value[tdm.sci.df$row_key==j&tdm.sci.df$col_key==k]/a  #get the weight of each word in document d divided by the length of document d    
}   

dl.sci.df<-do.call(rbind.data.frame,dl.sci)
dl.sci.df<-cbind(dl.sci.df,tdm.sci.df$col_key)
colnames(dl.sci.df)<-c("weight","document")

dl.sci.df$id<-(doc_ids(m.sci.ss)[dl.sci.df$document])
dl.sci.df$pubdate<-cut.Date(as.Date(metadata(m.sci.ss)$pubdate[dl.sci.df$document]),breaks="years")
dl.sci.df$year<- str_extract(dl.sci.df$pubdate, "[[:digit:]]{4}")
dl.sci.df<-as.data.frame(dl.sci.df)
colnames(dl.sci.df)<-c("weight","document","id","pubdate","year")


#####

##get years of publications for topic 
doc_years<-unique(dl.sci.df$year)

###Aqui esta la falla tengo que añadir otra columna solo con los años sin usar Date format


yrs.table.sci = table(d.sci$year)

yrs.table.10 = table(dl.sci.df$year[unique(dl.sci.df$document)])

###sum and divide for total of documents per year


s.sc.sci=list()

for (i in 1:length(doc_years)) {
        w<-doc_years[i]
        
        
        t<- dl.sci.df$weight[dl.sci.df$year %in% w]
        s.sc.sci[[i]] <-  sum(t) / 
                yrs.table.sci[i]
        
        
}    

years.prop.sci <- do.call(rbind.data.frame,s.sc.sci)
colnames(years.prop.sci)<-c("weight")

#years.prop.grey$document <- unique(dl.grey.df$document)## I need to solve this if I want to add the document name, this will be done by entering the right metadata

years.prop.sci$year<-as.numeric(unique(dl.sci.df$year))

years.prop.sci<-years.prop.sci[with(years.prop.sci, order(year)), ]




