######

y<-str_split(documents$text[1],"\\W")
z<-rep(documents$id[1], times = 1000)


words<-as.data.frame(unlist(y, use.names = F),stringsAsFactors=F)
words<-cbind(z,words)

colnames(words)<-c("id","word")

library(tm)



#create metadata
meta<-NULL
x<- str_extract(words$id, "[[:digit:]]{4}")
meta<- cbind(words$id,x)
colnames(meta)<- c("id","year")
meta$title<- gsub("\\..*","", met$id)




#####


table(documents)
