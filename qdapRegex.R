#I will use the qdapRegex package to remove non-words and words with 1 and 2 characters and add to stop list

library("qdapRegex")
x<- unique(counts$word)
x<-stringi::stri_trans_general(x, "latin-ascii") #this is used to change some ascii characters to real letters to preserve words

write.csv(x, "~/Dropbox//vocabulary.csv") ##I have used this to add more words to the dictionary


a<-c()
a<-unlist(rm_non_ascii(x,extract = T,ascii.out = F))
b<-unlist(rm_non_words(x,extract = T))
c<-unlist(rm_repeated_characters(x,extract = T))
d<-unlist(rm_nchar_words(rm_non_words(x), "1,3",extract = T))
y<-cbind(b,c,d)
write.csv(y, "~/Dropbox//non_words.csv")
write.csv(a, "~/Dropbox//ascii.csv")

## get name of authors and name of journal to add to the stop list 2 just those articles related to galapagos

meta <-meta %>%group_by(id) %>% filter(id %in% id_list)
x<-cbind(meta$author,meta$journaltitle)
write.csv(x, "~/Dropbox//author_journal.csv")

