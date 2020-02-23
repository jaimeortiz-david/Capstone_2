#R code for topic modelling of dfr JSTOR text data

install.packages(pkgs = "devtools", dependencies = TRUE)
library(devtools)
install_github("benmarwick/JSTORr")
library(JSTORr)
path = "~/Dropbox/2016-3"

##Unpack JSTOR journal articles and bibliographic data to a Document Term Matrix of 1-grams

# change directory to 1-gram CSV files    
setwd(paste0(path,"/wordcounts"))

#### get list of data, the CSV files of wordcounts

myfiles <- dir(pattern = "\\.(csv|CSV)$", full.names = TRUE)
# read CSV files into a R data.table object
# fread is 10x faster than read.csv...
suppressMessages(library(data.table))
library(plyr)
read_csv2dt <- function(x) data.table(fread(x, sep = ",", stringsAsFactors=FALSE))


        library(plyr)
        aawc <-  llply(myfiles, read_csv2dt, .progress = "text", .inform = FALSE)


# assign file names to each dataframe in the list
names(aawc) <- myfiles

# Identify empty CSV files and exclude them
lens <- sapply(aawc, function(i) i[1]$WEIGHT + i[2]$WEIGHT + i[3]$WEIGHT)
full <- unname(!is.na(lens))
# Subset only CSV files with at least three words...
aawc1 <- aawc[full]

#### convert DfR format to dtm for each doc

# custom version of tm::DocumentTermMatrix for 1-grams
library(slam); library(tm)
my_dtm_1gram <- function(x){ 
        y <- as.integer(x$WEIGHT)
        names(y) <- x$WORDCOUNTS
        v =  unname(y)          # num
        i = rep(1, length(y))   # int
        j = seq(1:length(y))    # int
        z <- simple_triplet_matrix(v = v,   # num
                                   i = i,        # int
                                   j = j,        # int
                                   nrow = max(i),
                                   ncol = max(j), 
                                   dimnames =                          
                                           list(                              
                                                   Docs = deparse(substitute(x)),
                                                   Terms = names(y)))
        zz <- as.DocumentTermMatrix(z, weighting = weightTf)
        return(zz)
}

# get all tables into dtms

        aawc2 <- llply(1:length(aawc1), function(i) my_dtm_1gram(aawc1[[i]]), .progress = "text", .inform = FALSE)


################################### 
# subset file names so we only get CSV files with three or more words
myfiles1 <- myfiles[full]
library(stringr)
names(aawc2) <- str_extract(basename(myfiles1), "[^wordcounts_].+[^.CSV]")


#### bring in citations file with biblio data for each paper
setwd(path) # change this to the location of the citations.csv file

# now read in file

cit <- read.delim("citations.tsv", row.names = NULL, comment.char = "", header = TRUE, stringsAsFactors = FALSE, colClasses="character", quote = "")
# replace for-slash with underscore to make it match the filenames
# and replace odd \t that was added during import 
cit$id <- str_extract(chartr('/', '_', cit$id), ".*[^\t]")
# limit list of citations to full length articles only 
# note that citation type is not in the correct column
# changed this in case we get a dataset that was not originally all fla
citfla <- cit[cit$publisher == 'fla',]
# subset from the wordcount data only the full length articles

# subset items in the list of wordcount data whose names are in 
# the list of fla citation IDs (clear out any spaces also)
citfla$id <- as.character(gsub(" ", "", citfla$id))
wordcounts <- aawc2[which(names(aawc2) %in% citfla$id)]

# put citation IDs in the same order with wordcount data names
# which is the same order as myfiles
bibliodata <- (merge(names(wordcounts), citfla, by.x=1, by.y="id"))
# create a variable that holds the year of publication for
# each article
bibliodata$year <- str_extract(bibliodata$issue, "[[:digit:]]{4}")

# clean up a little
rm(aawc1, aawc2, cit, citfla, myfiles); invisible(gc(verbose = FALSE))

# make one giant dtm with all docs (rather slow...)

        wordcounts <- do.call(tm:::c.DocumentTermMatrix, wordcounts)
        
# give docs their DOI as names
wordcounts$dimnames$Docs <- as.character(bibliodata$x)

# somehow docs got to be a factor... fix this
wordcounts <- wordcounts[unique(as.character(wordcounts$dimnames$Docs[1:nrow(wordcounts)])), ]

#removing stopwords...
#stop = "~/Dropbox/GALAPAGOS GOVERNANCE/Galapagos Science//stoplist_GPS.csv"
wordcounts <- wordcounts[, !(wordcounts$dimnames$Terms %in% stopwords(kind = "stoplist_islands"))]

#discarding words with <3 characters (probably OCR errors)
wordcounts <- wordcounts[,nchar(wordcounts$dimnames$Terms) > 3]

#discarding words with >2 consecutive characters (probably OCR errors)
wordcounts <- wordcounts[,!grepl("(.)\\1{2,}", wordcounts$dimnames$Terms)]

#discarding non-ASCII characters
wordcounts <- wordcounts[,(wordcounts$dimnames$Terms %in% iconv(wordcounts$dimnames$Terms, "latin1", "ASCII", sub=""))]

unpack1grams = list("wordcounts" = wordcounts, "bibliodata" = bibliodata)



