#get the words from the unpack1grams

words <- unpack1grams$wordcounts
words$dimnames$Docs <- as.character(words$dimnames$Docs)
words <- words[unique(as.character(words$dimnames$Docs[1:nrow(words)])), ]
words <- removeSparseTerms(words, 0.75)

#convert to text files

n <- length(wordcounts$dimnames$Docs)
for(i in 1:n){
        cat(paste0("Writing text file ", i," of ", n, " files\n"))
        tmp <- rep(wordcounts$dimnames$Terms, as.matrix(wordcounts[i,,]))
        tmp <- paste(tmp, collapse = " ")
        write.table(tmp, file = paste(wordcounts$dimnames$Docs[i], "txt", sep = "."), quote = FALSE,
                    row.names = FALSE, eol = " ")
        cat("\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b\b")
}