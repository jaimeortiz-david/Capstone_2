#find the optimal number of topics https://gist.github.com/trinker/594bd132b180a43945f7

library(topicmodels)
library(Rmpfr)


####this part is to use with the grey literatura data
vocab<- vocabulary(m)
ldaformat2dtm(my.corpus.l,vocab)


######

wordcount <- do.call(tm:::c.DocumentTermMatrix, documents)

optimal_k <- function(x, max.k = 30, burnin = 1000, iter = 1000, keep = 50, method = "Gibbs", verbose = TRUE, ...){
        
        
        if (max.k > 20) {
                message("\nGrab a cup of coffee this is gonna take a while...\n")
                flush.console()
        }
        
        tic <- Sys.time()
        
        hm_many <- sapply(2:max.k, function(k){
                fitted <- topicmodels::LDA(x, k = k, method = method, control = list(burnin = burnin, iter = iter, keep = keep) )
                logLiks <- fitted@logLiks[-c(1:(burnin/keep))]
                harmonicMean(logLiks)
        })
        
        out <- c(2:max.k)[which.max(hm_many)]
        
        class(out) <- c("optimal_k", class(out))
        attributes(out)[["k_dataframe"]] <- data.frame(
                k = 2:max.k, 
                harmonic_mean = hm_many
        )
        if (isTRUE(verbose)) cat(sprintf("Optimal number of topics = %s\n",as.numeric(out)))
        out
}

harmonicMean <- function(logLikelihoods, precision=2000L) {
        llMed <- Rmpfr::median(logLikelihoods)
        as.double(llMed - log(Rmpfr::mean(exp(-Rmpfr::mpfr(logLikelihoods, prec = precision) + llMed))))
}



#' Plots a plot.optimal_k Object
#' 
#' Plots a plot.optimal_k object
#' 
#' @param x A \code{optimal_k} object.
#' @param \ldots Ignored.
#' @method plot plot.optimal_k
#' @export 
plot.optimal_k <- function(x, ...){
        
        y <- attributes(x)[["k_dataframe"]]
        y <- y[y[["k"]] == as.numeric(x), ]
        
        ggplot2::ggplot(attributes(x)[["k_dataframe"]], ggplot2::aes_string(x="k", y="harmonic_mean")) + 
                ggplot2::xlab("Number of Topics") + 
                ggplot2::ylab("Harmonic Mean of Log Likelihood") + 
                geom_point(data=y, color="blue", fill=NA,  size = 6, shape = 21) +
                ggplot2::geom_line(size=1) + 
                ggplot2::theme_bw()  + 
                ggplot2::theme(
                        axis.title.x = ggplot2::element_text(vjust = -0.25, size = 14),
                        axis.title.y = ggplot2::element_text(size = 14, angle=90)
                )
        
}

#' Prints a optimal_k Object
#' 
#' Prints a optimal_k object
#' 
#' @param x A \code{optimal_k} object.
#' @param \ldots Ignored.
#' @method print optimal_k
#' @export 
print.optimal_k <- function(x, ...){
        
        print(graphics::plot(x))
        
}

opti_k<-optimal_k(x=wordcount,max.k=50, burnin = 1000, iter = 1000, keep = 50, method = "Gibbs", verbose = TRUE)


