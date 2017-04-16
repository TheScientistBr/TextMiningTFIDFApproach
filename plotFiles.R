# Plot Files from other under index directory
#
library(dplyr)
library("ggplot2")
# lfile <- "004062006at2.txt"

book_words <- read.table(file = "data/aTribunaBook_Words.csv")

plotWords <- function(lfile, corte = 0) {
        doc <- subset(book_words,file == lfile)
        doc <- subset(doc, tf_idf > corte)
        doc$i <- 1:length(doc$word)
        if(dim(doc)[1] > 100)
                doc <- doc[1:100,]
        doc <- doc[order(doc$tf_idf,decreasing = FALSE),]
        p <- ggplot(doc, aes(i, tf_idf, label = doc$word)) + 
                geom_text(check_overlap = TRUE,size = I(doc$tf_idf*300), aes(colour = doc$tf_idf)) +
                theme(legend.position="none")
        print(p)
        corM <- lm(doc$tf_idf ~ doc$i + I(doc$i^2))
        return(corM)
}


# file2 <- "05092005at2.txt"
# file1 <- "004062006at2.txt"

plotFile <- function(file1 = file1, file2 = file2, wplot = TRUE, typePlot = "p") {
        corM <- 0
        doc1  <- subset(book_words,file == file1)
        doc2  <- subset(book_words,file == file2)
        centroid <- c(doc1$word,doc2$word)
        centroid <- unique(sort(centroid))
        ni <- data.frame(word = centroid, stringsAsFactors = FALSE)
        ni$tfidf <- 0
        ni$mean <- 0
        ni$i <- 0
        if(wplot) {
                soma <- 0
                for(i  in 1:length(doc1$word)[1]) {
                        ind <- which(ni$word == doc1[i,]$word)
                        ni[ind,]$tfidf <- doc1[i,]$tf_idf
                        }
                for(i  in 1:length(doc2$word)[1]) {
                        ind <- which(ni$word == doc2[i,]$word)
                        ni[ind,]$mean <- doc2[i,]$tf_idf
                }
                ni <- subset(ni, tfidf > 0)
                ni <- subset(ni, mean > 0)
                if(length(ni$term) < 10) {
                        return(paste0("File ",index,"/",file2,".idx has less than 10 characters"))
                }
                ni <- ni[order(ni$mean,decreasing = FALSE),]
                ni$i <- 1:length(ni$word)
                model1 <- lm(ni$mean ~ ni$i + I(ni$i^2))
                model2 <- lm(ni$tfidf ~ ni$i + I(ni$i^2))
                corM <- abs(cor(predict(model1),predict(model2)))
                plot(ni$i, ni$mean, col = "blue", 
                     type = "p", main = paste(file1,file2),
                     xlim = c(0,max(ni$i)), ylim = c(0,max(ni$tfidf)),
                     xlab = paste("correlation: ",corM), ylab = "TF-IDF")
                lines(ni$i, predict(lm(ni$mean ~ ni$i + I(ni$i^2))), col = c("blue"))
                par(new = "T")
                plot(ni$i, ni$tfidf, col = "red", 
                     type = "p",
                     xlim = c(0,max(ni$i)), ylim = c(0,max(ni$tfidf)),
                     xlab = paste("correlation: ",corM), ylab = "TF-IDF")
                lines(ni$i, predict(lm(ni$tfidf ~ ni$i + I(ni$i^2))), col = c("red"))
                return(corM)
        }
        return(c("ERRO",length(compare)[1]))
}


