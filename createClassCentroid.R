# Plot Class from tree PSA
#
# variable for test into function
# classe <- "at2"
# myClass <- "./class"
# lfile <- "004062006at2.txt.idx"
# kfold <- 1

# reading collection with tf-idf metrics

library("data.table")
book_words <- read.table(file = "data/aTribunaBook_Words.csv")

createClassCentroid <- function(kfold = .7) {
        source("loadConfig.R")
        classes <- read.csv(myClass,stringsAsFactors = FALSE, header = TRUE)
        l <- 0
        sapply(classes, function(classe) {
                subClass <- subset(book_words, class == classe)
                files <- as.character(unique(sort(subClass$file)))
                nfiles <- as.integer(length(files)*kfold)
                files <- files[1:nfiles]
                subClass <- subset(book_words, file %in% files)
                centroid <- as.character(unique(sort(subClass$word)))
                centroid <- as.data.table(centroid)
                colnames(centroid) <- c("word")
                setkey(centroid,word)
                centroid$mean <- 0
                sapply(centroid$word, function(myword) {
                        centroid[myword,2] <<- 
                                mean(subClass[which(subClass$word == myword),]$tf_idf)
                })
                write.csv(centroid,paste0("data/centroid.",classe))
        })
}

# centroids$centroid$tf_idf["aconselha"]