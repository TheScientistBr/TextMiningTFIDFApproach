# Plot Class from tree PSA
#
# variable for test into function
# classe <- "at2"
# myClass <- "./class"
# lfile <- "004062006at2.txt.idx"
# kfold <- 1

# reading collection with tf-idf metrics

#book_words <- read.table(file = "data/aTribunaBook_Words.csv")
library("dtplyr")

createClassCentroid <- function(kfold = .7) {
        source("loadConfig.R")
        classes <- read.csv(myClass,stringsAsFactors = FALSE, header = TRUE)
        l <- 0
        centroids <- list()
        sapply(classes, function(classe) {
                subClass <- subset(book_words, class == classe)
                files <- as.character(unique(sort(subClass$file)))
                nfiles <- as.integer(length(files)*kfold)
                files <- files[1:2]
                subClass <- subset(book_words, file %in% files)
                subClass <- data.table(subClass,stringsAsFactors = FALSE)
                
                subClass$mean <- tapply(subClass$tf_idf,subClass$word,mean)
                subClass <- subset(subClass, !is.na(subClass$mean))

                centroid <- data.frame()
                colnames(centroid) <- c("tf_idf")
                write.csv(centroid,paste0("data/centroid.",classe))
        })
}

# centroids$centroid$tf_idf["aconselha"]