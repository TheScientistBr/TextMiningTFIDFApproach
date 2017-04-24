# Plot Class from tree PSA
#
# variable for test into function
# classe <- "imo"
# myClass <- "./class"
# lfile <- "004062006at2.txt.idx"
# kfold <- 1

# reading collection with tf-idf metrics

library("data.table")

if(!exists("book_words")) {
        book_words <- read.table(file = "data/aTribunaBook_Words.csv",
                                 stringsAsFactors = FALSE)
}

createFiles2Test <- function(kfold = .7) {
        source("loadConfig.R")
        classes <- read.csv(myClass,stringsAsFactors = FALSE, header = TRUE)
        for(classe in classes$class) {
                subClass <- subset(book_words, class == classe)
                files <- as.character(unique(sort(subClass$file)))
                nfiles <- as.integer(length(files)*kfold)
                files <- files[nfiles:length(files)]
                write(x = files,file = paste0("data/files2test.",classe))
        }        
}

createClassCentroid <- function(kfold = .7) {
        source("loadConfig.R")
        classes <- read.csv(myClass,stringsAsFactors = FALSE, header = TRUE)
        l <- 0
        for(classe in classes) {
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
        }
}

# centroids$centroid$tf_idf["aconselha"]