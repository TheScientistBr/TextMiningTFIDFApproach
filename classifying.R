# Classify file into a class structure from centroid
# The classification use correlation with file and any available centroids
#
printf <- function(...) invisible(print(sprintf(...)))

'for a test
maxFiles <- 5
minCor <- .95
class <- "at2"
i1 <-1
i2 <-1
lfile <- as.character(file2test$V1)[1]
'

library("data.table",quietly = TRUE)
book_words <- read.table(file = "data/aTribunaBook_Words.csv")

classifying <- function(class = FALSE, maxFiles = Inf, minCor = .7, clean = FALSE) {
        source("loadConfig.R")
        if(clean) {
                unlink(x = paste0(destClassified,"/*"),recursive = TRUE)
        }
        if(class == FALSE) {
                names(files)<-"Choose one Class"
                return(files)
        }
        files<-as.data.frame(x = list.files(paste0(myClass)))
        ifelse(is.numeric(class),
               classe <- as.character(files[[1]])[class],
               classe <- class)
        print(as.data.frame(classe))
        if(!dir.exists(classAtractors))
                dir.create(classAtractors)
        if(!dir.exists(trashClassified))
                dir.create(trashClassified)
        if(!dir.exists(destClassified))
                dir.create(destClassified)
        if(!dir.exists(source2Classify))
                return("ERRO - there is no source to classify")
        
        file2test <- read.csv(paste0("./files2test/",classe,".tst"), stringsAsFactors=FALSE)
        sourceFiles <- list.files(source2Classify)
        
        listAtractors <- data.frame(AtractorClass = list.files(paste0(classAtractors)))
        classAtractor <- sapply(list.files(classAtractors),list)
        classAtractor <- sapply(classAtractor[],function(class) { 
                classAtractor[[class]] <- list.files(paste0(classAtractors,"/",classAtractor[[class]]), 
                                                   full.names = TRUE)
                               })
        countFiles <- 0
        
        # All files from source
        mostFreqData <<- data.frame()
        mostFreqData <- data.frame()
        for(lfile in file2test[,2]) {
                lfile2plot <- lfile
                lfile <- paste0(lfile,".idx")
                if(countFiles >= maxFiles)
                        break
                if(is.na(lfile)) {
                        printf("ERROR file %d", countFiles)
                }
                doc  <- read.csv(paste0("index/",lfile),stringsAsFactors = FALSE, header = FALSE,
                                 col.names = c("term","tfidf"),sep = ";", 
                                 encoding = "UTF-8")
                countFiles <- countFiles + 1

                # Compare all atractors, winner is most freq
                mostFreq <- data.table(class = listAtractors[,1], cor = 0)
                setkey(mostFreq,class)
                for(pAtractor in as.character(listAtractors[,1])) {
                        for(fAtractor in classAtractor[[pAtractor]]) {
                                ni <- read.csv(fAtractor,stringsAsFactors = FALSE, 
                                               header = FALSE, col.names = c("term","mean"),
                                               sep = ";", encoding = "UTF-8")
                                ni$tfidf <- 0
                                ni$i <- 0
                                for(i  in 1:length(doc$term)[1]) {
                                        ind <- which(ni$term == doc$term[i])
                                        if(length(ni$term[ind])[1] > 0) {
                                                ni$tfidf[ind] <- doc$tfidf[i]
                                                }
                                        }
                                ni <- subset(ni, tfidf > 0)
                                ni <- subset(ni, mean > 0)
                                if(length(ni$term) < 10) {
                                        next
                                        }
                                ni <- ni[order(ni$mean,decreasing = FALSE),]
                                ni$i <- 1:length(ni$term)
                                model1 <- lm(ni$mean ~ ni$i + I(ni$i^2))
                                model2 <- lm(ni$tfidf ~ ni$i + I(ni$i^2))
                                corr <- cor(predict(model1),predict(model2))
                                if(corr >= minCor) {
                                        mostFreq[pAtractor,2] <- mostFreq[pAtractor,2] +1
                                        }
                        }

                }
                mostFreq$corp <- 0
                for(i in 1:length(mostFreq$class)) {
                        mostFreq[i]$corp <- ( mostFreq[i]$cor *100) / 
                                length(classAtractor[[mostFreq[i]$class]])
                }
                classChoice <- mostFreq[order(mostFreq$corp,decreasing = TRUE)][[1]][1]
                if(!dir.exists(paste0(destClassified,"/",classChoice)))
                        dir.create(paste0(destClassified,"/",classChoice))
                file.copy(from = paste0(index,"/",lfile), 
                          to = paste0(destClassified,"/",classChoice),
                          overwrite = TRUE)
                mostFreq$file <- lfile
                mostFreqData <- rbind(mostFreqData,mostFreq)
        }
mostFreqData <<- mostFreqData[order(mostFreqData$corp,decreasing = TRUE)]
}

showAtractors <- function(class = FALSE, maxFiles = Inf) {
        source("loadConfig.R")
        files<-as.data.frame(x = list.files(paste0(myClass)))
        if(class == FALSE) {
                names(files)<-"Choose one Class"
                return(files)
        }
        classe <- class
        if(is.numeric(class)) {
                classe<-as.character(files[[1]])[class]
        }
        print(as.data.frame(classe))
        lfiles <- list.files(paste0(classAtractors,"/",classe))
        lfiles <- substring(lfiles,first = 1,last = nchar(lfiles)-4)
        if(file.exists(paste0("correlation/",classe,".cor")))
                corData <- read.csv(paste0("./correlation/",classe,".cor"),
                                    stringsAsFactors = FALSE)
        lAtractors <- sapply(1:length(lfiles), function(i) {
                myFile <- which(corData$Filename == lfiles[i])
                })
        print(corData[lAtractors,2:3])
        
}

createAtractors <- function(class = FALSE, corMin = .95, maxFiles = Inf) {
        source("loadConfig.R")
        files<-as.data.frame(x = list.files(paste0(myClass)))
        if(class == FALSE) {
                names(files)<-"Choose one Class"
                return(files)
        }
        classe <- class
        if(is.numeric(class)) {
                classe<-as.character(files[[1]])[class]
        }
        print(as.data.frame(classe))
        if(file.exists(paste0("correlation/",classe,".cor")))
                corData <- read.csv(paste0("./correlation/",classe,".cor"),
                                    stringsAsFactors = FALSE)
        else
                print(paste0("File ","correlation/",classe,".cor"," not found"))
        count <- 0
        for(i in 1:length(corData$Filename)) {
                if(i > maxFiles)
                        break
                if(corData[i,]$Correlation >= minCor) {
                        count <- count +1
                        if(!dir.exists(paste0(classAtractors,"/",classe)))
                                dir.create(paste0(classAtractors,"/",classe))
                        file.copy(from = paste0("./index/", corData[i,]$Filename, ".idx"), 
                                  to = paste0(classAtractors,"/",classe),
                                  overwrite = TRUE) 
                        printf("%s %f >",corData[i,]$Filename,corData[i,]$Correlation)
                }
                #else
                        #printf("%s %f X",corData[myFile,]$Filename,corData[myFile,]$Correlation)
        }
        return(count)
}
