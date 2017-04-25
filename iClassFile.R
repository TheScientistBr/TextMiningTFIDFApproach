# Classify file into a class structure from centroid
# The classification use correlation with file and any available centroids
#

library(dplyr)


if(!exists("book_words")) {
        book_words <- read.table(file = "data/aTribunaBook_Words.csv",
                                 stringsAsFactors = FALSE)
}

showResults <- function(iclass = FALSE, print = FALSE) {
        classes <- read.csv("data/class.txt",stringsAsFactors = FALSE)
        if(iclass == FALSE) {
                names(classes)<-"Choose one Class"
                return(classes)
        }
        if(is.numeric(iclass)) {
                iclass <- classes[iclass,]
        }
        print(as.data.frame(iclass))
        results <- lapply(list.files(paste0("./results/",iclass,"/"),full.names = TRUE), 
                          FUN = function(lfile) {                
                                  return(read.csv(lfile))
                        })
        if(iclass != "con") {
                if(!file.exists(paste0("./results/",iclass)))
                        return(sprintf("File %s not found",paste0("./results/",iclass)))
                lFiles <- list.files(paste0("./results/",iclass))
        }
        if(iclass == "con") {
                if(!file.exists("./results/conn"))
                        return(sprintf("File not found ./results/conn"))
                lFiles <- list.files("./results/conn")
        }
        impressao <- data.frame(stringsAsFactors = FALSE)
        for(i in 1:length(results)[1]) {
                topClass <- as.character(head(results[[i]][order(results[[i]]$cor,decreasing = TRUE),],1)$class)
                maxCal <- head(results[[i]][order(results[[i]]$cor,decreasing = TRUE),],1)$cor
                impressao <- rbind(impressao, cbind(topClass,maxCal, lFiles[i]))
        }
        names(impressao)<-c("TopClass","Inference","File")
        if(print == TRUE) {
                print(impressao)
        }
        if(iclass != FALSE) {
                a<-length(which(impressao$TopClass == iclass))
                b<-length(impressao$TopClass)
                sprintf("F1: %d/%d %2.5f",a,b,((a*100)/b))
        }
}



# iCLassFileAll compute sucess and fails in iClassFile classification under that rules
#
iClassFileAll <- function(iclass = FALSE, iniFile = 0, maxFiles = 9999999, clean = TRUE) {
        source("functions.R")
        classes <- read.csv("data/class.txt",stringsAsFactors = FALSE)
        if(iclass == FALSE) {
                names(classes)<-"Choose one Class"
                return(classes)
        }
        if(is.numeric(iclass)) {
                iclass <- classes[iclass,]
        }
        print(as.data.frame(iclass))
        
        if(!dir.exists("./results"))
                dir.create("./results")
        if(clean == TRUE) {
                if(iclass == "con") {
                        if(!dir.exists("./results/conn"))
                                dir.create("./results/conn")
                        unlink("./results/conn/*")
                }
                if(iclass != "con") {
                        if(!dir.exists(paste0("./results/",iclass)))
                                dir.create(paste0("./results/",iclass))
                        unlink(paste0("./results/",iclass,"/*"))
                }
        }
        centroid <- readCentroid(iclass)
        files2test <- read.csv(file = paste0("data/files2test.",iclass), 
                               stringsAsFactors = FALSE,col.names = c("file"))
        subClass <- subset(book_words, class == iclass)
        
        sucess <- 0
        fail <- 0
        i<-0
        results <- data.frame()
        pb <- winProgressBar(title=sprintf("Classification process to %s",iclass), 
                             label="Initiating ...", min=0, max=100, initial=0)
        if(maxFiles>dim(files2test)[1])
                maxFiles<-dim(files2test)[1]
        total = maxFiles
        Subjects <- maxFiles
        class_resp<-"???"
        for(lfile in files2test$file) {
                info <- sprintf("%2.1f%% %d/%d %s %s", round(((i*100)/total),digits = 1),
                                i,total,lfile,class_resp)
                setWinProgressBar(pb, ((i*100)/total), label=info)
                response <- iClassFile(lfile)
                if(response[[1]]$class[1] == "ERRO")
                        next
                class_resp <- response[[2]][[1]]
                if(class_resp == iclass) {
                        sucess = sucess +1
                }
                if(iclass != "con") {
                        if(! dir.exists(paste0("./results/",iclass)))
                                dir.create(paste0("./results/",iclass))
                        write.csv(response[[1]],
                                  file = paste0("./results/",iclass,"/",lfile))
                }
                if(iclass == "con") {
                        if(! dir.exists("./results/conn"))
                                dir.create("./results/conn")
                        write.csv(response[[1]],
                                  file = paste0("./results/conn","/",lfile))
                }
                i = i + 1
                info <- sprintf("%2.1f%% %d/%d %s %s", round(((i*100)/total),digits = 1),
                                i,total,lfile,class_resp)
                setWinProgressBar(pb, ((i*100)/total), label=info)
                if(i>=maxFiles+iniFile) {
                        fail = Subjects - sucess
                        close(pb)
                        return(c(Subjects,sucess,fail,sucess/Subjects))
                        }
        }
        fail = Subjects - sucess
        close(pb)
        return(c(Subjects,sucess,fail,sucess/Subjects))
}

iClassFile <- function(lfile = lfile, wplot = FALSE) {
        source("loadConfig.R")
        source("functions.R")
        doc3 <- "none"
        rho  <- -Inf
        rhoClass <- "none"
        lClasses <- read.csv(myClass,stringsAsFactors = FALSE, header = TRUE)
        response <- data.frame(lClasses)
        response$cor <- 0
        doc  <- subset(book_words,file == lfile)

        if(length(doc$word)[1] < 10)
                return(list(response,c(rhoClass,rho)))
        for(niFiles in list.files("data/",pattern = "centroid.*")) {
                niFiles <- substr(niFiles,nchar(niFiles)-2,nchar(niFiles))
                ni <- readCentroid(niFiles) 
                ni$tfidf <- 0
                mySum <- 0
                for(i  in 1:length(doc$word)[1]) {
                        ind <- which(ni$word == doc[i,]$word)
                        if(length(ind)) {
                                ni[ind,]$tfidf <- doc[i,]$tf_idf
                        }
                }
                ni <- subset(ni, tfidf > 0)
                ni <- subset(ni, mean > 0)
                ni$i <- 1:length(ni$word)
                corr <- 0
                if(length(ni$word) > 3) {
                        ni <- ni[order(ni$mean,decreasing = FALSE),]
                        corr <- sum(ni$i)
                }
                #model1 <- lm(ni$mean ~ ni$i + I(ni$i^2))
                #xmodel2 <- lm(ni$tfidf ~ ni$i + I(ni$i^2))
                #fit <- abs(cor(predict(model1),predict(model2)))
                #corr <- corr * fit
                response$cor[which(response$class == niFiles)] <- corr
                if(corr > rho) {
                        rho <- corr
                        rhoClass <- niFiles
                        doc3<-ni
                }
        }
        if(wplot) {
                par(new=F)
                ifelse(max(doc3$tfidf)>max(doc3$mean),
                       maxylim <- max(doc3$tfidf), maxylim <- max(doc3$mean))
                maxylim<-as.numeric(maxylim)
                plot(doc3$i, doc3$mean, col = "blue", 
                     main = rhoClass,
                     xlim = c(0,max(doc3$i)), ylim = c(0,maxylim+5),
                     xlab = "Terms", ylab = "TF-IDF/Mean")
                lines(doc3$i, predict(lm(doc3$mean ~ doc3$i + I(doc3$i^2))), col = c("blue"))
                par(new=T)
                plot(doc3$i, doc3$tfidf, col = "red", 
                     xlim = c(0,max(doc3$i)), ylim = c(0,maxylim+5),
                     xlab = "Terms", ylab = "TF-IDF/Mean")
                lines(doc3$i, predict(lm(doc3$tfidf ~ doc3$i + I(doc3$i^2))), col = c("red"))
        }
        response <- response[order(response$cor,decreasing = TRUE),]
        return(list(response,c(rhoClass,rho)))
}
