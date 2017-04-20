
showCorrelation <- function(class = FALSE, maxFile = 6, corte = Inf) {
        source("loadConfig.R")
        files<-as.data.frame(x = list.files(paste0(myClass)))
        if(class == FALSE) {
                names(files)<-"Choose one Class"
                return(files)
        }
        if(is.numeric(class)) {
                classe<-as.character(files[[1]])[class]
        }
        print(as.data.frame(classe))
        if(file.exists(paste0("correlation/",classe,".cor")))
                corData <- read.csv(paste0("./correlation/",classe,".cor"), nrows = maxFile)
        else
                print(paste0("File ","correlation/",classe,".cor"," not found"))
        print(corData)
}


createCorrelation <- function(class = FALSE, clean = FALSE, max = Inf) {
        source("loadConfig.R")
        files <- read.table(myClass)
        if(class == FALSE) {
                names(files)<-"Choose one Class"
                return(files)
        }
        if(is.numeric(class)) {
                classe<-as.character(files[[1]])[class]
        }
        if(clean == TRUE) {
                if(!dir.exists("./correlation"))
                        dir.create("./correlation")
                unlink("./correlation/*")
        }
        print(as.data.frame(classe))
        ni <- read.csv(paste0("./data/centroid.",classe),stringsAsFactors = FALSE)
        ni$tfidf <- 0
        ni <- ni[order(ni$mean,decreasing = FALSE),]
        ni$i <- 1:length(ni$term)
        result<-data.frame()
        nibkp<-ni
        soma<-0
        cor7<-0
        for(lfile in list.files(paste0(myClass,"/",classe))) {
                soma=soma+1
                if(soma > max)
                        break
                doc  <- doc <- subset(book_words,file == lfile)
                if(length(doc$term)[1] < 10) {
                        printf("%5s - %15s length < 10", soma,lfile)
                        next
                }
                ni<-nibkp
                for(i  in 1:length(doc$term)[1]) {
                        ind <- which(ni$term == doc$term[i])
                        if(length(ni$term[ind])[1] > 0) {
                                ni$tfidf[ind] <- doc$tfidf[i]
                        }
                }
                ni <- subset(ni, tfidf > 0)
                ni <- ni[order(ni$mean,decreasing = FALSE),]
                ni$i <- 1:length(ni$term)
                model1 <- lm(ni$mean ~ ni$i + I(ni$i^2))
                model2 <- lm(ni$tfidf ~ ni$i + I(ni$i^2))
                corr <- abs(cor(predict(model1),predict(model2)))
                if(corr<.7) {
                        cor7=cor7+1
                }
                printf("%5s - %15s %2.5f %2.5f", soma,lfile,corr,(cor7*100)/soma)
                result <- rbind(result,data.frame(Filename = lfile,
                                                  Correlation = corr))
        }
        write.csv(result,paste0("./correlation/",classe,".cor"))
}
