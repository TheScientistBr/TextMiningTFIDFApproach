# Classify file into a class structure from centroid
# The classification use correlation with file and any available centroids
#
printf <- function(...) invisible(print(sprintf(...)))

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
        files<-as.data.frame(x = list.files(paste0(myClass)))
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
        ni <- read.csv(paste0(statistic,"/",classe,".trn"),stringsAsFactors = FALSE)
        ni$tfidf <- 0
        ni <- ni[order(ni$mean,decreasing = FALSE),]
        ni$i <- 1:length(ni$term)
        result<-data.frame(stringsAsFactors = FALSE, Filename = NULL,
                           Correlation = NULL)
        nibkp<-ni
        soma<-0
        cor7<-0
        for(lfile in list.files(paste0(myClass,"/",classe))) {
                soma=soma+1
                if(soma > max)
                        break
                doc  <- read.csv(paste0(index,"/",lfile,".idx"),stringsAsFactors = FALSE, header = FALSE,
                                 col.names = c("term","tfidf"),sep = ";", encoding = "UTF-8")
                if(file.info(paste0(index,"/",lfile,".idx"))$size == 1) {
                        printf("%5s - %15s empty", soma,lfile)
                        next
                }
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

featureCount <- function(class = FALSE) {
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
        val <- 0
        tam <- length(list.files(paste0(myClass,"/",classe)))[1]
        for(lfile in list.files(paste0(myClass,"/",classe))) {
                doc  <- read.csv(paste0(index,"/",lfile,".idx"),
                                 stringsAsFactors = FALSE, header = FALSE,
                                 col.names = c("term","tfidf"),
                                 sep = ";", encoding = "UTF-8")
                val <- val + length(doc$term)[1]
                }
        return(val/tam)
}

loadTestFile <- function(class = class) {
        if(file.exists(paste0("./files2test/",class,".tst"))) {
                file2test <<- read.csv(paste0("./files2test/",class,".tst"), 
                                       stringsAsFactors = FALSE)
                }
}

loadCentroid <- function(class = class) {
        if(file.exists(paste0("./statistic/",class,".trn"))) {
                centroid <<- read.csv(paste0("./statistic/",class,".trn"))
        }
}

showResults <- function(classe = FALSE, print = FALSE) {
        files<-as.data.frame(x = list.files("./class"))
        if(classe == FALSE) {
                names(files)<-"Choose one Class"
                return(files)
                }
        if(is.numeric(classe)) {
                classe<-as.character(files[[1]])[classe]
                }
        print(as.data.frame(classe))
        results <- lapply(list.files(paste0("./results/",classe,"/"),full.names = TRUE), 
                          FUN = function(lfile) {                
                                  return(read.csv(lfile))
                        })
        if(!file.exists(paste0("./results/",classe)))
                return(sprintf("File %s not found",paste0("./results/",classe)))
        lFiles <- list.files(paste0("./results/",classe))
        impressao <- data.frame(stringsAsFactors = FALSE)
        for(i in 1:length(results)[1]) {
                topClass <- as.character(head(results[[i]][order(results[[i]]$cor,decreasing = TRUE),],1)$lClasses)
                maxCal <- head(results[[i]][order(results[[i]]$cor,decreasing = TRUE),],1)$cor
                impressao <- rbind(impressao, cbind(topClass,maxCal, lFiles[i]))
        }
        names(impressao)<-c("TopClass","Inference","File")
        if(print == TRUE) {
                print(impressao)
                }
        if(classe != FALSE) {
                a<-length(which(impressao$TopClass == paste0(classe,".trn")))
                b<-length(impressao$TopClass)
                sprintf("F1: %d/%d %2.5f",a,b,((a*100)/b))
        }
}


testfuncion <- function() {
        i<-1
        niFiles<-"at2.trn"
        lfile<-"./index/004062006at2.txt.idx"
        source("loadConfig.R")
        old_pvalue<-9999
        rho  <- 0
        doc3 <- "none"
        corte <- 0
        ni   <- read.csv(paste0(statistic,"/",niFiles))
        doc  <- read.csv(lfile,stringsAsFactors = FALSE, header = FALSE,
                         col.names = c("term","tfidf"),sep = ";", encoding = "UTF-8")
        doc$mean <- 0
        doc$i <- 0
        for( i  in 1:length(doc$term)[1]) {
                iTerm <- which(doc$term[i] == ni$term)
                if(length(iTerm) != 0) {
                        doc$mean[i] <- ni$mean[iTerm]
                        doc$i[i] <- ni$i[iTerm]
                        doc$i[i] <- ni$i[iTerm]
                }
        }
        doc2 <- subset(doc)
        corM <- summary(lm(doc2$tfidf ~ doc2$mean))$coef[7]
        x <- lm(doc2$tfidf ~ doc2$mean)
        pvalue <- x$coefficients[1]
}

seeDiference <- function(x) {
        w<-0
        for(i in length(x$term)) {
                w <- w + abs(x$tfidf[[i]]-x$mean[[i]])
        }
        return(w/i)
}

# iCLassFileAll compute sucess and fails in iClassFile classification under that rules
#
iCLassFileAll <- function(class = FALSE, iniFile = 0, maxFiles = 9999999, clean = TRUE) {
        files<-as.data.frame(x = list.files("./class"))
        if(class == FALSE) {
                names(files)<-"Choose one Class"
                return(files)
        }
        if(is.numeric(class)) {
                class<-as.character(files[[1]])[class]
        }
        print(as.data.frame(class))
        
        if(!dir.exists("./results"))
                dir.create("./results")
        if(clean == TRUE) {
                if(!dir.exists(paste0("./results/",class)))
                        dir.create(paste0("./results/",class))
                unlink(paste0("./results/",class,"/*"))
                }
        file2test <- read.csv(paste0("./files2test/",class,".tst"), stringsAsFactors=FALSE)
        sucess <- 0
        fail <- 0
        i<-1
        results <- data.frame()
        pb <- winProgressBar(title=sprintf("Classification process to %s",class), 
                             label="Initiating ...", min=0, max=100, initial=0)
        if(maxFiles>dim(file2test)[1])
                maxFiles<-dim(file2test)[1]
        total = maxFiles
        Subjects <- maxFiles
        class_resp<-"???"
        for(lfile in as.character(file2test$V1)) {
                info <- sprintf("%2.1f%% %d/%d %s %s", round(((i*100)/total),digits = 1),
                                i,total,as.character(lfile),class_resp)
                setWinProgressBar(pb, ((i*100)/total), label=info)
                response <- iClassFile(paste0(index,"/",as.character(lfile),".idx"))
                if(response[[1]]$lClasses[1] == "ERRO")
                        next
                class_resp <- substr(response[[2]][[1]],start = 1,
                                     stop = nchar(response[[2]][1])-4)
                if(class_resp == class) {
                        sucess = sucess +1
                }
                if(! dir.exists(paste0("./results/",class)))
                        dir.create(paste0("./results/",class))
                write.csv(response[[1]],
                          file = paste0("./results/",class,"/",as.character(lfile)))
                i = i + 1
                info <- sprintf("%2.1f%% %d/%d %s %s", round(((i*100)/total),digits = 1),
                                i,total,as.character(lfile),class_resp)
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
        doc3 <- "none"
        rho  <- -999999
        rhoClass <- "none"
        lClasses <- list.files(statistic)
        response <- data.frame(lClasses)
        response$cor <- 0
        doc  <- subset(book_words,file == lfile)

        if(length(doc$term)[1] < 10)
                return(list(response,c(rhoClass,rho)))
        for(niFiles in list.files(statistic)) {
                ni <- read.csv(paste0(statistic,"/",niFiles),stringsAsFactors = FALSE)
                ni$tfidf <- 0
                soma<-0
                total<-length(ni$term)[1]
                for(i  in 1:length(doc$term)[1]) {
                        ind <- which(ni$term == doc$term[i])
                        if(length(ni$term[ind])[1] > 0) {
                                ni$tfidf[ind] <- doc$tfidf[i]
                                }
                }
                ni <- subset(ni, tfidf > 0)
                ni <- ni[order(ni$mean,decreasing = FALSE),]
                ni$i <- 1:length(ni$term)
                #ni <- subset(ni, mean > 0)
                model1 <- lm(ni$mean ~ ni$i + I(ni$i^2))
                model2 <- lm(ni$tfidf ~ ni$i + I(ni$i^2))
                corr <- abs(cor(predict(model1),predict(model2)))
                response$cor[which(response$lClasses == niFiles)] <- corr
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
                     type = "p", main = rhoClass,
                     xlim = c(0,max(doc3$i)), ylim = c(0,maxylim+5),
                     xlab = "Terms", ylab = "TF-IDF/Mean")
                lines(doc3$i, predict(lm(doc3$mean ~ doc3$i + I(doc3$i^2))), col = c("blue"))
                par(new=T)
                plot(doc3$i, doc3$tfidf, col = "red", 
                     pch = 16, xlim = c(0,max(doc3$i)), ylim = c(0,maxylim+5),
                     xlab = "Terms", ylab = "TF-IDF/Mean")
                lines(doc3$i, predict(lm(doc3$tfidf ~ doc3$i + I(doc3$i^2))), col = c("red"))
        }
        response <- response[order(response$cor,decreasing = TRUE),]
        return(list(response,c(rhoClass,rho)))
}
