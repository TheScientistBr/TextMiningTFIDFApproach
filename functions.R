readCentroid <- function(classCentroid) {
        if(file.exists(paste0("data/centroid.",classCentroid))) {
                return(read.csv(paste0("data/centroid.",classCentroid),stringsAsFactors = FALSE))
                
        }
}

printf <- function(...) invisible(print(sprintf(...)))


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
