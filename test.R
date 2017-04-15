myDataset <- data.frame(stringsAsFactors = FALSE)
id <- 1
classe <- list.dirs("../aTribunaDev/class/",full.names = FALSE,recursive = FALSE)[1]
lfile <- list.files(paste0("../aTribunaDev/class/",classe))[1]

texto <- scan(paste0("../aTribunaDev/class/",classe,"/",lfile),
              what = "character")
texto <- paste(texto,collapse = " ")
texto <- tolower(texto)
texto <- gsub("<.*?>", "", texto)
texto <- gsub('[[:punct:]]', '', texto)
documents <- Corpus(VectorSource(texto))
texto <- tm_map(documents, removeWords,stopwords("portuguese"))$content
myDataset <- rbind(myDataset,c(id,classe, lfile, t(texto)),deparse.level = 0,
                   stringsAsFactors =  FALSE)


id <- 2
classe <- list.dirs("../aTribunaDev/class/",full.names = FALSE,recursive = FALSE)[2]
lfile <- list.files(paste0("../aTribunaDev/class/",classe))[2]

texto <- scan(paste0("../aTribunaDev/class/",classe,"/",lfile),
              what = "character")
texto <- paste(texto,collapse = " ")
texto <- tolower(texto)
texto <- gsub("<.*?>", "", texto)
texto <- gsub('[[:punct:]]', '', texto)
documents <- Corpus(VectorSource(texto))
texto <- tm_map(documents, removeWords,stopwords("portuguese"))$content
myDataset <- rbind(myDataset,c(id,classe, lfile, t(texto)),deparse.level = 0,
                   stringsAsFactors =  FALSE)


myDataset <- data.frame(lapply(myDataset,as.character), stringsAsFactors = FALSE)
colnames(myDataset) <- c("ID","class","file","text")


