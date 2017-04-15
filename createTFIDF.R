library(dplyr)
library(tidytext)
library(ggplot2)
library(tm)
library(data.table)

classe <-  list.dirs("../aTribunaDev/class/",full.names = FALSE,recursive = FALSE)[1]
lfile <-  list.files(paste0("../aTribunaDev/class/",classe))[1]

lemmaWords <- read.table("data/lematizer.txt",strip.white = TRUE, sep = ";", quote = "",
                         stringsAsFactors = FALSE, header = FALSE, encoding = "UTF-8")
colnames(lemmaWords) <- c("lemma","words")

getLemma <- function(text = text) {
                ntermo <- character()
                nr     <- grep(text,lemmaWords)[1]
                ntermo <- lemmaWords[nr,1]
                ifelse(is.na(ntermo),return(text),return(ntermo[[1]]))
}

myDataset <- data.frame(stringsAsFactors = FALSE)
nRead <- Inf     # number files to read by class
id <- 0
for(classe in list.dirs("../aTribunaDev/class/",full.names = FALSE,recursive = FALSE)) {
        ct <- 0         # counter to read files
        for(lfile in list.files(paste0("../aTribunaDev/class/",classe))) {
                texto <- scan(paste0("../aTribunaDev/class/",classe,"/",lfile),
                              what = "character",quiet = TRUE)
                #texto <- sapply(texto,getLemma)
                texto <- paste(texto,collapse = " ")
                texto <- gsub("<.*?>", "", texto)
                documents <- Corpus(VectorSource(texto))
                documents = tm_map(documents, tolower)
                documents = tm_map(documents, removePunctuation)
                documents = tm_map(documents, removeNumbers)
                texto <- tm_map(documents, removeWords,stopwords("portuguese"))$content
                id <- id +1
                myDataset <- rbind(myDataset,c(id,classe, lfile, t(texto)),deparse.level = 0,
                                   stringsAsFactors =  FALSE)
                ct <- ct+1
                if(ct>=nRead)
                        break
        }
}

myDataset <- data.frame(lapply(myDataset,as.character), stringsAsFactors = FALSE)
colnames(myDataset) <- c("ID","class","file","text")
book_words <- myDataset %>%
        unnest_tokens(word, text,to_lower = TRUE) %>%
        count(file,word, sort = TRUE) %>%
        ungroup()

total_words <- book_words %>% group_by(file) %>% summarize(total = sum(n))
book_words <- left_join(book_words, total_words)
book_words

# Create matrix with TF-IDF
book_words <- book_words %>% bind_tf_idf(word, file, n)
book_words <- as.data.table(book_words)

setkey(book_words,file,word)
write.table(book_words,file = "data/aTribunaBook_Words.csv")

