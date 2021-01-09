library(tm)
library(tmcn)
library(Rwordseg)
library(wordcloud)

d.corpus <- Corpus(DirSource("doc"), list(language = NA))

##進行數據清理
#清除標點符號, 數字
d.corpus <- tm_map(d.corpus, removePunctuation)
d.corpus <- tm_map(d.corpus, removeNumbers)
#清除大小寫英文與數字
d.corpus <- tm_map(d.corpus, function(word) {
    gsub("[A-Za-z0-9]", "", word)
})

##進行中文斷詞
words <- readLines("http://wubi.sogou.com/dict/download_txt.php?id=9182") #
words <- toTrad(words)
insertWords(words)

d.corpus <- tm_map(d.corpus[1:100], segmentCN, nature = TRUE)
d.corpus <- tm_map(d.corpus, function(sentence) {
    noun <- lapply(sentence, function(w) {
        w[names(w) == "n"]
    })
    unlist(noun)
})
d.corpus <- Corpus(VectorSource(d.corpus))

myStopWords <- c(stopwordsCN(), "編輯", "時間", "標題", "發信", "實業", "作者")
d.corpus <- tm_map(d.corpus, removeWords, myStopWords)

head(myStopWords, 20)

##建立 TermDocumentMatrix
tdm <- TermDocumentMatrix(d.corpus, control = list(wordLengths = c(2, Inf)))
inspect(tdm[1:10, 1:2])

##畫出關鍵字詞雲
m1 <- as.matrix(tdm)
v <- sort(rowSums(m1), decreasing = TRUE)
d <- data.frame(word = names(v), freq = v)
wordcloud(d$word, d$freq, min.freq = 10, random.order = F, ordered.colors = F, 
    colors = rainbow(length(row.names(m1))))

##尋找關鍵字之間的關聯
d.dtm <- DocumentTermMatrix(d.corpus, control = list(wordLengths = c(2, Inf)))
inspect(d.dtm[1:10, 1:2])
findFreqTerms(d.dtm, 30)
findAssocs(d.dtm, "同學", 0.5)
