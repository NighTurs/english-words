library(data.table)
library(dplyr)
library(RSQLite)

if (file.exists("words.db")) {
    stop("wrods.db already exists")
}

wf <- fread("data/SUBTLEXus74286wordstextversion.txt", header = T, sep = "\t")
wf <- rename(wf, word = Word, freq_count = FREQcount, cd_count = CDcount,
             freq_low = FREQlow, cd_low = Cdlow, subtl_wf = SUBTLWF, 
             lg10_wf = Lg10WF, subtl_cd = SUBTLCD, lg10_cd = Lg10CD)
wf <- wf[rev(order(freq_count)), rating_wf := .I]

word_sum <- sum(wf$freq_count)
wf <- wf %>% mutate(freq_word = freq_count / word_sum) %>% 
    arrange(rating_wf) %>%
    mutate(cum_percent = cumsum(freq_word))

db <- dbConnect(SQLite(), dbname = "words.db")
dbWriteTable(db, name = "Freq", wf)
dbDisconnect(db)