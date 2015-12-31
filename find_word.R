library(RSQLite, quietly = T)

args <- commandArgs(trailingOnly = TRUE)

db <- dbConnect(SQLite(), dbname = "words.db")
dbGetQuery(db, paste0("select * from Freq where word = ", '\'', args[1], '\''))
res <- dbDisconnect(db)