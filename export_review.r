library(DBI)
library(RSQLite)
library(data.table)

con <- dbConnect(SQLite(), "outputs/dlmo_reviews.sqlite")

reviews <- as.data.table(dbReadTable(con, "reviews"))

fwrite(reviews, "outputs/dlmo_reviews.csv")
saveRDS(reviews, "outputs/dlmo_reviews.rds")

dbDisconnect(con)