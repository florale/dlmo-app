library(data.table)
library(googlesheets4)

source("db.r")

con <- connect_db()

reviews <- read_reviews(con)
setDT(reviews)

# Convert types
reviews[, clicked_dlmo_h := suppressWarnings(as.numeric(clicked_dlmo_h))]

reviews[, reviewed_at_posix := as.POSIXct(
  reviewed_at,
  format = "%Y-%m-%d %H:%M:%S",
  tz = "Australia/Melbourne"
)]

# Keep all submissions
dir.create("outputs", showWarnings = FALSE, recursive = TRUE)

fwrite(reviews, "outputs/dlmo_reviews_all_anonymous.csv")
saveRDS(reviews, "outputs/dlmo_reviews_all_anonymous.rds")

# Latest submission per reviewer x case
setorder(reviews, reviewer, id_tp, reviewed_at_posix, review_id_response)

reviews_latest <- reviews[, .SD[.N], by = .(reviewer, id_tp)]

fwrite(reviews_latest, "outputs/dlmo_reviews_latest_anonymous.csv")
saveRDS(reviews_latest, "outputs/dlmo_reviews_latest_anonymous.rds")

# Merge with local secure key
id_key <- readRDS("secure_local_id_key.rds")
setDT(id_key)

reviews_latest_linked <- merge(
  reviews_latest,
  id_key,
  by.x = "id_tp",
  by.y = "review_id",
  all.x = TRUE
)

fwrite(reviews_latest_linked, "outputs/dlmo_reviews_latest_linked.csv")
saveRDS(reviews_latest_linked, "outputs/dlmo_reviews_latest_linked.rds")