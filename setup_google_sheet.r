library(googlesheets4)

gs4_auth(email = "your.email@gmail.com")

review_header <- data.frame(
  review_id = character(),
  reviewer = character(),
  reviewed_at = character(),
  id_tp = character(),
  ID = character(),
  timepoint = character(),
  clicked_dlmo_h = numeric(),
  clicked_dlmo_clock = character(),
  decision = character(),
  confidence = character(),
  notes = character(),
  dlmo_hs = numeric(),
  dlmo_fixed_3 = numeric(),
  dlmo_fixed_4 = numeric(),
  reason_category_revised = character(),
  stringsAsFactors = FALSE
)

ss <- gs4_create("dlmo_review_responses")
sheet_write(review_header, ss = ss, sheet = "reviews")

ss