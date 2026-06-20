library(googlesheets4)

sheet_write(
  data = data.frame(
    review_id_response = character(),
    reviewer = character(),
    reviewed_at = character(),
    id_tp = character(),
    timepoint = character(),
    clicked_dlmo_h = numeric(),
    clicked_dlmo_clock = character(),
    decision = character(),
    confidence = character(),
    notes = character()
  ),
  ss = Sys.getenv("DLMO_REVIEW_SHEET"),
  sheet = "reviews"
)