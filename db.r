library(data.table)
library(googlesheets4)

review_cols <- c(
  "review_id_response",
  "reviewer",
  "reviewed_at",
  "id_tp",
  "timepoint",
  "clicked_dlmo_h",
  "clicked_dlmo_clock",
  "decision",
  "confidence",
  "notes"
)

# connect_db <- function(sheet_id = Sys.getenv("DLMO_REVIEW_SHEET"),
#                        sheet = "reviews") {
#   if (!nzchar(sheet_id)) {
#     stop("DLMO_REVIEW_SHEET is not set. Add the Google Sheet URL or ID to .Renviron.")
#   }
  
#   # Local interactive use
#   if (!googlesheets4::gs4_has_token()) {
#     googlesheets4::gs4_auth()
#   }
  
#   list(
#     ss = sheet_id,
#     sheet = sheet
#   )
# }

google_json <- Sys.getenv("GOOGLE_SERVICE_ACCOUNT_JSON")

if (nzchar(google_json)) {
  cred_path <- tempfile(fileext = ".json")
  writeLines(google_json, cred_path)
  Sys.setenv(GOOGLE_APPLICATION_CREDENTIALS = cred_path)
}

connect_db <- function(sheet_id = Sys.getenv("DLMO_REVIEW_SHEET"),
                       sheet = "reviews",
                       service_account_path = Sys.getenv("GOOGLE_APPLICATION_CREDENTIALS")) {
  if (!nzchar(sheet_id)) {
    stop("DLMO_REVIEW_SHEET is not set. Add the Google Sheet URL or ID to .Renviron.")
  }
  
  if (!nzchar(service_account_path)) {
    stop("GOOGLE_APPLICATION_CREDENTIALS is not set. Add the service account JSON path to .Renviron.")
  }
  
  if (!file.exists(service_account_path)) {
    stop("Service account JSON file not found: ", service_account_path)
  }
  
  googlesheets4::gs4_auth(
    path = service_account_path,
    scopes = "https://www.googleapis.com/auth/spreadsheets"
  )
  
  list(
    ss = sheet_id,
    sheet = sheet
  )
}

new_review_id <- function() {
  paste0(
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "_",
    sample(100000:999999, 1)
  )
}

save_review <- function(con, row) {
  row <- as.data.frame(row, stringsAsFactors = FALSE)
  
  row$review_id_response <- new_review_id()
  
  missing_cols <- setdiff(review_cols, names(row))
  for (x in missing_cols) row[[x]] <- NA
  
  row <- row[, review_cols, drop = FALSE]
  
  googlesheets4::sheet_append(
    ss = con$ss,
    data = row,
    sheet = con$sheet
  )
  
  invisible(row)
}

read_reviews <- function(con) {
  out <- googlesheets4::read_sheet(
    ss = con$ss,
    sheet = con$sheet,
    col_types = "c"
  )
  
  out <- as.data.table(out)
  
  missing_cols <- setdiff(review_cols, names(out))
  for (x in missing_cols) out[[x]] <- NA_character_
  
  out
}

get_latest_review <- function(con, reviewer, id_tp) {
  reviewer_name <- trimws(reviewer)
  id_tp_value <- id_tp
  
  if (!nzchar(reviewer_name) || is.null(id_tp_value) || is.na(id_tp_value)) {
    return(NULL)
  }
  
  reviews <- read_reviews(con)
  
  if (nrow(reviews) == 0) {
    return(NULL)
  }
  
  reviews <- reviews[
    trimws(get("reviewer")) == reviewer_name &
      get("id_tp") == id_tp_value
  ]
  
  if (nrow(reviews) == 0) {
    return(NULL)
  }
  
  reviews[, reviewed_at_posix := as.POSIXct(reviewed_at, tz = Sys.timezone())]
  setorder(reviews, reviewed_at_posix, review_id)
  
  reviews[.N]
}

get_clicked_reviewed_ids <- function(con, reviewer) {
  reviewer_name <- trimws(reviewer)
  
  if (!nzchar(reviewer_name)) {
    return(character())
  }
  
  reviews <- read_reviews(con)
  
  if (nrow(reviews) == 0) {
    return(character())
  }
  
  reviews <- reviews[trimws(get("reviewer")) == reviewer_name]
  
  if (nrow(reviews) == 0) {
    return(character())
  }
  
  reviews[, reviewed_at_posix := as.POSIXct(reviewed_at, tz = Sys.timezone())]
  setorder(reviews, id_tp, reviewed_at_posix, review_id)
  
  latest <- reviews[, .SD[.N], by = id_tp]
  
  latest[
    !is.na(suppressWarnings(as.numeric(clicked_dlmo_h))),
    id_tp
  ]
}

make_case_choices <- function(con, reviewer) {
  ids <- dlmo_review_data$id_tp
  clicked_ids <- get_clicked_reviewed_ids(con, reviewer)
  
  labels <- ifelse(
    ids %in% clicked_ids,
    paste0("✓ ", ids),
    ids
  )
  
  stats::setNames(ids, labels)
}