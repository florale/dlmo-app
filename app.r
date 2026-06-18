library(shiny)
library(data.table)
library(ggplot2)
library(googlesheets4)

source("plot.r")
source("db.r")

if (!exists("dlmo_check")) {
  dlmo_check <- readRDS("data/dlmo_check.rds")
}

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) y else x
}

# get_latest_review <- function(con, reviewer, id_tp) {
#   reviewer <- trimws(reviewer)
  
#   if (!nzchar(reviewer) || is.null(id_tp) || is.na(id_tp)) {
#     return(NULL)
#   }
  
#   out <- DBI::dbGetQuery(
#     con,
#     "
#     SELECT *
#     FROM reviews
#     WHERE reviewer = ? AND id_tp = ?
#     ORDER BY review_id DESC
#     LIMIT 1
#     ",
#     params = list(reviewer, id_tp)
#   )
  
#   if (nrow(out) == 0) NULL else out
# }

# get_clicked_reviewed_ids <- function(con, reviewer) {
#   reviewer <- trimws(reviewer)
  
#   if (!nzchar(reviewer)) {
#     return(character())
#   }
  
#   out <- DBI::dbGetQuery(
#     con,
#     "
#     SELECT r.id_tp
#     FROM reviews r
#     INNER JOIN (
#       SELECT id_tp, MAX(review_id) AS latest_review_id
#       FROM reviews
#       WHERE reviewer = ?
#       GROUP BY id_tp
#     ) latest
#       ON r.id_tp = latest.id_tp
#      AND r.review_id = latest.latest_review_id
#     WHERE r.clicked_dlmo_h IS NOT NULL
#     ",
#     params = list(reviewer)
#   )
  
#   out$id_tp
# }

# make_case_choices <- function(con, reviewer) {
#   ids <- dlmo_check$id_tp
#   clicked_ids <- get_clicked_reviewed_ids(con, reviewer)
  
#   labels <- ifelse(
#     ids %in% clicked_ids,
#     paste0("✓ ", ids),
#     ids
#   )
  
#   stats::setNames(ids, labels)
# }

ui <- fluidPage(
  titlePanel("DLMO Expert Review App"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("reviewer", "Reviewer name"),
      
      selectInput(
        "case_id",
        "Case",
        choices = dlmo_check$id_tp
      ),
      
      radioButtons(
        "decision",
        "Decision",
        choices = c(
          "Use clicked DLMO",
          "No DLMO",
          "Unsure",
          "Exclude"
        ),
        selected = "Unsure"
      ),
      
      radioButtons(
        "confidence",
        "Confidence",
        choices = c("High", "Medium", "Low"),
        selected = "High",
        inline = TRUE
      ),
      
      textAreaInput("notes", "Notes", height = "100px"),
      
      verbatimTextOutput("clicked_time"),
      verbatimTextOutput("previous_review"),
      
      actionButton("clear_click", "Clear clicked DLMO"),
      br(), br(),
      
      actionButton("submit", "Save & Next", class = "btn-primary"),
      
      br(), br(),
      helpText("Responses are saved only after clicking Save & Next.")
    ),
    
    mainPanel(
      plotOutput(
        "dlmo_plot",
        click = "plot_click",
        height = "650px"
      )
    )
  )
)

server <- function(input, output, session) {
  
  con <- connect_db()
  clicked_dlmo <- reactiveVal(NA_real_)
  previous_review <- reactiveVal(NULL)
  
  current_row <- reactive({
    dlmo_check[id_tp == input$case_id]
  })
  
  refresh_case_dropdown <- function(selected = isolate(input$case_id)) {
    selected <- selected %||% dlmo_check$id_tp[1]
    
    updateSelectInput(
      session,
      "case_id",
      choices = make_case_choices(con, input$reviewer),
      selected = selected
    )
  }
  
  load_previous_submission <- function() {
    req(input$case_id)
    
    reviewer_name <- trimws(input$reviewer)
    
    if (!nzchar(reviewer_name)) {
      previous_review(NULL)
      clicked_dlmo(NA_real_)
      updateRadioButtons(session, "decision", selected = "Unsure")
      updateRadioButtons(session, "confidence", selected = "Medium")
      updateTextAreaInput(session, "notes", value = "")
      return(invisible(NULL))
    }
    
    prev <- get_latest_review(
      con = con,
      reviewer = reviewer_name,
      id_tp = input$case_id
    )
    
    previous_review(prev)
    
    if (is.null(prev)) {
      clicked_dlmo(NA_real_)
      updateRadioButtons(session, "decision", selected = "Unsure")
      updateRadioButtons(session, "confidence", selected = "Medium")
      updateTextAreaInput(session, "notes", value = "")
    } else {
      prev_clicked <- suppressWarnings(as.numeric(prev$clicked_dlmo_h[1]))
      
      if (is.na(prev_clicked)) {
        clicked_dlmo(NA_real_)
      } else {
        clicked_dlmo(prev_clicked)
      }
      
      prev_decision <- prev$decision[1]
      if (!prev_decision %in% c("Use clicked DLMO", "No DLMO", "Unsure", "Exclude")) {
        prev_decision <- "Unsure"
      }
      
      prev_confidence <- prev$confidence[1]
      if (!prev_confidence %in% c("High", "Medium", "Low")) {
        prev_confidence <- "Medium"
      }
      
      updateRadioButtons(session, "decision", selected = prev_decision)
      updateRadioButtons(session, "confidence", selected = prev_confidence)
      updateTextAreaInput(session, "notes", value = prev$notes[1])
    }
    
    invisible(NULL)
  }
  
  observeEvent(input$reviewer, {
    refresh_case_dropdown(selected = isolate(input$case_id))
    load_previous_submission()
  }, ignoreInit = FALSE)
  
  observeEvent(input$case_id, {
    load_previous_submission()
  }, ignoreInit = FALSE)
  
  observeEvent(input$plot_click, {
    clicked_dlmo(input$plot_click$x)
    updateRadioButtons(session, "decision", selected = "Use clicked DLMO")
  })
  
  observeEvent(input$decision, {
    if (!is.na(clicked_dlmo()) && input$decision != "Use clicked DLMO") {
      updateRadioButtons(session, "decision", selected = "Use clicked DLMO")
    }
  }, ignoreInit = TRUE)
  
  observeEvent(input$clear_click, {
    clicked_dlmo(NA_real_)
    updateRadioButtons(session, "decision", selected = "Unsure")
  })
  
  output$clicked_time <- renderText({
    if (is.na(clicked_dlmo())) {
      "Clicked DLMO: none"
    } else {
      paste0(
        "Clicked DLMO: ",
        round(clicked_dlmo(), 3),
        " h = ",
        fmt_time(clicked_dlmo())
      )
    }
  })
  
  output$previous_review <- renderText({
    prev <- previous_review()
    
    if (is.null(prev)) {
      "Previous submission: none for this reviewer and case"
    } else {
      paste0(
        "Previous submission:\n",
        "Reviewed at: ", prev$reviewed_at[1], "\n",
        "Decision: ", prev$decision[1], "\n",
        "Clicked DLMO: ",
        ifelse(
          is.na(prev$clicked_dlmo_h[1]),
          "none",
          prev$clicked_dlmo_clock[1]
        ), "\n",
        "Confidence: ", prev$confidence[1], "\n",
        "Notes: ", prev$notes[1]
      )
    }
  })
  
  output$dlmo_plot <- renderPlot({
    plot_dlmo_app(
      current_row(),
      clicked_dlmo = clicked_dlmo()
    )
  })
  
  observeEvent(input$submit, {
    reviewer_name <- trimws(input$reviewer)
    
    if (!nzchar(reviewer_name)) {
      showNotification(
        "Please enter reviewer name before submitting.",
        type = "error"
      )
      return(NULL)
    }
    
    d <- current_row()
    
    decision_to_save <- if (!is.na(clicked_dlmo())) {
      "Use clicked DLMO"
    } else {
      input$decision
    }
    
    save_review(
      con,
      data.frame(
        reviewer = tolower(reviewer_name),
        reviewed_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
        id_tp = d$id_tp,
        ID = d$ID,
        timepoint = d$timepoint,
        clicked_dlmo_h = clicked_dlmo(),
        clicked_dlmo_clock = ifelse(
          is.na(clicked_dlmo()),
          NA,
          fmt_time(clicked_dlmo())
        ),
        decision = decision_to_save,
        confidence = input$confidence,
        notes = input$notes,
        dlmo_hs = d$dlmo_hs,
        dlmo_fixed_3 = d$dlmo_fixed_3,
        dlmo_fixed_4 = d$dlmo_fixed_4,
        reason_category_revised = d$reason_category_revised
      )
    )
    
    showNotification("Review saved.", type = "message")
    
    i <- match(input$case_id, dlmo_check$id_tp)
    next_i <- ifelse(i < nrow(dlmo_check), i + 1, i)
    next_case <- dlmo_check$id_tp[next_i]
    
    refresh_case_dropdown(selected = next_case)
  })
  
  # session$onSessionEnded(function() {
  #   if (DBI::dbIsValid(con)) {
  #     DBI::dbDisconnect(con)
  #   }
  # })
}

