library(shiny)
library(data.table)
library(ggplot2)
library(DBI)
library(RSQLite)

source("plot.r")
source("db.r")

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
        selected = "Medium",
        inline = TRUE
      ),
      
      textAreaInput("notes", "Notes", height = "100px"),
      
      verbatimTextOutput("clicked_time"),
      
      actionButton("clear_click", "Clear clicked DLMO"),
      br(), br(),
      
      actionButton("submit", "Submit & Next", class = "btn-primary")
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
  
  current_row <- reactive({
    dlmo_check[id_tp == input$case_id]
  })
  
  observeEvent(input$case_id, {
    clicked_dlmo(NA_real_)
    updateRadioButtons(session, "decision", selected = "Unsure")
    updateTextAreaInput(session, "notes", value = "")
  })
  
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
  
  output$dlmo_plot <- renderPlot({
    plot_dlmo_app(
      current_row(),
      clicked_dlmo = clicked_dlmo()
    )
  })
  
  observeEvent(input$submit, {
    d <- current_row()
    
    decision_to_save <- if (!is.na(clicked_dlmo())) {
      "Use clicked DLMO"
    } else {
      input$decision
    }
    
    save_review(
      con,
      data.frame(
        reviewer = input$reviewer,
        reviewed_at = as.character(Sys.time()),
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
    
    i <- match(input$case_id, dlmo_check$id_tp)
    next_i <- ifelse(i < nrow(dlmo_check), i + 1, i)
    
    updateSelectInput(
      session,
      "case_id",
      selected = dlmo_check$id_tp[next_i]
    )
    
    clicked_dlmo(NA_real_)
    updateRadioButtons(session, "decision", selected = "Unsure")
    updateTextAreaInput(session, "notes", value = "")
  })
  
  session$onSessionEnded(function() {
    if (DBI::dbIsValid(con)) {
      DBI::dbDisconnect(con)
    }
  })
}

shinyApp(ui, server)