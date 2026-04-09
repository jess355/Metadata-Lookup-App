# app.R — Genomic Metadata Shiny App
# Connects to genomic_metadata.sqlite and provides searchable views of
# Patients, Office Visits, and Samples.

library(shiny)
library(DBI)
library(RSQLite)
library(DT)
library(dplyr)

# ── Database helper ────────────────────────────────────────────────────────────
db_path <- "data/genomic_metadata.sqlite"

get_con <- function() dbConnect(SQLite(), db_path)

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("Genomic Metadata Browser"),
  tags$head(tags$style(HTML("
    .well { background: #f8f9fa; }
    h4 { color: #2c3e50; margin-top: 0; }
    .info-box { background:#e8f4f8; border-left:4px solid #3498db;
                padding:10px 14px; margin-bottom:12px; border-radius:3px; }
  "))),

  tabsetPanel(
    id = "main_tabs",

    # ── Patients tab ────────────────────────────────────────────────────────
    tabPanel("Patients",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Search Patients"),
            textInput("pt_id",   "Patient ID",  placeholder = "e.g. P2362"),
            textInput("pt_name", "Name",         placeholder = "partial name"),
            selectInput("pt_sex", "Sex",
                        choices = c("All", "M", "F"), selected = "All"),
            selectInput("pt_ancestry", "Ancestry",
                        choices = c("All"), selected = "All"),
            actionButton("pt_search", "Search", class = "btn-primary btn-sm"),
            actionButton("pt_reset",  "Reset",  class = "btn-sm")
          )
        ),
        column(9,
          div(class = "info-box",
            textOutput("pt_summary")
          ),
          DTOutput("pt_table")
        )
      )
    ),

    # ── Office Visits tab ───────────────────────────────────────────────────
    tabPanel("Office Visits",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Search Visits"),
            textInput("ov_pt_id", "Patient ID", placeholder = "e.g. P2362"),
            dateRangeInput("ov_dates", "Encounter Date Range",
                           start = "2020-01-01", end = Sys.Date() + 365),
            actionButton("ov_search", "Search", class = "btn-primary btn-sm"),
            actionButton("ov_reset",  "Reset",  class = "btn-sm")
          )
        ),
        column(9,
          div(class = "info-box", textOutput("ov_summary")),
          DTOutput("ov_table")
        )
      )
    ),

    # ── Samples tab ─────────────────────────────────────────────────────────
    tabPanel("Samples",
      br(),
      fluidRow(
        column(3,
          wellPanel(
            h4("Search Samples"),
            textInput("sm_id",     "Sample ID",   placeholder = "e.g. S_98154"),
            textInput("sm_pt_id",  "Patient ID",  placeholder = "e.g. P2362"),
            selectInput("sm_tissue", "Tissue Type",
                        choices = c("All"), selected = "All"),
            selectInput("sm_assay", "Assay Type",
                        choices = c("All"), selected = "All"),
            actionButton("sm_search", "Search", class = "btn-primary btn-sm"),
            actionButton("sm_reset",  "Reset",  class = "btn-sm")
          )
        ),
        column(9,
          div(class = "info-box", textOutput("sm_summary")),
          DTOutput("sm_table")
        )
      )
    )
  )
)

# ── Server ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  con <- get_con()
  onStop(function() dbDisconnect(con))

  # Pre-load full tables once
  all_patients <- dbReadTable(con, "Patients")
  all_visits   <- dbReadTable(con, "Office_Visits")
  all_samples  <- dbReadTable(con, "Samples")

  # Populate dynamic filter choices
  updateSelectInput(session, "pt_ancestry",
    choices = c("All", sort(unique(na.omit(all_patients$Ancestry)))))
  updateSelectInput(session, "sm_tissue",
    choices = c("All", sort(unique(na.omit(all_samples$Tissue_Type)))))
  updateSelectInput(session, "sm_assay",
    choices = c("All", sort(unique(na.omit(all_samples$Assay_Type)))))

  # ── Patients ──────────────────────────────────────────────────────────────
  pt_data <- reactiveVal(all_patients)

  observeEvent(input$pt_search, {
    df <- all_patients
    if (nzchar(input$pt_id))
      df <- df[grepl(input$pt_id, df$Patient_ID, ignore.case = TRUE), ]
    if (nzchar(input$pt_name))
      df <- df[grepl(input$pt_name, df$Full_Name, ignore.case = TRUE), ]
    if (input$pt_sex != "All")
      df <- df[df$Sex == input$pt_sex, ]
    if (input$pt_ancestry != "All")
      df <- df[!is.na(df$Ancestry) & df$Ancestry == input$pt_ancestry, ]
    pt_data(df)
  })

  observeEvent(input$pt_reset, {
    updateTextInput(session, "pt_id",   value = "")
    updateTextInput(session, "pt_name", value = "")
    updateSelectInput(session, "pt_sex",      selected = "All")
    updateSelectInput(session, "pt_ancestry", selected = "All")
    pt_data(all_patients)
  })

  output$pt_summary <- renderText({
    sprintf("Showing %d of %d patients", nrow(pt_data()), nrow(all_patients))
  })

  output$pt_table <- renderDT({
    datatable(pt_data(), rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE))
  })

  # ── Office Visits ─────────────────────────────────────────────────────────
  ov_data <- reactiveVal(all_visits)

  observeEvent(input$ov_search, {
    df <- all_visits
    if (nzchar(input$ov_pt_id))
      df <- df[grepl(input$ov_pt_id, df$Patient_ID, ignore.case = TRUE), ]
    df <- df[!is.na(df$Encounter_Date) &
               as.Date(df$Encounter_Date) >= input$ov_dates[1] &
               as.Date(df$Encounter_Date) <= input$ov_dates[2], ]
    ov_data(df)
  })

  observeEvent(input$ov_reset, {
    updateTextInput(session, "ov_pt_id", value = "")
    updateDateRangeInput(session, "ov_dates",
                         start = "2020-01-01", end = Sys.Date() + 365)
    ov_data(all_visits)
  })

  output$ov_summary <- renderText({
    sprintf("Showing %d of %d visits", nrow(ov_data()), nrow(all_visits))
  })

  output$ov_table <- renderDT({
    datatable(ov_data(), rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE))
  })

  # ── Samples ───────────────────────────────────────────────────────────────
  sm_data <- reactiveVal({
    # Enrich samples with Patient_ID via visits
    all_samples %>%
      left_join(all_visits %>% select(Visit_ID, Patient_ID), by = "Visit_ID") %>%
      select(Sample_ID, Patient_ID, Visit_ID, Tissue_Type, Assay_Type,
             Timepoint_Relative, Genome_Build, Coverage_Mean, File_Path)
  })

  enriched_samples <- reactive({
    all_samples %>%
      left_join(all_visits %>% select(Visit_ID, Patient_ID), by = "Visit_ID") %>%
      select(Sample_ID, Patient_ID, Visit_ID, Tissue_Type, Assay_Type,
             Timepoint_Relative, Genome_Build, Coverage_Mean, File_Path)
  })

  observeEvent(input$sm_search, {
    df <- enriched_samples()
    if (nzchar(input$sm_id))
      df <- df[grepl(input$sm_id, df$Sample_ID, ignore.case = TRUE), ]
    if (nzchar(input$sm_pt_id))
      df <- df[grepl(input$sm_pt_id, df$Patient_ID, ignore.case = TRUE), ]
    if (input$sm_tissue != "All")
      df <- df[!is.na(df$Tissue_Type) & df$Tissue_Type == input$sm_tissue, ]
    if (input$sm_assay != "All")
      df <- df[!is.na(df$Assay_Type) & df$Assay_Type == input$sm_assay, ]
    sm_data(df)
  })

  observeEvent(input$sm_reset, {
    updateTextInput(session, "sm_id",    value = "")
    updateTextInput(session, "sm_pt_id", value = "")
    updateSelectInput(session, "sm_tissue", selected = "All")
    updateSelectInput(session, "sm_assay",  selected = "All")
    sm_data(enriched_samples())
  })

  output$sm_summary <- renderText({
    sprintf("Showing %d of %d samples", nrow(sm_data()), nrow(all_samples))
  })

  output$sm_table <- renderDT({
    datatable(sm_data(), rownames = FALSE, filter = "top",
              options = list(pageLength = 15, scrollX = TRUE))
  })
}

shinyApp(ui, server)
