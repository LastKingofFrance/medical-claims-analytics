# app.R - Comprehensive Medical Claims Analytics Dashboard
# Check and install missing packages
required_packages <- c("shiny", "shinythemes", "shinydashboard", "shinyWidgets", 
                       "DT", "dplyr", "ggplot2", "plotly", "lubridate", 
                       "scales", "tidyr", "readxl", "RSQLite", "pool", "stringr",
                       "dbplyr")  # ← ADD THIS LINE

# Install any missing packages
missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
if(length(missing_packages)) {
  install.packages(missing_packages)
}

# Now load all packages
lapply(required_packages, library, character.only = TRUE)

# Increase Shiny's file size limit
options(shiny.maxRequestSize = 100*1024^2) # 100MB limit

# Initialize SQLite database (file-based for portability)
if (!file.exists("medical_claims.db")) {
  con <- dbConnect(RSQLite::SQLite(), "medical_claims.db")
  dbExecute(con, "
    CREATE TABLE medical_claims (
      Payrun_Date DATE,
      Group_Name TEXT,
      Provider TEXT,
      Sub_limit TEXT,
      Sub_Category TEXT,
      Claim_Amount REAL,
      Month INTEGER,
      Quarter INTEGER,
      Half INTEGER,
      Year INTEGER,
      upload_timestamp DATETIME
    )
  ")
  dbDisconnect(con)
}

# Create database pool
pool <- dbPool(
  drv = RSQLite::SQLite(),
  dbname = "medical_claims.db"
)

# ---- HELPER FUNCTIONS ----
get_period_data <- function(pool, period_type, period_value, year_value) {
  query <- tbl(pool, "medical_claims") %>%
    filter(Year == year_value)
  
  if (period_type != "Year") {
    period_col <- switch(period_type,
                         "Month" = "Month",
                         "Quarter" = "Quarter",
                         "Half" = "Half")
    query <- query %>% filter(!!sym(period_col) == period_value)
  }
  
  filtered_data <- query %>% collect()
  
  if (nrow(filtered_data) > 0) {
    filtered_data %>%
      mutate(
        Payrun_Date = as.Date(Payrun_Date, origin = "1970-01-01"), # Convert back from numeric
        ComparisonYear = as.character(year_value),
        PeriodLabel = paste0(period_type, "_", period_value, "_", year_value),
        .before = 1
      )
  } else {
    filtered_data
  }
}

build_comparison_table <- function(data, group_var) {
  if (nrow(data) == 0) return(data.frame())
  
  required_cols <- c(group_var, "PeriodLabel", "Claim_Amount")
  missing_cols <- setdiff(required_cols, colnames(data))
  if (length(missing_cols) > 0) {
    stop(paste("Missing required columns:", paste(missing_cols, collapse = ", ")))
  }
  
  agg <- data %>%
    group_by(!!sym(group_var), PeriodLabel) %>%
    summarise(
      incidences = n(),
      total_amount = sum(Claim_Amount, na.rm = TRUE),
      cost_per_claim = ifelse(incidences == 0, NA_real_, total_amount / incidences),
      .groups = "drop"
    )
  
  if (nrow(agg) == 0) return(data.frame())
  
  comparison_table <- agg %>%
    pivot_wider(
      names_from = PeriodLabel,
      values_from = c(incidences, total_amount, cost_per_claim),
      names_prefix = "",
      names_sep = " - "
    )
  
  return(comparison_table)
}

format_kes <- function(x) {
  paste0("KSh ", format(round(x), big.mark = ","))
}

clean_names <- function(names, preserve_col = NULL) {
  if (!is.null(preserve_col)) {
    preserve_index <- which(names == preserve_col)
    if (length(preserve_index)) {
      original_name <- names[preserve_index]
    }
  }
  
  names <- gsub("_", " ", names)
  names <- gsub("incidences", "Cases", names)
  names <- gsub("total amount", "Amount", names)
  names <- gsub("cost per claim", "Cost/Claim", names)
  names <- gsub("Quarter", "Q", names)
  names <- gsub("Half", "H", names)
  names <- gsub("Month", "M", names)
  names <- gsub("Year", "Y", names)
  
  if (!is.null(preserve_col)) {
    if (length(preserve_index)) {
      names[preserve_index] <- original_name
    }
  }
  
  names
}

# ---- UI ----
ui <- dashboardPage(
  dashboardHeader(title = "Medical Claims Analytics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Management", tabName = "data_management", icon = icon("database")),
      menuItem("Comparison Dashboard", tabName = "comparison", icon = icon("chart-bar")),
      menuItem("Drill-down Explorer", tabName = "drill_down", icon = icon("search")),
      menuItem("Data Quality", tabName = "debug", icon = icon("bug"))
    )
  ),
  dashboardBody(
    tabItems(
      # Data Management Tab
      tabItem(
        tabName = "data_management",
        h2("Data Management"),
        fluidRow(
          box(
            title = "Upload Data", status = "primary", solidHeader = TRUE, width = 6,
            fileInput("file_upload", "Upload Medical Data Excel File",
                      accept = c(".xlsx", ".xls")),
            helpText("Maximum file size: 100MB. Required columns:",
                     tags$ul(
                       tags$li("Payrun Date"),
                       tags$li("Group Name"),
                       tags$li("Provider"),
                       tags$li("Sub limit"),
                       tags$li("Sub_Category"),
                       tags$li("Claim_Amount")
                     )),
            actionButton("process", "Upload & Process Data"),
            hr(),
            downloadButton("download_report", "Download Data Quality Report")
          ),
          box(
            title = "Database Information", status = "info", solidHeader = TRUE, width = 6,
            htmlOutput("db_info"),
            hr(),
            actionButton("clear_data", "Clear All Data", icon = icon("trash"), 
                         style = "color: white; background-color: #dd4b39; border-color: #d73925")
          )
        ),
        fluidRow(
          box(
            title = "Data Quality Check", status = "warning", solidHeader = TRUE, width = 12,
            column(6,
                   h4("Column Verification"),
                   verbatimTextOutput("col_check"),
                   h4("Date Range Summary"),
                   verbatimTextOutput("date_summary")
            ),
            column(6,
                   h4("Existing Data Overlap"),
                   verbatimTextOutput("overlap_check"),
                   h4("Sample Data Preview"),
                   DTOutput("sample_data_preview")
            )
          )
        )
      ),
      
      # Comparison Tab
      tabItem(
        tabName = "comparison",
        h2("Comparison Dashboard"),
        tabsetPanel(
          tabPanel("Controls",
                   box(
                     title = "Analysis Parameters", status = "primary", solidHeader = TRUE, width = 12,
                     fluidRow(
                       column(6,
                              selectInput("period_type", "Period Type",
                                          choices = c("Month", "Quarter", "Half", "Year"),
                                          selected = "Quarter"),
                              numericInput("year_1", "Base Year:", value = 2024, min = 2010, max = 2025),
                              numericInput("period_1", "Base Period:", value = 1, min = 1, max = 4)
                       ),
                       column(6,
                              numericInput("year_2", "Comparison Year:", value = 2024, min = 2010, max = 2025),
                              numericInput("period_2", "Comparison Period:", value = 2, min = 1, max = 4),
                              selectInput("category", "Analysis Category",
                                          choices = c("Provider", "Group_Name", "Sub_limit", "Sub_Category"),
                                          selected = "Provider")
                       )
                     ),
                     fluidRow(
                       column(6,
                              selectInput("metric", "Primary Metric",
                                          choices = c("Total Amount" = "total_amount",
                                                      "Incidences" = "incidences",
                                                      "Cost Per Claim" = "cost_per_claim",
                                                      "% Change" = "pct_change"),
                                          selected = "total_amount")
                       ),
                       column(6,
                              numericInput("top_n", "Top N Items:", value = 10, min = 1, max = 50),
                              actionBttn("update", "Update Analysis", 
                                         style = "gradient", color = "primary")
                       )
                     )
                   )
          ),
          tabPanel("Visualizations",
                   fluidRow(
                     box(
                       title = "Top Changes", status = "info", solidHeader = TRUE, width = 12,
                       plotlyOutput("comparisonPlot", height = "500px")
                     )
                   ),
                   fluidRow(
                     box(
                       title = "Detailed Comparison", status = "info", solidHeader = TRUE, width = 12,
                       DTOutput("comparisonTable"),
                       downloadButton("downloadData", "Download Data")
                     )
                   )
          ),
          tabPanel("Executive Summary",
                   box(
                     title = "Key Findings", status = "success", solidHeader = TRUE, width = 12,
                     htmlOutput("comparisonSummary"),
                     hr(),
                     htmlOutput("metricSummary")
                   )
          )
        )
      ),
      
      # Drill-down Explorer Tab
      tabItem(
        tabName = "drill_down",
        h2("Drill-down Explorer"),
        fluidRow(
          column(3,
                 box(
                   title = "Analysis Focus", status = "primary", solidHeader = TRUE, width = 12,
                   selectInput("drill_category", "Primary Category:",
                               choices = c("Provider", "Group_Name", "Sub_limit", "Sub_Category"),
                               selected = "Provider"),
                   selectizeInput("drill_item", "Select Item:",
                                  choices = NULL,
                                  options = list(placeholder = 'Type to search...',
                                                 maxOptions = 1000)),
                   selectInput("drill_by_category", "Drill-down by:",
                               choices = c("Provider", "Group_Name", "Sub_limit", "Sub_Category"),
                               selected = "Sub_Category"),
                   selectInput("drill_metric", "Analysis Metric:",
                               choices = c("Total Amount" = "total_amount",
                                           "Incidences" = "incidences",
                                           "Cost Per Claim" = "cost_per_claim"),
                               selected = "total_amount"),
                   sliderInput("top_items", "Show Top:", 
                               min = 5, max = 20, value = 10)
                 )
          ),
          column(9,
                 tabsetPanel(
                   tabPanel("Trend Analysis", 
                            plotlyOutput("trendPlot", height = "500px")),
                   tabPanel("Composition",
                            fluidRow(
                              column(6, plotlyOutput("currentCompositionPlot")),
                              column(6, plotlyOutput("comparisonCompositionPlot"))
                            )
                   ),
                   tabPanel("Change Analysis", 
                            DTOutput("detailedChangesTable"))
                 )
          )
        )
      ),
      
      # Debug Tab
      tabItem(
        tabName = "debug",
        h2("Data Quality Checks"),
        fluidRow(
          box(
            title = "Data Summary", status = "warning", solidHeader = TRUE, width = 12,
            htmlOutput("data_quality")
          )
        ),
        fluidRow(
          box(
            title = "Debug Output", status = "danger", solidHeader = TRUE, width = 12,
            verbatimTextOutput("debug")
          )
        )
      )
    )
  )
)

# ---- SERVER ----
server <- function(input, output, session) {
  # Reactive values
  rv <- reactiveValues(
    pool = pool,
    data_quality_info = NULL,
    processed_data = NULL
  )
  
  # Close database connection when session ends
  session$onSessionEnded(function() {
    poolClose(pool)
  })
  
  # Database info
  output$db_info <- renderUI({
    total_records <- tbl(rv$pool, "medical_claims") %>% count() %>% pull(n)
    date_range <- tbl(rv$pool, "medical_claims") %>% 
      summarise(min_date = min(Payrun_Date, na.rm = TRUE), 
                max_date = max(Payrun_Date, na.rm = TRUE)) %>% collect()
    
    tags$div(
      tags$p(tags$b("Total records:"), total_records),
      tags$p(tags$b("Date range:"), 
             ifelse(total_records > 0, 
                    paste(date_range$min_date, "to", date_range$max_date),
                    "No data")),
      tags$p(tags$b("Database file:"), "medical_claims.db"),
      tags$p(tags$b("Size:"), 
             ifelse(file.exists("medical_claims.db"),
                    paste(round(file.info("medical_claims.db")$size / 1024 / 1024, 2), "MB"),
                    "Not found"))
    )
  })
  
  # Clear all data
  observeEvent(input$clear_data, {
    showModal(modalDialog(
      title = "Confirm Data Deletion",
      "Are you sure you want to delete ALL data from the database? This action cannot be undone.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear", "Delete All Data", class = "btn-danger")
      )
    ))
  })
  
  observeEvent(input$confirm_clear, {
    tryCatch({
      dbExecute(rv$pool, "DELETE FROM medical_claims")
      removeModal()
      showNotification("All data has been deleted.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error deleting data:", e$message), type = "error")
    })
  })
  
  # Process uploaded file
  observeEvent(input$process, {
    req(input$file_upload)
    
    showModal(modalDialog("Processing data...", footer = NULL))
    
    tryCatch({
      # Read and process the file
      raw_data <- readxl::read_excel(input$file_upload$datapath)
      
      target_columns <- c("Payrun Date", "Group Name", "Provider", 
                          "Sub limit", "Sub_Category", "Claim_Amount")
      
      # Data Quality Checks
      quality_info <- list()
      
      # 1. Check for missing columns
      missing_cols <- setdiff(target_columns, colnames(raw_data))
      quality_info$missing_cols <- if (length(missing_cols) > 0) {
        paste("Missing columns:", paste(missing_cols, collapse = ", "))
      } else {
        "All required columns present"
      }
      
      # 2. Check date range and quality
      if ("Payrun Date" %in% colnames(raw_data)) {
        dates <- try(as.Date(raw_data$`Payrun Date`))
        if (inherits(dates, "try-error") || any(is.na(dates))) {
          quality_info$date_quality <- "Invalid dates found in Payrun Date"
        } else {
          date_range <- range(dates, na.rm = TRUE)
          quality_info$date_quality <- paste(
            "Date range from", date_range[1], "to", date_range[2],
            "(", length(unique(dates)), "unique dates)")
        }
      } else {
        quality_info$date_quality <- "No Payrun Date column found"
      }
      
      # Only proceed if all columns are present
      if (length(missing_cols) == 0) {
        # Process data (maintaining original column names)
        # In the data processing section, add this after reading the Excel file:
        # Convert dates properly for SQLite storage
        clean_data <- raw_data %>%
          select(all_of(target_columns)) %>%
          mutate(
            Payrun_Date = as.Date(`Payrun Date`),
            # Convert to numeric for SQLite storage (optional, but helps with consistency)
            Payrun_Date_numeric = as.numeric(Payrun_Date),
            Month = month(Payrun_Date),
            Quarter = quarter(Payrun_Date),
            Half = ifelse(Month <= 6, 1, 2),
            Year = year(Payrun_Date),
            upload_timestamp = Sys.time()
          ) %>%
          select(-`Payrun Date`) %>%
          rename(
            Group_Name = `Group Name`,
            Sub_limit = `Sub limit`
          ) %>%
          # Use numeric date for SQLite storage
          select(-Payrun_Date) %>%
          rename(Payrun_Date = Payrun_Date_numeric)
        
        # 3. Check for existing data overlap
        max_date <- max(clean_data$Payrun_Date, na.rm = TRUE)
        existing_dates <- tbl(rv$pool, "medical_claims") %>%
          filter(Payrun_Date <= max_date) %>%
          select(Payrun_Date) %>%
          distinct() %>%
          collect()
        
        quality_info$overlap <- if (nrow(existing_dates) > 0) {
          paste("Overlap detected:", nrow(existing_dates), 
                "existing dates will be replaced")
        } else {
          "No overlap with existing data"
        }
        
        # Write to database (replace overlapping data)
        if (nrow(existing_dates) > 0) {
          # Use transaction to ensure data integrity
          dbExecute(rv$pool, 'BEGIN')
          dbExecute(rv$pool, 
                    paste0('DELETE FROM medical_claims WHERE Payrun_Date IN (',
                           paste0('"', existing_dates$Payrun_Date, '"', collapse = ','), 
                           ')'))
          dbWriteTable(rv$pool, "medical_claims", clean_data, append = TRUE)
          dbExecute(rv$pool, 'COMMIT')
        } else {
          dbWriteTable(rv$pool, "medical_claims", clean_data, append = TRUE)
        }
        
        rv$processed_data <- clean_data
      }
      
      rv$data_quality_info <- quality_info
      removeModal()
      
    }, error = function(e) {
      removeModal()
      showModal(modalDialog(paste("Error:", e$message), easyClose = TRUE))
    })
  })
  
  # Data quality outputs
  output$col_check <- renderText({
    req(rv$data_quality_info)
    rv$data_quality_info$missing_cols
  })
  
  output$date_summary <- renderText({
    req(rv$data_quality_info)
    rv$data_quality_info$date_quality
  })
  
  output$overlap_check <- renderText({
    req(rv$data_quality_info)
    rv$data_quality_info$overlap %||% "Upload data to check for overlaps"
  })
  
  # Sample data display
  output$sample_data_preview <- renderDT({
    if (!is.null(rv$processed_data)) {
      data <- rv$processed_data
    } else {
      data <- tbl(rv$pool, "medical_claims") %>% 
        head(100) %>% 
        collect()
    }
    
    if (nrow(data) > 0) {
      datatable(head(data, 10), options = list(scrollX = TRUE))
    } else {
      datatable(data.frame(Message = "No data available"), options = list(dom = 't'))
    }
  })
  
  # Data quality report download
  output$download_report <- downloadHandler(
    filename = function() {
      paste("data_quality_report_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      req(rv$data_quality_info)
      report <- c(
        "MEDICAL CLAIMS DATA QUALITY REPORT",
        "=================================",
        "",
        paste("Generated on:", Sys.time()),
        "",
        "1. COLUMN VERIFICATION",
        rv$data_quality_info$missing_cols,
        "",
        "2. DATE RANGE SUMMARY",
        rv$data_quality_info$date_quality,
        "",
        "3. EXISTING DATA OVERLAP CHECK",
        rv$data_quality_info$overlap %||% "No overlap check performed"
      )
      writeLines(report, file)
    }
  )
  
  # Update period input limits
  observeEvent(input$period_type, {
    max_val <- switch(input$period_type,
                      "Month" = 12,
                      "Quarter" = 4,
                      "Half" = 2,
                      "Year" = 1)
    updateNumericInput(session, "period_1", max = max_val)
    updateNumericInput(session, "period_2", max = max_val)
  })
  
  # Update drill-down selector
  observe({
    req(rv$pool, input$drill_category)
    choices_data <- tbl(rv$pool, "medical_claims") %>%
      select(!!sym(input$drill_category)) %>%
      distinct() %>%
      collect()
    
    updateSelectizeInput(session, "drill_item", 
                         choices = sort(choices_data[[1]]),
                         server = TRUE)
  })
  
  # Reactive data processing
  analysis_data <- eventReactive(input$update, {
    req(input$year_1, input$year_2)
    
    withProgress(message = "Processing data...", {
      d1 <- get_period_data(rv$pool, input$period_type, input$period_1, input$year_1)
      d2 <- get_period_data(rv$pool, input$period_type, input$period_2, input$year_2)
      combined <- bind_rows(d1, d2)
      
      if (nrow(combined) == 0) {
        showNotification("No data found for selected parameters", type = "warning")
      }
      
      combined
    })
  })
  
  # Create comparison table
  full_comparison_table <- reactive({
    req(analysis_data())
    full_table <- build_comparison_table(analysis_data(), input$category)
    
    # Clean column names for display
    names(full_table) <- clean_names(names(full_table), preserve_col = input$category)
    full_table
  })
  
  # Get top N items for comparison
  comparison_table_data <- reactive({
    full_table <- full_comparison_table()
    
    validate(
      need(nrow(full_table) > 0, "No data available for selected time periods.")
    )
    
    metric_label <- switch(input$metric,
                           "total_amount" = "Amount",
                           "incidences" = "Cases",
                           "cost_per_claim" = "Cost/Claim",
                           "pct_change" = "pct_change")
    
    baseline_col_name <- paste(metric_label, "-", clean_names(paste0(input$period_type, "_", input$period_1, "_", input$year_1)))
    comparison_col_name <- paste(metric_label, "-", clean_names(paste0(input$period_type, "_", input$period_2, "_", input$year_2)))
    
    validate(
      need(baseline_col_name %in% colnames(full_table), 
           paste("Baseline data not available for", baseline_col_name)),
      need(comparison_col_name %in% colnames(full_table),
           paste("Comparison data not available for", comparison_col_name))
    )
    
    df_with_metrics <- full_table %>%
      mutate(
        Difference = .data[[comparison_col_name]] - .data[[baseline_col_name]],
        `Percentage Change` = case_when(
          .data[[baseline_col_name]] == 0 ~ NA_real_,
          is.infinite(Difference/.data[[baseline_col_name]]) ~ NA_real_,
          TRUE ~ Difference/.data[[baseline_col_name]]
        ),
        .after = 1
      )
    
    metric_to_sort <- switch(input$metric,
                             "total_amount" = "Difference",
                             "incidences" = "Difference",
                             "cost_per_claim" = "Difference",
                             "pct_change" = "Percentage Change")
    
    df_with_metrics %>%
      arrange(desc(abs(.data[[metric_to_sort]]))) %>%
      head(input$top_n)
  })
  
  output$comparisonPlot <- renderPlotly({
    df <- comparison_table_data()
    
    validate(
      need(!is.null(df) && nrow(df) > 0,
           "No data available for selected comparison.")
    )
    
    y_var <- switch(input$metric,
                    "total_amount" = "Difference",
                    "incidences" = "Difference",
                    "cost_per_claim" = "Difference",
                    "pct_change" = "Percentage Change")
    
    y_label <- switch(input$metric,
                      "total_amount" = "Amount Change (KSh)",
                      "incidences" = "Case Change",
                      "cost_per_claim" = "Cost/Claim Change (KSh)",
                      "pct_change" = "Percentage Change")
    
    # Create proper period labels
    period1_label <- paste0(
      switch(input$period_type,
             "Month" = "M",
             "Quarter" = "Q",
             "Half" = "H",
             "Year" = "Y"),
      input$period_1, " ", input$year_1
    )
    
    period2_label <- paste0(
      switch(input$period_type,
             "Month" = "M",
             "Quarter" = "Q",
             "Half" = "H",
             "Year" = "Y"),
      input$period_2, " ", input$year_2
    )
    
    # Create row-specific hover text
    df$hover_text <- paste0(
      input$category, ": ", df[[input$category]], "<br>",
      period1_label, " Value: ",
      ifelse(
        input$metric == "pct_change",
        percent(
          df[[paste0("Amount - ", clean_names(paste0(input$period_type, "_", input$period_1, "_", input$year_1)))]]
          / ifelse(
            input$metric == "total_amount",
            1,
            df[[paste0("Cases - ", clean_names(paste0(input$period_type, "_", input$period_1, "_", input$year_1)))]]
          ),
          accuracy = 0.1
        ),
        format_kes(
          df[[paste0("Amount - ", clean_names(paste0(input$period_type, "_", input$period_1, "_", input$year_1)))]]
        )
      ),
      "<br>",
      period2_label, " Value: ",
      ifelse(
        input$metric == "pct_change",
        percent(
          df[[paste0("Amount - ", clean_names(paste0(input$period_type, "_", input$period_2, "_", input$year_2)))]]
          / ifelse(
            input$metric == "total_amount",
            1,
            df[[paste0("Cases - ", clean_names(paste0(input$period_type, "_", input$period_2, "_", input$year_2)))]]
          ),
          accuracy = 0.1
        ),
        format_kes(
          df[[paste0("Amount - ", clean_names(paste0(input$period_type, "_", input$period_2, "_", input$year_2)))]]
        )
      ),
      "<br>",
      y_label, ": ",
      ifelse(
        input$metric == "pct_change",
        percent(df[[y_var]], accuracy = 0.1),
        format_kes(df[[y_var]])
      )
    )
    
    p <- ggplot(df, aes(
      x = reorder(!!sym(input$category), .data[[y_var]]),
      y = .data[[y_var]],
      fill = .data[[y_var]] > 0,
      text = .data[["hover_text"]]
    )) +
      geom_col() +
      coord_flip() +
      labs(
        title = paste("Top", input$top_n, clean_names(input$category), "by", y_label),
        x = "", y = y_label
      ) +
      scale_fill_manual(values = c("TRUE" = "#1f77b4", "FALSE" = "#d62728")) +
      theme_minimal() +
      theme(legend.position = "none")
    
    if (input$metric == "pct_change") {
      p <- p + scale_y_continuous(labels = percent)
    } else if (input$metric %in% c("total_amount", "cost_per_claim")) {
      p <- p + scale_y_continuous(labels = function(x) format_kes(x))
    } else {
      p <- p + scale_y_continuous(labels = comma)
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        margin = list(l = 150),
        yaxis = list(
          tickformat = ",.0f",
          tickprefix = "KSh "
        )
      )
  })
  
  # Comparison table
  output$comparisonTable <- renderDT({
    df <- comparison_table_data()
    
    validate(
      need(!is.null(df) && nrow(df) > 0, 
           "No data available for selected comparison.")
    )
    
    dt <- datatable(df, 
                    rownames = FALSE,
                    extensions = 'Buttons',
                    options = list(
                      scrollX = TRUE,
                      dom = 'Bfrtip',
                      buttons = c('copy', 'csv', 'excel'),
                      pageLength = 10
                    )) %>%
      formatCurrency(columns = grep("Amount", names(df)), currency = "KSh ", digits = 0) %>%
      formatRound(columns = grep("Cost/Claim", names(df)), digits = 0) %>%
      formatPercentage(columns = "Percentage Change", digits = 1)
    
    if ("Cases" %in% names(df)) {
      dt <- dt %>% formatRound(columns = grep("Cases", names(df)), digits = 0)
    }
    
    dt
  })
  
  # Enhanced summary section
  output$comparisonSummary <- renderUI({
    full_df <- full_comparison_table()
    
    validate(
      need(nrow(full_df) > 0, 
           "No data available for selected time periods.")
    )
    
    amount_baseline_col <- clean_names(paste0("total_amount - ", input$period_type, "_", input$period_1, "_", input$year_1))
    amount_comparison_col <- clean_names(paste0("total_amount - ", input$period_type, "_", input$period_2, "_", input$year_2))
    
    cases_baseline_col <- clean_names(paste0("incidences - ", input$period_type, "_", input$period_1, "_", input$year_1))
    cases_comparison_col <- clean_names(paste0("incidences - ", input$period_type, "_", input$period_2, "_", input$year_2))
    
    validate(
      need(amount_baseline_col %in% colnames(full_df), "Amount baseline data not available"),
      need(amount_comparison_col %in% colnames(full_df), "Amount comparison data not available"),
      need(cases_baseline_col %in% colnames(full_df), "Cases baseline data not available"),
      need(cases_comparison_col %in% colnames(full_df), "Cases comparison data not available")
    )
    
    total_amount_baseline <- sum(full_df[[amount_baseline_col]], na.rm = TRUE)
    total_amount_comparison <- sum(full_df[[amount_comparison_col]], na.rm = TRUE)
    amount_pct_change <- ifelse(total_amount_baseline == 0, 
                                ifelse(total_amount_comparison == 0, 0, Inf),
                                (total_amount_comparison - total_amount_baseline) / total_amount_baseline)
    
    total_cases_baseline <- sum(full_df[[cases_baseline_col]], na.rm = TRUE)
    total_cases_comparison <- sum(full_df[[cases_comparison_col]], na.rm = TRUE)
    cases_pct_change <- ifelse(total_cases_baseline == 0, 
                               ifelse(total_cases_comparison == 0, 0, Inf),
                               (total_cases_comparison - total_cases_baseline) / total_cases_baseline)
    
    avg_cost_baseline <- ifelse(total_cases_baseline > 0, 
                                total_amount_baseline / total_cases_baseline, 
                                0)
    avg_cost_comparison <- ifelse(total_cases_comparison > 0, 
                                  total_amount_comparison / total_cases_comparison, 
                                  0)
    
    df_top5 <- comparison_table_data() %>% head(5)
    top_contributors <- paste(df_top5[[input$category]], collapse = ", ")
    
    period1_label <- paste0(input$period_type, " ", input$period_1, " ", input$year_1)
    period2_label <- paste0(input$period_type, " ", input$period_2, " ", input$year_2)
    
    amount_change_text <- ifelse(
      is.infinite(amount_pct_change), "&#8734;",
      paste0(
        ifelse(amount_pct_change >= 0, "+", ""),
        percent(amount_pct_change, accuracy = 0.1)
      )
    )
    
    cases_change_text <- ifelse(
      is.infinite(cases_pct_change), "&#8734;",
      paste0(
        ifelse(cases_pct_change >= 0, "+", ""),
        percent(cases_pct_change, accuracy = 0.1)
      )
    )
    
    HTML(paste0(
      "<h4>Period Comparison: ", period1_label,
      " vs ", period2_label, "</h4>",
      "<ul>",
      "<li><b>Total Amount:</b> ", format_kes(total_amount_baseline), " &#8594; ", format_kes(total_amount_comparison), 
      " (", amount_change_text, ")</li>",
      "<li><b>Total Cases:</b> ", comma(total_cases_baseline), " &#8594; ", comma(total_cases_comparison),
      " (", cases_change_text, ")</li>",
      "<li><b>Average Cost/Claim:</b> ", format_kes(avg_cost_baseline), 
      " &#8594; ", format_kes(avg_cost_comparison), "</li>",
      "</ul>",
      "<h4>Key Drivers:</h4>",
      "<p>", top_contributors, "</p>"
    ))
  })
  
  # Drill-down data
  drill_down_data <- reactive({
    req(input$drill_item, input$drill_category)
    tbl(rv$pool, "medical_claims") %>%
      filter(!!sym(input$drill_category) == input$drill_item) %>%
      collect()
  })
  
  # Drill-down analysis table
  drill_down_table <- reactive({
    req(drill_down_data(), input$drill_by_category)
    
    # Get data for both periods
    d1 <- get_period_data(rv$pool, input$period_type, input$period_1, input$year_1) %>%
      filter(!!sym(input$drill_category) == input$drill_item)
    d2 <- get_period_data(rv$pool, input$period_type, input$period_2, input$year_2) %>%
      filter(!!sym(input$drill_category) == input$drill_item)
    
    combined <- bind_rows(d1, d2)
    
    validate(
      need(nrow(combined) > 0, "No drill-down data found for selected time periods.")
    )
    
    # Create comparison table
    comparison_table <- combined %>%
      group_by(!!sym(input$drill_by_category), PeriodLabel) %>%
      summarise(
        incidences = n(),
        total_amount = sum(Claim_Amount, na.rm = TRUE),
        cost_per_claim = ifelse(incidences == 0, NA_real_, total_amount / incidences),
        .groups = "drop"
      ) %>%
      pivot_wider(
        names_from = PeriodLabel,
        values_from = c(incidences, total_amount, cost_per_claim),
        names_glue = "{.value}_{PeriodLabel}"
      )
    
    baseline_label <- paste0(input$period_type, "_", input$period_1, "_", input$year_1)
    comparison_label <- paste0(input$period_type, "_", input$period_2, "_", input$year_2)
    
    # Ensure all required columns exist
    required_cols <- c(
      paste0("incidences_", baseline_label),
      paste0("incidences_", comparison_label),
      paste0("total_amount_", baseline_label),
      paste0("total_amount_", comparison_label),
      paste0("cost_per_claim_", baseline_label),
      paste0("cost_per_claim_", comparison_label)
    )
    
    # Add missing columns with NA values
    for (col in required_cols) {
      if (!col %in% colnames(comparison_table)) {
        comparison_table[[col]] <- NA_real_
      }
    }
    
    # Calculate differences and percentage changes
    result <- comparison_table %>%
      mutate(
        incidences_diff = .data[[paste0("incidences_", comparison_label)]] - 
          .data[[paste0("incidences_", baseline_label)]],
        amount_diff = .data[[paste0("total_amount_", comparison_label)]] - 
          .data[[paste0("total_amount_", baseline_label)]],
        cost_per_claim_diff = .data[[paste0("cost_per_claim_", comparison_label)]] - 
          .data[[paste0("cost_per_claim_", baseline_label)]],
        incidences_pct_change = ifelse(.data[[paste0("incidences_", baseline_label)]] == 0,
                                       ifelse(.data[[paste0("incidences_", comparison_label)]] == 0, 0, Inf),
                                       incidences_diff / .data[[paste0("incidences_", baseline_label)]]),
        amount_pct_change = ifelse(.data[[paste0("total_amount_", baseline_label)]] == 0,
                                   ifelse(.data[[paste0("total_amount_", comparison_label)]] == 0, 0, Inf),
                                   amount_diff / .data[[paste0("total_amount_", baseline_label)]]),
        cost_per_claim_pct_change = ifelse(.data[[paste0("cost_per_claim_", baseline_label)]] == 0,
                                           ifelse(.data[[paste0("cost_per_claim_", comparison_label)]] == 0, 0, Inf),
                                           cost_per_claim_diff / .data[[paste0("cost_per_claim_", baseline_label)]])
      )
    
    # Remove rows with all NA values in the comparison columns
    result <- result %>%
      filter(!(is.na(.data[[paste0("total_amount_", baseline_label)]]) & 
                 is.na(.data[[paste0("total_amount_", comparison_label)]])))
    
    # IMPORTANT: Add a check for an empty result data frame
    if (nrow(result) == 0) {
      return(NULL)
    }
    
    result
  })
  
  # Drill-down trend plot
  output$trendPlot <- renderPlotly({
    req(drill_down_data())
    
    trend_data <- drill_down_data() %>%
      mutate(
        Payrun_Date = as.Date(Payrun_Date),  # Ensure it's Date type
        MonthYear = floor_date(Payrun_Date, "month")
      ) %>%
      group_by(MonthYear) %>%
      summarise(
        total_amount = sum(Claim_Amount, na.rm = TRUE),
        incidences = n(),
        cost_per_claim = ifelse(incidences == 0, NA_real_, total_amount / incidences),
        .groups = "drop"
      )
    
    validate(
      need(nrow(trend_data) > 0, "No trend data available")
    )
    
    metric_col <- switch(input$drill_metric,
                         "total_amount" = "total_amount",
                         "incidences" = "incidences",
                         "cost_per_claim" = "cost_per_claim")
    
    y_label <- switch(input$drill_metric,
                      "total_amount" = "Total Amount (KSh)",
                      "incidences" = "Number of Cases",
                      "cost_per_claim" = "Cost per Claim (KSh)")
    
    p <- ggplot(trend_data, aes(x = MonthYear, y = .data[[metric_col]])) +
      geom_line(color = "#1f77b4", size = 1) +
      geom_point(color = "#1f77b4", size = 2) +
      labs(
        title = paste("Trend Analysis for", input$drill_item),
        x = "Month", y = y_label
      ) +
      theme_minimal()
    
    if (input$drill_metric %in% c("total_amount", "cost_per_claim")) {
      p <- p + scale_y_continuous(labels = function(x) format_kes(x))
    } else {
      p <- p + scale_y_continuous(labels = comma)
    }
    
    ggplotly(p) %>%
      layout(
        xaxis = list(
          tickformat = "%b %Y"
        )
      )
  })

  # Current Composition Plot
  output$currentCompositionPlot <- renderPlotly({
    req(drill_down_table(), input$drill_metric)
    
    metric_col <- switch(input$drill_metric,
                         "total_amount" = "total_amount",
                         "incidences" = "incidences",
                         "cost_per_claim" = "cost_per_claim")
    
    comp_data <- drill_down_data() %>%
      group_by(!!sym(input$drill_by_category)) %>%
      summarise(
        total_amount = sum(Claim_Amount, na.rm = TRUE),
        incidences = n(),
        cost_per_claim = ifelse(incidences == 0, NA_real_, total_amount / incidences),
        .groups = "drop"
      ) %>%
      arrange(desc(.data[[metric_col]])) %>%
      head(input$top_items)
    
    # Create "Others" category
    if (nrow(comp_data) > 0) {
      top_data <- head(comp_data, 9)
      if (nrow(comp_data) > 9) {
        others <- data.frame(
          category = "Others",
          value = sum(tail(comp_data, -9)[[metric_col]], na.rm = TRUE)
        )
        names(others) <- c(input$drill_by_category, metric_col)
        comp_data <- bind_rows(top_data, others)
      }
    }
    
    validate(need(nrow(comp_data) > 0, "No composition data available"))
    
    # Create better title
    period_label <- paste0(
      switch(input$period_type,
             "Month" = "M",
             "Quarter" = "Q", 
             "Half" = "H",
             "Year" = "Y"),
      input$period_1, " ", input$year_1
    )
    
    title <- paste(period_label, input$drill_by_category, "Composition")
    
    # Create pie chart - FIXED: removed !!sym() from plot_ly
    p <- plot_ly(comp_data, 
                 labels = ~get(input$drill_by_category), 
                 values = ~get(metric_col),
                 type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 hoverinfo = 'text',
                 text = ~paste0(
                   get(input$drill_by_category), 
                   ": ", 
                   ifelse(input$drill_metric == "total_amount", format_kes(get(metric_col)),
                          ifelse(input$drill_metric == "incidences", comma(get(metric_col)),
                                 format_kes(get(metric_col))))
                 )) %>%
      layout(title = title,
             showlegend = TRUE)
    
    p
  })
  
  # Comparison Composition Plot
  # Comparison Composition Plot
  output$comparisonCompositionPlot <- renderPlotly({
    req(drill_down_table(), input$drill_metric)
    
    metric_col <- switch(input$drill_metric,
                         "total_amount" = "total_amount",
                         "incidences" = "incidences", 
                         "cost_per_claim" = "cost_per_claim")
    
    # Get the correct column name for comparison period
    comparison_label <- paste0(input$period_type, "_", input$period_2, "_", input$year_2)
    comparison_col <- paste0(metric_col, "_", comparison_label)
    
    # Check if the comparison column exists
    validate(
      need(comparison_col %in% colnames(drill_down_table()),
           paste("Comparison data not available for", comparison_label))
    )
    
    comp_data <- drill_down_table() %>%
      select(!!sym(input$drill_by_category), all_of(comparison_col)) %>%
      filter(!is.na(.data[[comparison_col]]), .data[[comparison_col]] > 0) %>%
      arrange(desc(.data[[comparison_col]])) %>%
      head(input$top_items)
    
    validate(
      need(nrow(comp_data) > 0, 
           paste("No valid data available for comparison period:", comparison_label))
    )
    
    # Create "Others" category
    if (nrow(comp_data) > 9) {
      top_data <- head(comp_data, 9)
      others_sum <- sum(tail(comp_data, -9)[[comparison_col]], na.rm = TRUE)
      if (others_sum > 0) {
        others_row <- data.frame(
          category = "Others",
          value = others_sum
        )
        names(others_row) <- c(input$drill_by_category, comparison_col)
        comp_data <- bind_rows(top_data, others_row)
      } else {
        comp_data <- top_data
      }
    }
    
    # Create better title
    period_label <- paste0(
      switch(input$period_type,
             "Month" = "M",
             "Quarter" = "Q",
             "Half" = "H", 
             "Year" = "Y"),
      input$period_2, " ", input$year_2
    )
    
    title <- paste(period_label, input$drill_by_category, "Composition")
    
    # Format values for hover text
    comp_data$formatted_value <- if (input$drill_metric == "total_amount") {
      format_kes(comp_data[[comparison_col]])
    } else if (input$drill_metric == "incidences") {
      format(comp_data[[comparison_col]], big.mark = ",")
    } else {
      format_kes(comp_data[[comparison_col]])
    }
    
    # Create pie chart - FIXED: removed !!sym() from plot_ly
    p <- plot_ly(comp_data, 
                 labels = ~get(input$drill_by_category), 
                 values = ~get(comparison_col),
                 type = 'pie',
                 textposition = 'inside',
                 textinfo = 'label+percent',
                 hoverinfo = 'text',
                 text = ~paste0(
                   get(input$drill_by_category), 
                   ": ", 
                   formatted_value
                 )) %>%
      layout(title = title,
             showlegend = TRUE)
    
    p
  })
  
  # Detailed changes table
  output$detailedChangesTable <- renderDT({
    req(drill_down_table())
    
    dt_data <- drill_down_table() %>%
      select(
        !!sym(input$drill_by_category),
        contains("incidences_"),
        contains("total_amount_"),
        contains("cost_per_claim_"),
        incidences_diff,
        amount_diff,
        cost_per_claim_diff,
        incidences_pct_change,
        amount_pct_change,
        cost_per_claim_pct_change
      )
    
    datatable(dt_data, 
              rownames = FALSE,
              extensions = 'Buttons',
              options = list(
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel'),
                pageLength = 10
              )) %>%
      formatCurrency(columns = grep("total_amount|amount", colnames(dt_data)), currency = "KSh ", digits = 0) %>%
      formatCurrency(columns = grep("cost_per_claim", colnames(dt_data)), currency = "KSh ", digits = 0) %>%
      formatRound(columns = grep("incidences", colnames(dt_data)), digits = 0) %>%
      formatPercentage(columns = grep("pct_change", colnames(dt_data)), digits = 1)
  })
  
  # Data quality output
  output$data_quality <- renderUI({
    total_records <- tbl(rv$pool, "medical_claims") %>% count() %>% pull(n)
    # In the data_quality output section, replace the date_range calculation:
    date_range <- tbl(rv$pool, "medical_claims") %>% 
      summarise(min_date = min(Payrun_Date, na.rm = TRUE), 
                max_date = max(Payrun_Date, na.rm = TRUE)) %>% collect() %>%
      mutate(across(c(min_date, max_date), ~as.Date(.x, origin = "1970-01-01")))
    
    unique_counts <- tbl(rv$pool, "medical_claims") %>%
      summarise(
        groups = n_distinct(Group_Name),
        providers = n_distinct(Provider),
        sub_limits = n_distinct(Sub_limit),
        sub_categories = n_distinct(Sub_Category)
      ) %>% collect()
    
    HTML(paste0(
      "<h4>Database Summary</h4>",
      "<ul>",
      "<li><b>Total Records:</b> ", format(total_records, big.mark = ","), "</li>",
      "<li><b>Date Range:</b> ", date_range$min_date, " to ", date_range$max_date, "</li>",
      "<li><b>Unique Groups:</b> ", unique_counts$groups, "</li>",
      "<li><b>Unique Providers:</b> ", unique_counts$providers, "</li>",
      "<li><b>Unique Sub-limits:</b> ", unique_counts$sub_limits, "</li>",
      "<li><b>Unique Sub-categories:</b> ", unique_counts$sub_categories, "</li>",
      "</ul>"
    ))
  })
  
  # Debug output
  output$debug <- renderPrint({
    req(rv$pool)
    
    # Sample some data for debugging
    sample_data <- tbl(rv$pool, "medical_claims") %>% 
      head(5) %>% 
      collect()
    
    cat("Sample Data (first 5 rows):\n")
    print(sample_data)
    cat("\n\nColumn Names:\n")
    print(colnames(sample_data))
    cat("\n\nData Types:\n")
    print(sapply(sample_data, class))
  })
}

# Run the application
shinyApp(ui = ui, server = server)