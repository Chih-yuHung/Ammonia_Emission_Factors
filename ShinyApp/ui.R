# Shiny UI layout only. Shared choices and helper functions are defined in server.R.

library(shiny)

month_labels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

make_month_input <- function(id, label) {
  numericInput(
    inputId = id,
    label = label,
    value = NA_real_,
    min = -50,
    width = "100%"
  )
}

month_input_grid <- function(prefix, input_fun) {
  fluidRow(
    column(
      width = 4,
      input_fun(paste0(prefix, "_jan"), "Jan"),
      input_fun(paste0(prefix, "_apr"), "Apr"),
      input_fun(paste0(prefix, "_jul"), "Jul"),
      input_fun(paste0(prefix, "_oct"), "Oct")
    ),
    column(
      width = 4,
      input_fun(paste0(prefix, "_feb"), "Feb"),
      input_fun(paste0(prefix, "_may"), "May"),
      input_fun(paste0(prefix, "_aug"), "Aug"),
      input_fun(paste0(prefix, "_nov"), "Nov")
    ),
    column(
      width = 4,
      input_fun(paste0(prefix, "_mar"), "Mar"),
      input_fun(paste0(prefix, "_jun"), "Jun"),
      input_fun(paste0(prefix, "_sep"), "Sep"),
      input_fun(paste0(prefix, "_dec"), "Dec")
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        background-color: #f6f6f2;
        color: #1f2a1f;
      }
      .panel-card {
        background: #ffffff;
        border: 1px solid #d8ddd3;
        border-radius: 8px;
        padding: 16px;
        margin-bottom: 16px;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
      }
      .summary-card {
        background: #eef3e6;
        border: 1px solid #d6dccd;
        border-radius: 8px;
        padding: 14px 16px;
        min-height: 100px;
      }
      .summary-label {
        display: block;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.04em;
        text-transform: uppercase;
        color: #53604e;
        margin-bottom: 6px;
      }
      .summary-value {
        font-size: 22px;
        font-weight: 700;
        color: #203127;
      }
      .help-note {
        font-size: 12px;
        color: #5d665b;
      }
      .assumption-note {
        font-size: 13px;
        line-height: 1.4;
        color: #3f4c3f;
        background: #f4f7ef;
        border-left: 4px solid #8fa96f;
        padding: 10px 12px;
        margin-top: 8px;
      }
      .action-row .btn {
        width: 100%;
        margin-bottom: 10px;
      }
      .plot-button {
        background-color: #355f3b;
        border-color: #355f3b;
        color: #ffffff;
      }
      .plot-button:hover,
      .plot-button:focus {
        background-color: #28492d;
        border-color: #28492d;
        color: #ffffff;
      }
      .download-button {
        background-color: #d9e4d2;
        border-color: #c9d6c1;
        color: #213122;
      }
      .results-caption {
        margin-top: 8px;
        color: #4f5c4d;
        font-size: 13px;
      }
    "))
  ),
  titlePanel(tagList("NH", tags$sub("3"), " Emission Factor Calculator")),
  fluidRow(
    column(
      width = 4,
      div(
        class = "panel-card",
        h4("Calculation Section"),
        radioButtons(
          inputId = "calculation_section",
          label = NULL,
          choices = c("Housing" = "housing", "Storage" = "storage"),
          selected = "housing"
        )
      ),
      conditionalPanel(
        condition = "input.calculation_section == 'housing'",
        div(
          class = "panel-card",
          h4("Housing Factors"),
          selectInput("housing_animal", "Animal type", choices = NULL),
          selectInput("housing_house", "Housing type", choices = NULL),
          selectInput("housing_manure", "Manure type", choices = NULL),
          selectInput("housing_factor_row", "Correction factor option", choices = NULL),
          tags$div(class = "assumption-note", textOutput("housing_factor_note"))
        ),
        div(
          class = "panel-card",
          h4("Annual Temperature"),
          radioButtons(
            inputId = "housing_temperature_basis",
            label = "Temperature basis",
            choices = c(
              "Outdoor annual temperature" = "outdoor",
              "Indoor annual temperature" = "indoor"
            ),
            selected = "outdoor"
          ),
          numericInput(
            "housing_annual_temp",
            "Annual temperature (deg C)",
            value = NA_real_,
            width = "100%"
          )
        ),
        div(
          class = "panel-card",
          h4(tagList("Annual Total N for ", "NH", tags$sub("3"), " Emissions")),
          numericInput(
            "annual_total_n",
            "Annual Total N (kg N)",
            value = NA_real_,
            min = 0,
            width = "100%"
          ),
          tags$p(
            class = "help-note",
            "Leave blank to calculate the EF only."
          )
        )
      ),
      conditionalPanel(
        condition = "input.calculation_section == 'storage'",
        div(
          class = "panel-card",
          h4("Storage Factors"),
          selectInput("storage_animal", "Animal type", choices = NULL),
          selectInput("storage_type", "Storage type", choices = NULL),
          selectInput("storage_manure", "Manure type", choices = NULL),
          selectInput("storage_factor_row", "Correction factor option", choices = NULL),
          tags$div(class = "assumption-note", textOutput("storage_factor_note"))
        ),
        div(
          class = "panel-card",
          h4("Monthly Air Temperatures (deg C)"),
          month_input_grid("storage_temp", make_month_input)
        ),
        div(
          class = "panel-card",
          h4(tagList("Annual TAN for ", "NH", tags$sub("3"), " Emissions")),
          numericInput(
            "annual_tan",
            "Annual TAN generated (kg N)",
            value = NA_real_,
            min = 0,
            width = "100%"
          ),
          tags$p(
            class = "help-note",
            "Annual TAN is split evenly across months in the three-year storage simulation. Leave blank to calculate EFs only."
          )
        ),
        div(
          class = "panel-card",
          h4("Batch Monthly Temperatures"),
          fileInput("temperature_csv", "Upload temperature scenario CSV", accept = ".csv"),
          downloadButton("download_temperature_template", "Download Template", class = "download-button"),
          tags$p(
            class = "help-note",
            "CSV format: Scenario, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec."
          )
        )
      ),
      div(
        class = "panel-card action-row",
        h4("Actions"),
        actionButton("plot_button", "Calculate", class = "plot-button"),
        uiOutput("download_ui")
      )
    ),
    column(
      width = 8,
      fluidRow(
        column(
          width = 4,
          div(
            class = "summary-card",
            span(class = "summary-label", textOutput("summary_ef_label", inline = TRUE)),
            div(class = "summary-value", textOutput("summary_ef", inline = TRUE))
          )
        ),
        column(
          width = 4,
          div(
            class = "summary-card",
            span(class = "summary-label", textOutput("summary_nh3_label", inline = TRUE)),
            div(class = "summary-value", uiOutput("summary_nh3", inline = TRUE))
          )
        ),
        column(
          width = 4,
          div(
            class = "summary-card",
            span(class = "summary-label", textOutput("summary_mass_label", inline = TRUE)),
            div(class = "summary-value", textOutput("summary_mass", inline = TRUE))
          )
        )
      ),
      uiOutput("plot_panel"),
      div(
        class = "panel-card",
        h4("Primary Scenario Results"),
        tableOutput("results_table")
      )
    )
  )
)
