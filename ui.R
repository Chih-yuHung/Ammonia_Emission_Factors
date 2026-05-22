# Shiny UI layout only. Shared choices and helper functions are defined in server.R.

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
        border-radius: 10px;
        padding: 16px;
        margin-bottom: 16px;
        box-shadow: 0 1px 3px rgba(0, 0, 0, 0.05);
      }
      .summary-card {
        background: linear-gradient(135deg, #f5f1de 0%, #eef3e6 100%);
        border: 1px solid #d6dccd;
        border-radius: 10px;
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
          radioButtons(
            inputId = "housing_ct_basis",
            label = "Climate CT basis",
            choices = c("Wet" = "wet", "Dry" = "dry"),
            selected = "wet",
            inline = TRUE
          ),
          tags$div(class = "help-note", textOutput("housing_factor_note"))
        ),
        div(
          class = "panel-card",
          h4("Temperature"),
          radioButtons(
            inputId = "housing_temp_mode",
            label = "Temperature entry mode",
            choices = c("Annual temperature" = "annual", "12 monthly temperatures" = "monthly"),
            selected = "monthly"
          ),
          conditionalPanel(
            condition = "input.housing_temp_mode == 'annual'",
            numericInput("housing_annual_temp", "Annual temperature (deg C)", value = NA_real_, width = "100%")
          ),
          conditionalPanel(
            condition = "input.housing_temp_mode == 'monthly'",
            month_input_grid("housing_temp", make_month_input)
          )
        ),
        div(
          class = "panel-card",
          h4("Total N Input"),
          radioButtons(
            inputId = "total_n_mode",
            label = "Total N entry mode",
            choices = c("Annual Total N" = "annual", "12 monthly Total N values" = "monthly"),
            selected = "annual"
          ),
          conditionalPanel(
            condition = "input.total_n_mode == 'annual'",
            numericInput("annual_total_n", "Annual Total N (kg N)", value = NA_real_, min = 0, width = "100%"),
            tags$p(class = "help-note", "Annual Total N is evenly distributed across months for monthly calculations.")
          ),
          conditionalPanel(
            condition = "input.total_n_mode == 'monthly'",
            month_input_grid("total_n", make_nonnegative_month_input)
          )
        )
      ),
      conditionalPanel(
        condition = "input.calculation_section == 'storage'",
        div(
          class = "panel-card",
          h4("Storage Animal Parameters"),
          selectInput(
            inputId = "storage_animal_type",
            label = "Animal type",
            choices = storage_animal_defaults$animal_type,
            selected = storage_animal_defaults$animal_type[1]
          ),
          tags$div(class = "help-note", strong("Temperature coefficient (CT): "), textOutput("storage_ct_display", inline = TRUE)),
          tags$div(class = "help-note", strong("IPCC storage group: "), textOutput("storage_ipcc_group_display", inline = TRUE)),
          numericInput("storage_tref", "Tref (deg C)", value = 15, width = "100%"),
          numericInput("storage_eftref", "EFTref (%)", value = storage_animal_defaults$eftref[1], min = 0, width = "100%"),
          tags$p(class = "help-note", "Auto-filled from animal type, but you can edit it.")
        ),
        div(
          class = "panel-card",
          h4("Average Monthly Manure Temperature (deg C)"),
          month_input_grid("storage_temp", make_month_input)
        ),
        div(
          class = "panel-card",
          h4("Storage Assumptions"),
          selectInput("storage_type", "Storage type", choices = storage_choices, selected = "slurry_tank"),
          selectInput("cover_condition", "Cover/crust condition", choices = cover_choices, selected = "uncovered"),
          tags$div(class = "help-note", strong("Storage/cover multiplier: "), textOutput("storage_multiplier_display", inline = TRUE)),
          numericInput("removal_efficiency", "Removal efficiency (%)", value = 95, min = 0, max = 100, width = "100%"),
          selectInput(
            inputId = "removal_months",
            label = "Removal months",
            choices = stats::setNames(as.character(seq_along(month_labels)), month_labels),
            selected = character(0),
            multiple = TRUE
          )
        ),
        div(
          class = "panel-card",
          h4(tagList("TAN Input for ", "NH", tags$sub("3"), " Emissions")),
          radioButtons(
            inputId = "tan_mode",
            label = "TAN entry mode",
            choices = c("Annual TAN" = "annual", "12 monthly TANs" = "monthly"),
            selected = "annual"
          ),
          conditionalPanel(
            condition = "input.tan_mode == 'annual'",
            numericInput("annual_tan", "Annual TAN generated (kg N)", value = NA_real_, min = 0, width = "100%"),
            tags$p(class = "help-note", "Annual TAN is evenly distributed across the 12 months.")
          ),
          conditionalPanel(
            condition = "input.tan_mode == 'monthly'",
            month_input_grid("tan", make_nonnegative_month_input)
          ),
          tags$p(class = "help-note", "Removed TAN exits this storage model and is not counted as later NH3 emission.")
        )
      ),
      div(
        class = "panel-card",
        h4("Batch Monthly Temperatures"),
        fileInput("temperature_csv", "Upload temperature scenario CSV", accept = ".csv"),
        downloadButton("download_temperature_template", "Download Template", class = "download-button"),
        tags$p(class = "help-note", "Uploaded scenarios are included in the downloaded CSV results but are not plotted.")
      ),
      div(
        class = "panel-card action-row",
        h4("Actions"),
        actionButton("plot_button", "Plot", class = "plot-button"),
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
            span(class = "summary-label", HTML("Annual NH<sub>3</sub> Emission")),
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
      div(
        class = "panel-card",
        plotOutput("ef_plot", height = "460px"),
        tags$div(class = "results-caption", textOutput("plot_caption"))
      ),
      div(
        class = "panel-card",
        h4("Primary Scenario Results"),
        tableOutput("results_table")
      )
    )
  )
)

