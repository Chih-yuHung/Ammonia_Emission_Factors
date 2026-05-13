library(shiny)

animal_defaults <- data.frame(
  animal_type = c(
    "DAIRY_CATTLE",
    "BEEF_CATTLE",
    "OTHER_CATTLE",
    "BUFFALO_BEEF",
    "SHEEP",
    "PIG"
  ),
  ct = c(1.056, 1.065, 1.062, 1.067, 1.049, 1.036),
  eftref = c(12.2, 10.0, 7.6, 13.4, 25.0, 29.6),
  stringsAsFactors = FALSE
)

month_labels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

make_month_input <- function(id, label) {
  numericInput(
    inputId = id,
    label = label,
    value = NA_real_,
    min = -5,
    width = "100%"
  )
}

month_input_grid <- fluidRow(
  column(
    width = 4,
    make_month_input("temp_jan", "Jan"),
    make_month_input("temp_apr", "Apr"),
    make_month_input("temp_jul", "Jul"),
    make_month_input("temp_oct", "Oct")
  ),
  column(
    width = 4,
    make_month_input("temp_feb", "Feb"),
    make_month_input("temp_may", "May"),
    make_month_input("temp_aug", "Aug"),
    make_month_input("temp_nov", "Nov")
  ),
  column(
    width = 4,
    make_month_input("temp_mar", "Mar"),
    make_month_input("temp_jun", "Jun"),
    make_month_input("temp_sep", "Sep"),
    make_month_input("temp_dec", "Dec")
  )
)

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
  titlePanel(tagList("NH", tags$sub("3"), " Emission Factor Temperature Correction")),
  fluidRow(
    column(
      width = 4,
      div(
        class = "panel-card",
        h4("Animal Parameters"),
        selectInput(
          inputId = "animal_type",
          label = "Animal type",
          choices = animal_defaults$animal_type,
          selected = animal_defaults$animal_type[1]
        ),
        tags$div(
          class = "help-note",
          strong("Temperature coefficient (CT): "),
          textOutput("ct_display", inline = TRUE)
        ),
        numericInput(
          inputId = "tref",
          label = "Tref (deg C)",
          value = 15,
          width = "100%"
        ),
        numericInput(
          inputId = "eftref",
          label = "EFTref (%)",
          value = animal_defaults$eftref[1],
          min = 0,
          width = "100%"
        ),
        tags$p(
          class = "help-note",
          "Auto-filled from animal type, but you can edit it."
        )
      ),
      div(
        class = "panel-card",
        h4("Average Monthly Manure Temperature (deg C)"),
        month_input_grid
      ),
      div(
        class = "panel-card",
        h4(tagList("Optional ", "NH", tags$sub("3"), " Emission Estimate")),
        numericInput(
          inputId = "monthly_tan",
          label = "Monthly TAN (kg N)",
          value = NA_real_,
          min = 0,
          width = "100%"
        ),
        tags$p(
          class = "help-note",
          tagList(
            "If provided, monthly ",
            "NH", tags$sub("3"),
            " emission is calculated as TAN x EF / 100."
          )
        ),
        tags$p(
          class = "help-note",
          "The same TAN value is applied to all 12 months."
        )
      ),
      div(
        class = "panel-card action-row",
        h4("Actions"),
        actionButton(
          inputId = "plot_button",
          label = "Plot",
          class = "plot-button"
        ),
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
            span(class = "summary-label", "Animal Type"),
            div(class = "summary-value", textOutput("summary_animal", inline = TRUE))
          )
        ),
        column(
          width = 4,
          div(
            class = "summary-card",
            span(class = "summary-label", "Annual Average EF"),
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
        )
      ),
      div(
        class = "panel-card",
        plotOutput("ef_plot", height = "460px"),
        tags$div(
          class = "results-caption",
          "Solid line: EF (%). Dashed line: manure temperature (deg C)."
        )
      ),
      div(
        class = "panel-card",
        h4(tagList("Monthly Results")),
        tableOutput("results_table")
      )
    )
  )
)

server <- function(input, output, session) {
  has_results <- reactiveVal(FALSE)

  selected_defaults <- reactive({
    animal_defaults[animal_defaults$animal_type == input$animal_type, , drop = FALSE]
  })

  output$ct_display <- renderText({
    sprintf("%.3f", selected_defaults()$ct)
  })

  observeEvent(input$animal_type, {
    updateNumericInput(session, "eftref", value = selected_defaults()$eftref)
  }, ignoreInit = TRUE)

  calculation_results <- eventReactive(input$plot_button, {
    temps <- c(
      input$temp_jan, input$temp_feb, input$temp_mar, input$temp_apr,
      input$temp_may, input$temp_jun, input$temp_jul, input$temp_aug,
      input$temp_sep, input$temp_oct, input$temp_nov, input$temp_dec
    )

    validate(
      need(!any(is.na(temps)), "Enter numeric manure temperatures for all 12 months."),
      need(!is.na(input$tref), "Enter a numeric Tref value."),
      need(!is.na(input$eftref), "Enter a numeric EFTref value."),
      need(input$eftref >= 0, "EFTref must be non-negative.")
    )

    if (!is.na(input$monthly_tan)) {
      validate(
        need(input$monthly_tan >= 0, "Monthly TAN must be non-negative.")
      )
    }

    ct_value <- selected_defaults()$ct
    ef_values <- input$eftref * (ct_value ^ (temps - input$tref))
    annual_ef <- mean(ef_values)
    monthly_tan <- if (is.na(input$monthly_tan)) rep(NA_real_, 12) else rep(input$monthly_tan, 12)
    monthly_nh3 <- if (is.na(input$monthly_tan)) rep(NA_real_, 12) else monthly_tan * ef_values / 100
    annual_nh3 <- if (is.na(input$monthly_tan)) NA_real_ else sum(monthly_nh3)

    results_df <- data.frame(
      Month = month_labels,
      AnimalType = rep(input$animal_type, 12),
      Temperature_C = temps,
      Tref_C = rep(input$tref, 12),
      CT = rep(ct_value, 12),
      EFTref_percent = rep(input$eftref, 12),
      EF_percent = ef_values,
      Monthly_TAN = monthly_tan,
      NH3_emission = monthly_nh3,
      stringsAsFactors = FALSE
    )

    summary_row <- data.frame(
      Month = "Annual Summary",
      AnimalType = input$animal_type,
      Temperature_C = NA_real_,
      Tref_C = input$tref,
      CT = ct_value,
      EFTref_percent = input$eftref,
      EF_percent = annual_ef,
      Monthly_TAN = if (is.na(input$monthly_tan)) NA_real_ else input$monthly_tan,
      NH3_emission = annual_nh3,
      stringsAsFactors = FALSE
    )

    list(
      animal_type = input$animal_type,
      ct = ct_value,
      annual_ef = annual_ef,
      annual_nh3 = annual_nh3,
      monthly_tan_input = input$monthly_tan,
      results_df = results_df,
      export_df = rbind(results_df, summary_row)
    )
  })

  observeEvent(calculation_results(), {
    has_results(TRUE)
  })

  output$download_ui <- renderUI({
    if (!has_results()) {
      tags$button(
        "Download CSV",
        type = "button",
        class = "btn download-button",
        disabled = "disabled"
      )
    } else {
      downloadButton(
        outputId = "download_results",
        label = "Download CSV",
        class = "download-button"
      )
    }
  })

  output$summary_animal <- renderText({
    req(input$plot_button > 0)
    results <- calculation_results()
    results$animal_type
  })

  output$summary_ef <- renderText({
    req(input$plot_button > 0)
    results <- calculation_results()
    sprintf("%.2f%%", results$annual_ef)
  })

  output$summary_nh3 <- renderUI({
    req(input$plot_button > 0)
    results <- calculation_results()
    if (is.na(results$annual_nh3)) {
      span("Not provided")
    } else {
      HTML(sprintf("%.2f kg NH<sub>3</sub>-N", results$annual_nh3))
    }
  })

  output$ef_plot <- renderPlot({
    req(input$plot_button > 0)
    results <- calculation_results()
    df <- results$results_df

    ef_values <- df$EF_percent
    temp_values <- df$Temperature_C
    x_vals <- seq_along(month_labels)

    ef_range <- range(ef_values, na.rm = TRUE)
    temp_range <- range(temp_values, na.rm = TRUE)

    if (diff(ef_range) == 0) {
      ef_range <- ef_range + c(-1, 1)
    }

    if (diff(temp_range) == 0) {
      temp_range <- temp_range + c(-1, 1)
    }

    ef_padding <- diff(ef_range) * 0.12
    temp_breaks <- pretty(temp_range)
    temp_to_ef <- function(x) {
      (x - temp_range[1]) / diff(temp_range) * diff(ef_range) + ef_range[1]
    }

    par(mar = c(4.5, 4.8, 4.5, 4.8))
    plot(
      x_vals,
      ef_values,
      type = "o",
      pch = 16,
      lwd = 2.8,
      col = "#2E5E4E",
      xaxt = "n",
      xlab = "Month",
      ylab = "EF (%)",
      ylim = c(ef_range[1] - ef_padding * 0.25, ef_range[2] + ef_padding),
      main = expression("Monthly " * NH[3] * " Emission Factor")
    )
    axis(1, at = x_vals, labels = month_labels)

    lines(
      x_vals,
      temp_to_ef(temp_values),
      type = "o",
      pch = 1,
      lwd = 2,
      lty = 2,
      col = "#C06A2B"
    )
    axis(
      side = 4,
      at = temp_to_ef(temp_breaks),
      labels = round(temp_breaks, 1),
      col.axis = "#C06A2B"
    )
    mtext("Manure temperature (deg C)", side = 4, line = 3, col = "#C06A2B")

    usr <- par("usr")
    text(
      x = usr[2],
      y = usr[4],
      labels = sprintf("Annual average EF = %.2f%%", results$annual_ef),
      adj = c(1, 1),
      cex = 0.95,
      font = 2,
      col = "#203127"
    )

    legend(
      "topleft",
      legend = c("EF (%)", "Manure temperature (deg C)"),
      col = c("#2E5E4E", "#C06A2B"),
      lty = c(1, 2),
      lwd = c(2.8, 2),
      pch = c(16, 1),
      bty = "n"
    )
    box()
  })

  output$results_table <- renderTable({
    req(input$plot_button > 0)
    results <- calculation_results()
    df <- results$export_df

    if (is.na(results$monthly_tan_input)) {
      df$Monthly_TAN <- NULL
      df$NH3_emission <- NULL
    }

    display_names <- names(df)
    display_names[display_names == "Temperature_C"] <- "Temperature (deg C)"
    display_names[display_names == "Tref_C"] <- "Tref (deg C)"
    display_names[display_names == "EFTref_percent"] <- "EFTref (%)"
    display_names[display_names == "EF_percent"] <- "EF (%)"
    display_names[display_names == "Monthly_TAN"] <- "Monthly TAN (kg N)"
    display_names[display_names == "NH3_emission"] <- "NH<sub>3</sub> emission (kg NH<sub>3</sub>-N)"
    names(df) <- display_names

    numeric_cols <- vapply(df, is.numeric, logical(1))
    df[numeric_cols] <- lapply(df[numeric_cols], function(col) round(col, 3))
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s", sanitize.text.function = function(x) x)

  output$download_results <- downloadHandler(
    filename = function() {
      paste0("nh3_ef_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      results <- calculation_results()
      export_df <- results$export_df
      names(export_df)[names(export_df) == "Temperature_C"] <- "Temperature_deg_C"
      names(export_df)[names(export_df) == "Tref_C"] <- "Tref_deg_C"
      names(export_df)[names(export_df) == "EFTref_percent"] <- "EFTref_percent"
      names(export_df)[names(export_df) == "EF_percent"] <- "EF_percent"
      names(export_df)[names(export_df) == "Monthly_TAN"] <- "Monthly_TAN_kg_N"
      names(export_df)[names(export_df) == "NH3_emission"] <- "NH3_emission_kg_NH3_N"
      write.csv(export_df, file, row.names = FALSE, na = "")
    }
  )
}

shinyApp(ui = ui, server = server)
