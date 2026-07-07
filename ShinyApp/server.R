# Shared constants, data loading, calculation helpers, and Shiny server logic.
# Keep NH3 emission model changes here so UI layout stays separate.

library(shiny)
library(readxl)

month_labels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

month_numbers <- seq_along(month_labels)
names(month_numbers) <- month_labels

ef_max <- 1
simulation_years <- 3
storage_default_monthly_tan_kg <- 100
storage_removal_months <- c(month_numbers["Apr"], month_numbers["Oct"])
storage_removal_month_names <- paste(month_labels[storage_removal_months], collapse = ", ")
storage_removal_efficiency <- 0.95

num_or_na <- function(x) {
  suppressWarnings(as.numeric(x))
}

app_file <- function(...) {
  local_path <- file.path(...)
  parent_path <- file.path("..", ...)
  if (file.exists(local_path)) local_path else parent_path
}

clean_text_col <- function(x) {
  x <- trimws(as.character(x))
  x[is.na(x) | x == "NA"] <- ""
  x
}

cap_ef <- function(ef_value) {
  pmin(ef_value, ef_max)
}

load_factor_sheet <- function(sheet, required_cols) {
  factors <- as.data.frame(
    readxl::read_excel(
      app_file("Inputs", "NH3_Correction_Factors.xlsx"),
      sheet = sheet
    ),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  names(factors) <- trimws(names(factors))

  missing_cols <- setdiff(required_cols, names(factors))
  if (length(missing_cols) > 0) {
    stop(
      "Missing ", sheet, "-sheet columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  factors <- factors[required_cols]
  factors$Source_Row <- seq_len(nrow(factors))
  factors
}

load_housing_factors <- function() {
  required_cols <- c(
    "Animal_Type", "Housing", "Manure_type",
    "EFs_TotalN", "T_air", "CT_15", "Note"
  )
  factors <- load_factor_sheet("Housing", required_cols)

  for (col in c("Animal_Type", "Housing", "Manure_type", "Note")) {
    factors[[col]] <- clean_text_col(factors[[col]])
  }
  for (col in c("EFs_TotalN", "T_air", "CT_15")) {
    factors[[col]] <- num_or_na(factors[[col]])
  }

  factors$Selectable <- !is.na(factors$EFs_TotalN) &
    !is.na(factors$T_air) &
    !is.na(factors$CT_15)
  factors
}

load_storage_factors <- function() {
  required_cols <- c(
    "Animal_Type", "Storage", "Manure_type",
    "EFs_TAN", "T_air", "CT"
  )
  factors <- load_factor_sheet("Storage", required_cols)

  for (col in c("Animal_Type", "Storage", "Manure_type")) {
    factors[[col]] <- clean_text_col(factors[[col]])
  }
  for (col in c("EFs_TAN", "T_air", "CT")) {
    factors[[col]] <- num_or_na(factors[[col]])
  }

  factors$Selectable <- !is.na(factors$EFs_TAN) & !is.na(factors$T_air)
  factors
}

housing_factors <- load_housing_factors()
storage_factors <- load_storage_factors()

format_housing_factor_label <- function(row) {
  note_text <- if (nzchar(row$Note)) paste0("; note: ", row$Note) else ""
  sprintf(
    "Row %s: EF %.4f; T_air %.1f C; CT_15 %s%s",
    row$Source_Row,
    row$EFs_TotalN,
    row$T_air,
    ifelse(is.na(row$CT_15), "not applicable", sprintf("%.3f", row$CT_15)),
    note_text
  )
}

format_storage_factor_label <- function(row) {
  sprintf(
    "Row %s: EF %.4f; T_air %.1f C; CT %s",
    row$Source_Row,
    row$EFs_TAN,
    row$T_air,
    ifelse(is.na(row$CT), "not applicable", sprintf("%.3f", row$CT))
  )
}

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

get_month_values <- function(input, prefix) {
  vapply(
    c("jan", "feb", "mar", "apr", "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec"),
    function(month_id) input[[paste0(prefix, "_", month_id)]],
    numeric(1)
  )
}

housing_temperature_group <- function(animal_type) {
  animal_type <- tolower(as.character(animal_type))

  if (grepl("cattle", animal_type)) {
    return("cattle")
  }
  if (grepl("swine", animal_type)) {
    return("swine")
  }
  if (grepl("broiler", animal_type)) {
    return("broiler")
  }
  if (grepl("layer", animal_type)) {
    return("layer")
  }

  stop("No indoor temperature equation defined for animal type: ", animal_type)
}

calculate_indoor_temperature <- function(outdoor_temperature_c, animal_type) {
  group <- housing_temperature_group(animal_type)

  if (identical(group, "cattle")) {
    return(outdoor_temperature_c + 3)
  }

  if (identical(group, "swine")) {
    return(ifelse(
      outdoor_temperature_c <= 0,
      20 + 0.5 * outdoor_temperature_c,
      ifelse(
        outdoor_temperature_c <= 12.5,
        20,
        20 + (outdoor_temperature_c - 12.5)
      )
    ))
  }

  if (identical(group, "broiler")) {
    return(
      2.0e-4 * outdoor_temperature_c^3 +
        1.0e-3 * outdoor_temperature_c^2 +
        2.4e-2 * outdoor_temperature_c +
        22.1
    )
  }

  1.4e-4 * outdoor_temperature_c^3 +
    2.3e-3 * outdoor_temperature_c^2 +
    1.1e-2 * outdoor_temperature_c +
    23.8
}

threshold_from_note <- function(note) {
  note <- as.character(note)
  match_text <- regmatches(
    note,
    regexpr(">[[:space:]]*[-+]?[0-9]*\\.?[0-9]+", note)
  )

  if (length(match_text) == 0 || is.na(match_text) || match_text == "") {
    return(NA_real_)
  }

  as.numeric(gsub(">|[[:space:]]", "", match_text))
}

ct_is_applicable <- function(ct, corrected_temperature_c, note) {
  if (is.na(ct)) {
    return(FALSE)
  }

  threshold_c <- threshold_from_note(note)
  if (is.na(threshold_c)) {
    return(TRUE)
  }

  corrected_temperature_c > threshold_c
}

read_temperature_upload <- function(file_info) {
  if (is.null(file_info)) {
    return(NULL)
  }

  uploaded <- read.csv(
    file_info$datapath,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  required_cols <- c("Scenario", month_labels)
  missing_cols <- setdiff(required_cols, names(uploaded))
  if (length(missing_cols) > 0) {
    stop(
      "Uploaded temperature CSV is missing columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  uploaded <- uploaded[required_cols]
  uploaded$Scenario <- trimws(uploaded$Scenario)
  blank_scenarios <- is.na(uploaded$Scenario) | uploaded$Scenario == ""
  uploaded$Scenario[blank_scenarios] <- paste0(
    "Uploaded scenario ",
    which(blank_scenarios)
  )

  for (month in month_labels) {
    uploaded[[month]] <- num_or_na(uploaded[[month]])
  }
  if (any(is.na(uploaded[month_labels]))) {
    stop("Uploaded temperature CSV contains missing or non-numeric monthly temperatures.")
  }

  uploaded
}

calculate_housing_scenario <- function(
  scenario,
  input_temperature_c,
  temperature_basis,
  annual_total_n,
  factor_row
) {
  if (identical(temperature_basis, "outdoor")) {
    outdoor_temperature_c <- input_temperature_c
    indoor_temperature_c <- calculate_indoor_temperature(
      outdoor_temperature_c = outdoor_temperature_c,
      animal_type = factor_row$Animal_Type
    )
  } else {
    outdoor_temperature_c <- NA_real_
    indoor_temperature_c <- input_temperature_c
  }

  ct_applied <- ct_is_applicable(
    ct = factor_row$CT_15,
    corrected_temperature_c = indoor_temperature_c,
    note = factor_row$Note
  )
  annual_ef <- if (ct_applied) {
    factor_row$EFs_TotalN *
      factor_row$CT_15^(indoor_temperature_c - factor_row$T_air)
  } else {
    factor_row$EFs_TotalN
  }
  annual_ef <- cap_ef(annual_ef)

  has_total_n <- !is.na(annual_total_n)
  annual_nh3 <- if (has_total_n) annual_total_n * annual_ef else NA_real_

  data.frame(
    Calculation = "Housing",
    Scenario = scenario,
    AnimalType = factor_row$Animal_Type,
    HousingType = factor_row$Housing,
    ManureType = factor_row$Manure_type,
    TemperatureBasis = ifelse(
      identical(temperature_basis, "outdoor"),
      "Outdoor annual temperature",
      "Indoor annual temperature"
    ),
    Input_Annual_Temperature_C = input_temperature_c,
    Outdoor_Annual_Temperature_C = outdoor_temperature_c,
    Indoor_Annual_Temperature_C = indoor_temperature_c,
    EFs_TotalN = factor_row$EFs_TotalN,
    T_air_Reference_C = factor_row$T_air,
    CT_15 = factor_row$CT_15,
    CT_Threshold_C = threshold_from_note(factor_row$Note),
    CT_Applied = ct_applied,
    Annual_EF_TotalN = annual_ef,
    Annual_TotalN_kg = annual_total_n,
    Annual_NH3_N_kg = annual_nh3,
    Note = factor_row$Note,
    Assumptions = "Annual housing calculation; EF capped at 1 kg NH3-N/kg Total N; workbook Note threshold applied when present.",
    stringsAsFactors = FALSE
  )
}

calculate_storage_monthly_ef <- function(factor_row, temperatures_c) {
  if (is.na(factor_row$CT)) {
    return(cap_ef(rep(factor_row$EFs_TAN, length(temperatures_c))))
  }

  cap_ef(factor_row$EFs_TAN * factor_row$CT^(temperatures_c - factor_row$T_air))
}

add_storage_summary_columns <- function(detail) {
  detail$Year3_EF_TAN <- NA_real_
  detail$Year3_Unweighted_Monthly_EF_TAN <- NA_real_
  detail$Year3_Mass_Balance_EF_TAN <- NA_real_
  detail$Assumptions <- storage_assumption_text()
  detail
}

simulate_storage_scenario <- function(
  scenario,
  temperatures_c,
  annual_tan,
  factor_row
) {
  monthly_ef <- calculate_storage_monthly_ef(factor_row, temperatures_c)
  total_months <- simulation_years * length(month_labels)
  monthly_ef_3yr <- rep(monthly_ef, simulation_years)
  report_mass <- !is.na(annual_tan)
  monthly_tan <- if (report_mass) {
    annual_tan / 12
  } else {
    storage_default_monthly_tan_kg
  }

  detail <- data.frame(
    Calculation = character(0),
    Scenario = character(0),
    Simulation_Year = integer(0),
    Month = character(0),
    Month_Number = integer(0),
    AnimalType = character(0),
    StorageType = character(0),
    ManureType = character(0),
    Temperature_C = numeric(0),
    EFs_TAN = numeric(0),
    T_air_Reference_C = numeric(0),
    CT = numeric(0),
    CT_Applied = logical(0),
    Monthly_EF_TAN = numeric(0),
    Monthly_TAN_Input_kg = numeric(0),
    TAN_Removed_kg = numeric(0),
    TAN_Available_kg = numeric(0),
    NH3_N_kg = numeric(0),
    TAN_End_Stored_kg = numeric(0),
    Removal_Months = character(0),
    Removal_Efficiency = numeric(0),
    stringsAsFactors = FALSE
  )

  stored_tan <- 0
  year3_rows <- vector("list", length(month_labels))
  year3_nh3 <- 0
  year3_tan <- 0
  year3_weighted_ef_sum <- 0
  year3_available_tan_sum <- 0
  year3_unweighted_ef_sum <- 0

  for (period_index in seq_len(total_months)) {
    month_index <- ((period_index - 1) %% length(month_labels)) + 1
    year_index <- ((period_index - 1) %/% length(month_labels)) + 1

    removed_this_month <- if (month_index %in% storage_removal_months) {
      stored_tan * storage_removal_efficiency
    } else {
      0
    }
    stored_tan <- stored_tan - removed_this_month

    available_tan <- stored_tan + monthly_tan
    nh3_loss <- min(available_tan, available_tan * monthly_ef_3yr[period_index])
    monthly_storage_ef <- if (available_tan > 0) nh3_loss / available_tan else NA_real_
    stored_tan <- available_tan - nh3_loss

    if (year_index == simulation_years) {
      year3_rows[[month_index]] <- data.frame(
        Calculation = "Storage",
        Scenario = scenario,
        Simulation_Year = year_index,
        Month = month_labels[month_index],
        Month_Number = month_index,
        AnimalType = factor_row$Animal_Type,
        StorageType = factor_row$Storage,
        ManureType = factor_row$Manure_type,
        Temperature_C = temperatures_c[month_index],
        EFs_TAN = factor_row$EFs_TAN,
        T_air_Reference_C = factor_row$T_air,
        CT = factor_row$CT,
        CT_Applied = !is.na(factor_row$CT),
        Monthly_EF_TAN = monthly_storage_ef,
        Monthly_TAN_Input_kg = monthly_tan,
        TAN_Removed_kg = removed_this_month,
        TAN_Available_kg = available_tan,
        NH3_N_kg = nh3_loss,
        TAN_End_Stored_kg = stored_tan,
        Removal_Months = storage_removal_month_names,
        Removal_Efficiency = storage_removal_efficiency,
        stringsAsFactors = FALSE
      )

      year3_nh3 <- year3_nh3 + nh3_loss
      year3_tan <- year3_tan + monthly_tan
      year3_weighted_ef_sum <- year3_weighted_ef_sum +
        monthly_storage_ef * available_tan
      year3_available_tan_sum <- year3_available_tan_sum + available_tan
      year3_unweighted_ef_sum <- year3_unweighted_ef_sum + monthly_storage_ef
    }
  }

  detail <- do.call(rbind, year3_rows)
  detail <- add_storage_summary_columns(detail)
  year3_ef <- year3_weighted_ef_sum / year3_available_tan_sum
  year3_unweighted_ef <- year3_unweighted_ef_sum / length(month_labels)
  year3_mass_balance_ef <- year3_nh3 / year3_tan

  summary <- data.frame(
    Calculation = "Storage",
    Scenario = scenario,
    Simulation_Year = simulation_years,
    Month = "Annual Summary",
    Month_Number = NA_integer_,
    AnimalType = factor_row$Animal_Type,
    StorageType = factor_row$Storage,
    ManureType = factor_row$Manure_type,
    Temperature_C = NA_real_,
    EFs_TAN = factor_row$EFs_TAN,
    T_air_Reference_C = factor_row$T_air,
    CT = factor_row$CT,
    CT_Applied = !is.na(factor_row$CT),
    Monthly_EF_TAN = NA_real_,
    Monthly_TAN_Input_kg = monthly_tan,
    TAN_Removed_kg = sum(detail$TAN_Removed_kg),
    TAN_Available_kg = sum(detail$TAN_Available_kg),
    NH3_N_kg = year3_nh3,
    TAN_End_Stored_kg = stored_tan,
    Removal_Months = storage_removal_month_names,
    Removal_Efficiency = storage_removal_efficiency,
    Year3_EF_TAN = year3_ef,
    Year3_Unweighted_Monthly_EF_TAN = year3_unweighted_ef,
    Year3_Mass_Balance_EF_TAN = year3_mass_balance_ef,
    Assumptions = storage_assumption_text(),
    stringsAsFactors = FALSE
  )

  if (!report_mass) {
    mass_cols <- c(
      "Monthly_TAN_Input_kg", "TAN_Removed_kg", "TAN_Available_kg",
      "NH3_N_kg", "TAN_End_Stored_kg"
    )
    detail[mass_cols] <- NA_real_
    summary[mass_cols] <- NA_real_
    summary$Assumptions <- paste(
      summary$Assumptions,
      "No annual TAN was entered, so a nominal 100 kg TAN/month was used only to calculate the year-3 EF; NH3-N mass outputs are blank."
    )
  }

  list(
    detail = detail,
    summary = summary,
    annual_ef = year3_ef,
    annual_nh3 = if (report_mass) year3_nh3 else NA_real_,
    end_stored_tan = if (report_mass) stored_tan else NA_real_
  )
}

storage_assumption_text <- function() {
  paste(
    "Annual TAN split evenly across months;",
    "simulation runs for 3 years;",
    "reported EF and NH3-N are from year 3;",
    "removal occurs at the start of April and October;",
    "removal efficiency is 95%; EF capped at 1 kg NH3-N/kg TAN."
  )
}

server <- function(input, output, session) {
  has_results <- reactiveVal(FALSE)

  housing_available <- reactive({
    housing_factors[housing_factors$Selectable, , drop = FALSE]
  })

  storage_available <- reactive({
    storage_factors[storage_factors$Selectable, , drop = FALSE]
  })

  observeEvent(
    list(
      input$calculation_section,
      input$housing_factor_row,
      input$housing_temperature_basis,
      input$housing_annual_temp,
      input$annual_total_n,
      input$storage_factor_row,
      input$annual_tan,
      input$temperature_csv,
      input$storage_temp_jan,
      input$storage_temp_feb,
      input$storage_temp_mar,
      input$storage_temp_apr,
      input$storage_temp_may,
      input$storage_temp_jun,
      input$storage_temp_jul,
      input$storage_temp_aug,
      input$storage_temp_sep,
      input$storage_temp_oct,
      input$storage_temp_nov,
      input$storage_temp_dec
    ),
    {
      has_results(FALSE)
    },
    ignoreInit = TRUE
  )

  observe({
    rows <- housing_available()
    choices <- unique(rows$Animal_Type)
    if (length(choices) == 0) {
      return()
    }
    selected <- if (!is.null(input$housing_animal) && input$housing_animal %in% choices) {
      input$housing_animal
    } else {
      choices[1]
    }
    updateSelectInput(session, "housing_animal", choices = choices, selected = selected)
  })

  observe({
    rows <- housing_available()
    req(input$housing_animal)
    choices <- unique(rows$Housing[rows$Animal_Type == input$housing_animal])
    if (length(choices) == 0) {
      return()
    }
    selected <- if (!is.null(input$housing_house) && input$housing_house %in% choices) {
      input$housing_house
    } else {
      choices[1]
    }
    updateSelectInput(session, "housing_house", choices = choices, selected = selected)
  })

  observe({
    rows <- housing_available()
    req(input$housing_animal, input$housing_house)
    filtered <- rows[
      rows$Animal_Type == input$housing_animal &
        rows$Housing == input$housing_house,
      ,
      drop = FALSE
    ]
    choices <- unique(filtered$Manure_type)
    if (length(choices) == 0) {
      return()
    }
    selected <- if (!is.null(input$housing_manure) && input$housing_manure %in% choices) {
      input$housing_manure
    } else {
      choices[1]
    }
    updateSelectInput(session, "housing_manure", choices = choices, selected = selected)
  })

  observe({
    rows <- housing_available()
    req(input$housing_animal, input$housing_house, input$housing_manure)
    filtered <- rows[
      rows$Animal_Type == input$housing_animal &
        rows$Housing == input$housing_house &
        rows$Manure_type == input$housing_manure,
      ,
      drop = FALSE
    ]
    if (nrow(filtered) == 0) {
      return()
    }
    choice_values <- as.character(filtered$Source_Row)
    names(choice_values) <- vapply(
      seq_len(nrow(filtered)),
      function(i) format_housing_factor_label(filtered[i, ]),
      character(1)
    )
    selected <- if (!is.null(input$housing_factor_row) && input$housing_factor_row %in% choice_values) {
      input$housing_factor_row
    } else {
      choice_values[1]
    }
    updateSelectInput(session, "housing_factor_row", choices = choice_values, selected = selected)
  })

  selected_housing_factor <- reactive({
    req(input$housing_factor_row)
    row <- housing_factors[
      housing_factors$Source_Row == as.integer(input$housing_factor_row),
      ,
      drop = FALSE
    ]
    req(nrow(row) == 1)
    row
  })

  output$housing_factor_note <- renderText({
    row <- selected_housing_factor()
    threshold_c <- threshold_from_note(row$Note)
    pieces <- c(
      sprintf("EFs_TotalN: %.4f kg NH3-N/kg Total N.", row$EFs_TotalN),
      sprintf("Reference indoor T_air: %.1f deg C.", row$T_air),
      sprintf("CT_15: %.3f.", row$CT_15)
    )
    if (!is.na(threshold_c)) {
      pieces <- c(
        pieces,
        sprintf("CT is applied only when corrected indoor temperature is greater than %.1f deg C.", threshold_c)
      )
    }
    if (nzchar(row$Note)) {
      pieces <- c(pieces, paste("Workbook note:", row$Note))
    }
    paste(pieces, collapse = " ")
  })

  observe({
    rows <- storage_available()
    choices <- unique(rows$Animal_Type)
    if (length(choices) == 0) {
      return()
    }
    selected <- if (!is.null(input$storage_animal) && input$storage_animal %in% choices) {
      input$storage_animal
    } else {
      choices[1]
    }
    updateSelectInput(session, "storage_animal", choices = choices, selected = selected)
  })

  observe({
    rows <- storage_available()
    req(input$storage_animal)
    choices <- unique(rows$Storage[rows$Animal_Type == input$storage_animal])
    if (length(choices) == 0) {
      return()
    }
    selected <- if (!is.null(input$storage_type) && input$storage_type %in% choices) {
      input$storage_type
    } else {
      choices[1]
    }
    updateSelectInput(session, "storage_type", choices = choices, selected = selected)
  })

  observe({
    rows <- storage_available()
    req(input$storage_animal, input$storage_type)
    filtered <- rows[
      rows$Animal_Type == input$storage_animal &
        rows$Storage == input$storage_type,
      ,
      drop = FALSE
    ]
    choices <- unique(filtered$Manure_type)
    if (length(choices) == 0) {
      return()
    }
    selected <- if (!is.null(input$storage_manure) && input$storage_manure %in% choices) {
      input$storage_manure
    } else {
      choices[1]
    }
    updateSelectInput(session, "storage_manure", choices = choices, selected = selected)
  })

  observe({
    rows <- storage_available()
    req(input$storage_animal, input$storage_type, input$storage_manure)
    filtered <- rows[
      rows$Animal_Type == input$storage_animal &
        rows$Storage == input$storage_type &
        rows$Manure_type == input$storage_manure,
      ,
      drop = FALSE
    ]
    if (nrow(filtered) == 0) {
      return()
    }
    choice_values <- as.character(filtered$Source_Row)
    names(choice_values) <- vapply(
      seq_len(nrow(filtered)),
      function(i) format_storage_factor_label(filtered[i, ]),
      character(1)
    )
    selected <- if (!is.null(input$storage_factor_row) && input$storage_factor_row %in% choice_values) {
      input$storage_factor_row
    } else {
      choice_values[1]
    }
    updateSelectInput(session, "storage_factor_row", choices = choice_values, selected = selected)
  })

  selected_storage_factor <- reactive({
    req(input$storage_factor_row)
    row <- storage_factors[
      storage_factors$Source_Row == as.integer(input$storage_factor_row),
      ,
      drop = FALSE
    ]
    req(nrow(row) == 1)
    row
  })

  output$storage_factor_note <- renderText({
    row <- selected_storage_factor()
    paste(
      sprintf("EFs_TAN: %.4f kg NH3-N/kg TAN.", row$EFs_TAN),
      sprintf("Reference outdoor T_air: %.1f deg C.", row$T_air),
      sprintf("CT: %s.", ifelse(is.na(row$CT), "not applied", sprintf("%.3f", row$CT))),
      storage_assumption_text()
    )
  })

  output$download_temperature_template <- downloadHandler(
    filename = function() "temperature_upload_template.csv",
    content = function(file) {
      template_path <- app_file("temperature_upload_template.csv")
      if (!file.exists(template_path)) {
        template_path <- app_file("ShinyApp", "temperature_upload_template.csv")
      }
      file.copy(template_path, file, overwrite = TRUE)
    }
  )

  calculation_results <- eventReactive(input$plot_button, {
    if (identical(input$calculation_section, "housing")) {
      factor_row <- selected_housing_factor()
      validate(
        need(!is.na(input$housing_annual_temp), "Enter an annual housing temperature."),
        need(is.na(input$annual_total_n) || input$annual_total_n >= 0, "Annual Total N must be non-negative.")
      )

      annual_total_n <- if (is.na(input$annual_total_n)) NA_real_ else input$annual_total_n
      primary <- calculate_housing_scenario(
        scenario = "Primary",
        input_temperature_c = input$housing_annual_temp,
        temperature_basis = input$housing_temperature_basis,
        annual_total_n = annual_total_n,
        factor_row = factor_row
      )

      list(
        section = "housing",
        primary = primary,
        results_df = primary,
        export_df = primary
      )
    } else {
      uploaded_temperatures <- tryCatch(
        read_temperature_upload(input$temperature_csv),
        error = function(e) {
          validate(need(FALSE, e$message))
        }
      )

      factor_row <- selected_storage_factor()
      temps <- get_month_values(input, "storage_temp")
      validate(
        need(!any(is.na(temps)), "Enter monthly storage air temperatures for all 12 months."),
        need(is.na(input$annual_tan) || input$annual_tan >= 0, "Annual TAN must be non-negative.")
      )

      annual_tan <- if (is.na(input$annual_tan)) NA_real_ else input$annual_tan
      primary <- simulate_storage_scenario(
        scenario = "Primary",
        temperatures_c = temps,
        annual_tan = annual_tan,
        factor_row = factor_row
      )

      batch_parts <- list()
      if (!is.null(uploaded_temperatures)) {
        for (i in seq_len(nrow(uploaded_temperatures))) {
          batch_parts[[i]] <- simulate_storage_scenario(
            scenario = uploaded_temperatures$Scenario[i],
            temperatures_c = as.numeric(uploaded_temperatures[i, month_labels]),
            annual_tan = annual_tan,
            factor_row = factor_row
          )
        }
      }

      export_rows <- c(
        list(primary$detail, primary$summary),
        unlist(
          lapply(batch_parts, function(x) list(x$detail, x$summary)),
          recursive = FALSE
        )
      )

      list(
        section = "storage",
        primary = primary,
        results_df = primary$detail,
        export_df = do.call(rbind, export_rows)
      )
    }
  })

  observeEvent(calculation_results(), {
    has_results(TRUE)
  })

  output$download_ui <- renderUI({
    if (!has_results()) {
      tags$button("Download CSV", type = "button", class = "btn download-button", disabled = "disabled")
    } else {
      downloadButton("download_results", "Download CSV", class = "download-button")
    }
  })

  output$summary_ef_label <- renderText({
    if (identical(input$calculation_section, "housing")) {
      "Annual Housing EF"
    } else {
      "Year 3 Storage EF"
    }
  })

  output$summary_nh3_label <- renderText({
    section <- if (has_results()) {
      calculation_results()$section
    } else {
      input$calculation_section
    }

    if (identical(section, "storage")) {
      "Year 3 NH3-N Loss"
    } else {
      "Annual NH3 Emission"
    }
  })

  output$summary_ef <- renderText({
    req(has_results())
    results <- calculation_results()
    if (identical(results$section, "housing")) {
      sprintf("%.4f", results$primary$Annual_EF_TotalN)
    } else {
      sprintf("%.4f", results$primary$annual_ef)
    }
  })

  output$summary_nh3 <- renderUI({
    req(has_results())
    results <- calculation_results()
    if (identical(results$section, "housing")) {
      annual_nh3 <- results$primary$Annual_NH3_N_kg
      missing_text <- "Total N needed"
    } else {
      annual_nh3 <- results$primary$annual_nh3
      missing_text <- "TAN needed"
    }

    if (is.na(annual_nh3)) {
      span(missing_text)
    } else {
      HTML(sprintf("%.2f kg NH<sub>3</sub>-N", annual_nh3))
    }
  })

  output$summary_mass_label <- renderText({
    if (identical(input$calculation_section, "housing")) {
      "Input Basis"
    } else {
      "Ending Stored TAN"
    }
  })

  output$summary_mass <- renderText({
    req(has_results())
    results <- calculation_results()
    if (identical(results$section, "housing")) {
      "Annual Total N"
    } else if (is.na(results$primary$end_stored_tan)) {
      "TAN needed"
    } else {
      sprintf("%.2f kg N", results$primary$end_stored_tan)
    }
  })

  output$plot_panel <- renderUI({
    if (!has_results()) {
      return(NULL)
    }

    results <- calculation_results()
    if (!identical(results$section, "storage")) {
      return(NULL)
    }

    div(
      class = "panel-card",
      plotOutput("ef_plot", height = "460px"),
      tags$div(
        class = "results-caption",
        "Primary scenario only. Line: year-3 monthly storage EF. Dashed line: monthly air temperature."
      )
    )
  })

  output$ef_plot <- renderPlot({
    req(has_results())
    results <- calculation_results()
    req(identical(results$section, "storage"))

    df <- results$results_df
    x_vals <- seq_along(month_labels)
    ef_values <- df$Monthly_EF_TAN
    temp_values <- df$Temperature_C
    ef_range <- range(ef_values, na.rm = TRUE)
    temp_range <- range(temp_values, na.rm = TRUE)
    if (diff(ef_range) == 0) ef_range <- ef_range + c(-0.01, 0.01)
    if (diff(temp_range) == 0) temp_range <- temp_range + c(-1, 1)
    temp_to_ef <- function(x) {
      (x - temp_range[1]) / diff(temp_range) * diff(ef_range) + ef_range[1]
    }
    temp_breaks <- pretty(temp_range)

    par(mar = c(4.5, 4.8, 4.5, 4.8))
    plot(
      x_vals, ef_values,
      type = "o", pch = 16, lwd = 2.8, col = "#2E5E4E",
      xaxt = "n", xlab = "Month", ylab = "EF (kg NH3-N/kg TAN)",
      ylim = ef_range + c(-0.12, 0.12) * diff(ef_range),
      main = expression("Year 3 Monthly Storage " * NH[3] * " Emission Factor")
    )
    axis(1, at = x_vals, labels = month_labels)
    lines(
      x_vals, temp_to_ef(temp_values),
      type = "o", pch = 1, lwd = 2, lty = 2, col = "#C06A2B"
    )
    axis(
      side = 4,
      at = temp_to_ef(temp_breaks),
      labels = round(temp_breaks, 1),
      col.axis = "#C06A2B"
    )
    mtext("Air temperature (deg C)", side = 4, line = 3, col = "#C06A2B")
    legend(
      "topleft",
      legend = c("Monthly storage EF", "Air temperature"),
      col = c("#2E5E4E", "#C06A2B"),
      lty = c(1, 2),
      lwd = c(2.8, 2),
      pch = c(16, 1),
      bty = "n"
    )
    box()
  })

  output$results_table <- renderTable({
    req(has_results())
    results <- calculation_results()
    df <- if (identical(results$section, "housing")) {
      results$results_df
    } else {
      rbind(results$results_df, results$primary$summary)
    }
    numeric_cols <- vapply(df, is.numeric, logical(1))
    df[numeric_cols] <- lapply(df[numeric_cols], function(col) round(col, 4))
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$download_results <- downloadHandler(
    filename = function() {
      req(has_results())
      paste0("nh3_results_", calculation_results()$section, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(has_results())
      write.csv(calculation_results()$export_df, file, row.names = FALSE, na = "")
    }
  )
}
