# Shared constants, data loading, calculation helpers, and Shiny server logic.
# Keep NH3 emission model changes here so UI layout stays separate.

library(shiny)

month_labels <- c(
  "Jan", "Feb", "Mar", "Apr", "May", "Jun",
  "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"
)

housing_ref_monthly_temps <- c(-5.4, -4.4, 0.4, 6.6, 11.8, 16.4, 18.8, 17.8, 13.1, 7.7, 2.4, -2.9)
housing_ref_annual_temp <- 6.8

# Storage will be updated later 
storage_animal_defaults <- data.frame(
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
  ipcc_group = c(
    "Dairy Cow",
    "Other Cattle",
    "Other Cattle",
    "Other Cattle",
    "Other animals",
    "Swine"
  ),
  stringsAsFactors = FALSE
)

housing_default_efs <- data.frame(
  Animal_Type = c("Dairy Cattle", "Non-Dairy Cattle", "Sheep", "Swine"),
  Default_EF_TotalN = c(12.2, 7.6, 25.0, 29.6) / 100,
  stringsAsFactors = FALSE
)

# Storage/cover factors use IPCC FracGas values normalized to the baseline
# liquid/slurry value for each animal group.
storage_choices <- c(
  "slurry tank" = "slurry_tank",
  "lagoon" = "lagoon",
  "manure heap" = "manure_heap",
  "pit" = "pit",
  "weeping-wall" = "weeping_wall",
  "experimental vessels" = "experimental_vessels",
  "unsure" = "unsure"
)

cover_choices <- c(
  "uncovered" = "uncovered",
  "natural crust" = "natural_crust",
  "covered" = "covered",
  "covered/compacted solid" = "covered_compacted_solid",
  "additives" = "additives",
  "bulking agent" = "bulking_agent",
  "unsure" = "unsure"
)

ipcc_baselines <- data.frame(
  ipcc_group = c("Swine", "Dairy Cow", "Other Cattle", "Other animals"),
  baseline_fracgas = c(0.48, 0.48, 0.48, 0.15),
  stringsAsFactors = FALSE
)

storage_cover_defaults <- data.frame(
  ipcc_group = c(
    "Swine", "Dairy Cow", "Other Cattle", "Other animals",
    "Swine", "Dairy Cow", "Other Cattle", "Other animals",
    "Swine", "Dairy Cow", "Other Cattle", "Other animals",
    "Swine", "Dairy Cow", "Other Cattle", "Other animals",
    "Swine", "Dairy Cow", "Other Cattle", "Other animals",
    "Swine", "Dairy Cow", "Other Cattle", "Other animals",
    "Swine", "Dairy Cow", "Other Cattle", "Other animals",
    "Swine", "Dairy Cow", "Other Cattle", "Other animals",
    "Swine", "Dairy Cow", "Other Cattle", "Other animals",
    "Swine", "Dairy Cow", "Other Cattle", "Other animals"
  ),
  storage_type = c(
    rep("slurry_tank", 12),
    rep("lagoon", 4),
    rep("pit", 4),
    rep("manure_heap", 20)
  ),
  cover_condition = c(
    rep("uncovered", 4),
    rep("natural_crust", 4),
    rep("covered", 4),
    rep("uncovered", 4),
    rep("uncovered", 4),
    rep("uncovered", 4),
    rep("covered", 4),
    rep("covered_compacted_solid", 4),
    rep("additives", 4),
    rep("bulking_agent", 4)
  ),
  fracgas = c(
    0.48, 0.48, 0.48, 0.15,
    0.30, 0.30, 0.30, 0.09,
    0.10, 0.10, 0.10, 0.03,
    0.40, 0.35, 0.35, 0.35,
    0.25, 0.28, 0.25, 0.25,
    0.45, 0.30, 0.45, 0.12,
    0.22, 0.14, 0.22, 0.05,
    0.22, 0.14, 0.22, 0.05,
    0.17, 0.11, 0.17, 0.04,
    0.58, 0.38, 0.58, 0.15
  ),
  source_label = c(
    rep("IPCC 2019 Table 10.22: liquid/slurry without natural crust", 4),
    rep("IPCC 2019 Table 10.22: liquid/slurry with natural crust", 4),
    rep("IPCC 2019 Table 10.22: liquid/slurry with cover", 4),
    rep("IPCC 2019 Table 10.22: uncovered anaerobic lagoon", 4),
    rep("IPCC 2019 Table 10.22: pit storage below animal confinements", 4),
    rep("IPCC 2019 Table 10.22: solid storage", 4),
    rep("IPCC 2019 Table 10.22: solid storage covered/compacted", 4),
    rep("IPCC 2019 Table 10.22: solid storage covered/compacted", 4),
    rep("IPCC 2019 Table 10.22: solid storage with additives", 4),
    rep("IPCC 2019 Table 10.22: solid storage with bulking agent", 4)
  ),
  stringsAsFactors = FALSE
)

num_or_na <- function(x) {
  suppressWarnings(as.numeric(x))
}

load_housing_factors <- function(path = "Reference/NH3_Correction_Factors.csv") {
  # Housing correction factors are kept in CSV form so shinyapps.io can load
  # them without needing Excel-reading packages.
  factors <- read.csv(path, stringsAsFactors = FALSE, na.strings = c(""))
  names(factors) <- trimws(names(factors))
  factors$Animal_Type <- trimws(factors$Animal_Type)
  factors$Housing <- trimws(factors$Housing)
  factors$Manure_type <- trimws(factors$Manure_type)
  factors$Note[is.na(factors$Note)] <- ""
  factors$EFs_TotalN <- num_or_na(factors$EFs_TotalN)
  factors$Efs_TAN <- num_or_na(factors$Efs_TAN)
  factors$CT_wet <- num_or_na(factors$CT_wet)
  factors$CT_dry <- num_or_na(factors$CT_dry)
  factors$Source_Row <- seq_len(nrow(factors))

  # If observed Total-N EF is missing, fall back to the app-level EF default
  # converted from percent to decimal. Rows with neither source are hidden.
  defaults <- housing_default_efs[match(factors$Animal_Type, housing_default_efs$Animal_Type), ]
  factors$Default_EF_TotalN <- defaults$Default_EF_TotalN
  factors$EFref_TotalN <- ifelse(
    is.na(factors$EFs_TotalN),
    factors$Default_EF_TotalN,
    factors$EFs_TotalN
  )
  factors$EF_Source <- ifelse(
    is.na(factors$EFs_TotalN),
    "App default EFTref converted from percent to decimal",
    "Observed EFs_TotalN from correction-factor file"
  )
  factors$Selectable <- !is.na(factors$EFref_TotalN)
  factors
}

housing_factors <- load_housing_factors()

format_factor_label <- function(row) {
  note_text <- if (nzchar(row$Note)) paste0("; note: ", row$Note) else ""
  sprintf(
    "Row %s: EF %.4f (%s); CT_wet %s; CT_dry %s%s",
    row$Source_Row,
    row$EFref_TotalN,
    ifelse(grepl("^Observed", row$EF_Source), "observed", "default"),
    ifelse(is.na(row$CT_wet), "not applicable", sprintf("%.3f", row$CT_wet)),
    ifelse(is.na(row$CT_dry), "not applicable", sprintf("%.3f", row$CT_dry)),
    note_text
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

make_nonnegative_month_input <- function(id, label) {
  numericInput(
    inputId = id,
    label = label,
    value = NA_real_,
    min = 0,
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

get_storage_multiplier <- function(ipcc_group, storage_type, cover_condition) {
  baseline <- ipcc_baselines$baseline_fracgas[ipcc_baselines$ipcc_group == ipcc_group]
  matched <- storage_cover_defaults[
    storage_cover_defaults$ipcc_group == ipcc_group &
      storage_cover_defaults$storage_type == storage_type &
      storage_cover_defaults$cover_condition == cover_condition,
    ,
    drop = FALSE
  ]

  if (length(baseline) != 1 || nrow(matched) != 1) {
    return(list(
      multiplier = 1,
      fracgas = NA_real_,
      source = "No IPCC default available; multiplier set to 1.000"
    ))
  }

  list(
    multiplier = matched$fracgas / baseline,
    fracgas = matched$fracgas,
    source = matched$source_label
  )
}

read_temperature_upload <- function(file_info) {
  # Batch temperatures are wide: one scenario per row, one column per month.
  # They are exported with results but intentionally not plotted in the app.
  if (is.null(file_info)) {
    return(NULL)
  }
  uploaded <- read.csv(file_info$datapath, stringsAsFactors = FALSE, check.names = FALSE)
  required_cols <- c("Scenario", month_labels)
  missing_cols <- setdiff(required_cols, names(uploaded))
  if (length(missing_cols) > 0) {
    stop(paste("Uploaded temperature CSV is missing columns:", paste(missing_cols, collapse = ", ")))
  }
  uploaded <- uploaded[required_cols]
  uploaded$Scenario <- trimws(uploaded$Scenario)
  uploaded$Scenario[uploaded$Scenario == ""] <- paste0("Uploaded scenario ", which(uploaded$Scenario == ""))
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
  temps,
  temp_basis,
  n_values,
  factor_row,
  ct_basis,
  annual_temperature = NA_real_
) {
  # Housing uses Total N and Total-N EF values. There is no manure removal
  # mass flow here; each month is simply N input multiplied by corrected EF.
  ct_value <- if (identical(ct_basis, "wet")) factor_row$CT_wet else factor_row$CT_dry
  if (is.na(ct_value)) {
    stop("Selected CT is not applicable for this climate.")
  }

  if (identical(temp_basis, "Annual")) {
    # Annual temperature uses the annual reference temperature and produces
    # one annual-style EF repeated across months for a consistent table shape.
    ef_values <- factor_row$EFref_TotalN * (ct_value ^ (annual_temperature - housing_ref_annual_temp))
    ef_values <- rep(ef_values, 12)
    temp_values <- rep(annual_temperature, 12)
    ref_values <- rep(housing_ref_annual_temp, 12)
  } else {
    # Monthly temperature uses month-specific reference temperatures from the
    # cool temperate moist baseline.
    ef_values <- factor_row$EFref_TotalN * (ct_value ^ (temps - housing_ref_monthly_temps))
    temp_values <- temps
    ref_values <- housing_ref_monthly_temps
  }

  has_n <- !all(is.na(n_values))
  nh3_emission <- if (has_n) n_values * ef_values else rep(NA_real_, 12)
  annual_n <- if (has_n) sum(n_values) else NA_real_
  annual_nh3 <- if (has_n) sum(nh3_emission) else NA_real_
  annual_ef <- if (has_n && annual_n > 0) annual_nh3 / annual_n else mean(ef_values)

  results <- data.frame(
    Calculation = rep("Housing", 12),
    Scenario = rep(scenario, 12),
    Month = month_labels,
    AnimalType = rep(factor_row$Animal_Type, 12),
    HousingType = rep(factor_row$Housing, 12),
    ManureType = rep(factor_row$Manure_type, 12),
    ClimateCTBasis = rep(ifelse(identical(ct_basis, "wet"), "Wet", "Dry"), 12),
    CT = rep(ct_value, 12),
    CTNote = rep(factor_row$Note, 12),
    EFSource = rep(factor_row$EF_Source, 12),
    EFref_TotalN = rep(factor_row$EFref_TotalN, 12),
    Temperature_C = temp_values,
    ReferenceTemperature_C = ref_values,
    Corrected_EF_TotalN = ef_values,
    TotalN = n_values,
    NH3_emission = nh3_emission,
    stringsAsFactors = FALSE
  )

  summary <- data.frame(
    Calculation = "Housing",
    Scenario = scenario,
    Month = "Annual Summary",
    AnimalType = factor_row$Animal_Type,
    HousingType = factor_row$Housing,
    ManureType = factor_row$Manure_type,
    ClimateCTBasis = ifelse(identical(ct_basis, "wet"), "Wet", "Dry"),
    CT = ct_value,
    CTNote = factor_row$Note,
    EFSource = factor_row$EF_Source,
    EFref_TotalN = factor_row$EFref_TotalN,
    Temperature_C = if (identical(temp_basis, "Annual")) annual_temperature else NA_real_,
    ReferenceTemperature_C = if (identical(temp_basis, "Annual")) housing_ref_annual_temp else NA_real_,
    Corrected_EF_TotalN = annual_ef,
    TotalN = annual_n,
    NH3_emission = annual_nh3,
    stringsAsFactors = FALSE
  )

  list(results = results, summary = summary, annual_ef = annual_ef, annual_nh3 = annual_nh3)
}

calculate_storage_scenario <- function(
  scenario,
  temps,
  generated_tan,
  defaults,
  tref,
  eftref,
  storage,
  removal_months,
  removal_efficiency
) {
  # Storage keeps the previous TAN-based model: temperature-corrected EF,
  # storage/cover multiplier, then monthly TAN mass flow with removals.
  ct_value <- defaults$ct
  base_ef <- eftref * (ct_value ^ (temps - tref))
  adjusted_ef <- base_ef * storage$multiplier

  has_tan <- !all(is.na(generated_tan))
  tan_removed <- rep(NA_real_, 12)
  tan_available <- rep(NA_real_, 12)
  nh3_emission <- rep(NA_real_, 12)
  ending_stored_tan <- rep(NA_real_, 12)

  if (has_tan) {
    stored_tan <- 0
    for (i in seq_along(month_labels)) {
      # Removal happens at the start of selected months and only affects TAN
      # carried over from previous months; current-month TAN is added after.
      removed_this_month <- if (i %in% removal_months) stored_tan * removal_efficiency else 0
      stored_tan <- stored_tan - removed_this_month
      available_this_month <- stored_tan + generated_tan[i]
      # Cap NH3 loss at available TAN to prevent negative storage.
      emission_this_month <- min(available_this_month, available_this_month * adjusted_ef[i] / 100)
      stored_tan <- available_this_month - emission_this_month

      tan_removed[i] <- removed_this_month
      tan_available[i] <- available_this_month
      nh3_emission[i] <- emission_this_month
      ending_stored_tan[i] <- stored_tan
    }
  }

  annual_generated_tan <- if (has_tan) sum(generated_tan) else NA_real_
  annual_removed_tan <- if (has_tan) sum(tan_removed) else NA_real_
  annual_nh3 <- if (has_tan) sum(nh3_emission) else NA_real_
  end_stored_tan <- if (has_tan) tail(ending_stored_tan, 1) else NA_real_
  annual_effective_ef <- if (has_tan && annual_generated_tan > 0) {
    annual_nh3 / annual_generated_tan * 100
  } else {
    mean(adjusted_ef)
  }

  results <- data.frame(
    Calculation = rep("Storage", 12),
    Scenario = rep(scenario, 12),
    Month = month_labels,
    AnimalType = rep(defaults$animal_type, 12),
    Temperature_C = temps,
    Tref_C = rep(tref, 12),
    CT = rep(ct_value, 12),
    EFTref_percent = rep(eftref, 12),
    Base_EF_percent = base_ef,
    StorageCoverMultiplier = rep(storage$multiplier, 12),
    StorageCoverDefault = rep(storage$source, 12),
    Adjusted_EF_percent = adjusted_ef,
    TAN_Generated = generated_tan,
    TAN_Removed = tan_removed,
    TAN_Available = tan_available,
    NH3_emission = nh3_emission,
    TAN_End_Stored = ending_stored_tan,
    stringsAsFactors = FALSE
  )

  summary <- data.frame(
    Calculation = "Storage",
    Scenario = scenario,
    Month = "Annual Summary",
    AnimalType = defaults$animal_type,
    Temperature_C = NA_real_,
    Tref_C = tref,
    CT = ct_value,
    EFTref_percent = eftref,
    Base_EF_percent = mean(base_ef),
    StorageCoverMultiplier = storage$multiplier,
    StorageCoverDefault = storage$source,
    Adjusted_EF_percent = annual_effective_ef,
    TAN_Generated = annual_generated_tan,
    TAN_Removed = annual_removed_tan,
    TAN_Available = NA_real_,
    NH3_emission = annual_nh3,
    TAN_End_Stored = end_stored_tan,
    stringsAsFactors = FALSE
  )

  list(
    results = results,
    summary = summary,
    annual_effective_ef = annual_effective_ef,
    annual_nh3 = annual_nh3,
    annual_removed_tan = annual_removed_tan,
    end_stored_tan = end_stored_tan
  )
}

server <- function(input, output, session) {
  has_results <- reactiveVal(FALSE)

  housing_available <- reactive({
    housing_factors[housing_factors$Selectable, , drop = FALSE]
  })

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
    filtered <- rows[rows$Animal_Type == input$housing_animal & rows$Housing == input$housing_house, , drop = FALSE]
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
    names(choice_values) <- vapply(seq_len(nrow(filtered)), function(i) format_factor_label(filtered[i, ]), character(1))
    selected <- if (!is.null(input$housing_factor_row) && input$housing_factor_row %in% choice_values) {
      input$housing_factor_row
    } else {
      choice_values[1]
    }
    updateSelectInput(session, "housing_factor_row", choices = choice_values, selected = selected)
  })

  selected_housing_factor <- reactive({
    req(input$housing_factor_row)
    row <- housing_factors[housing_factors$Source_Row == as.integer(input$housing_factor_row), , drop = FALSE]
    req(nrow(row) == 1)
    row
  })

  output$housing_factor_note <- renderText({
    row <- selected_housing_factor()
    ct_value <- if (identical(input$housing_ct_basis, "wet")) row$CT_wet else row$CT_dry
    pieces <- c(
      sprintf("EFref: %.4f kg NH3-N/kg Total N (%s).", row$EFref_TotalN, row$EF_Source),
      sprintf("Selected CT: %s.", ifelse(is.na(ct_value), "not applicable", sprintf("%.3f", ct_value)))
    )
    if (nzchar(row$Note)) {
      pieces <- c(pieces, paste("Note:", row$Note))
    }
    paste(pieces, collapse = " ")
  })

  selected_storage_defaults <- reactive({
    storage_animal_defaults[
      storage_animal_defaults$animal_type == input$storage_animal_type,
      ,
      drop = FALSE
    ]
  })

  storage_multiplier <- reactive({
    get_storage_multiplier(
      selected_storage_defaults()$ipcc_group,
      input$storage_type,
      input$cover_condition
    )
  })

  output$storage_ct_display <- renderText({
    sprintf("%.3f", selected_storage_defaults()$ct)
  })

  output$storage_ipcc_group_display <- renderText({
    selected_storage_defaults()$ipcc_group
  })

  output$storage_multiplier_display <- renderText({
    storage <- storage_multiplier()
    fracgas_text <- if (is.na(storage$fracgas)) "no default" else sprintf("FracGas %.2f", storage$fracgas)
    sprintf("%.3f (%s)", storage$multiplier, fracgas_text)
  })

  observeEvent(input$storage_animal_type, {
    updateNumericInput(session, "storage_eftref", value = selected_storage_defaults()$eftref)
  }, ignoreInit = TRUE)

  output$download_temperature_template <- downloadHandler(
    filename = function() "temperature_upload_template.csv",
    content = function(file) {
      file.copy("temperature_upload_template.csv", file, overwrite = TRUE)
    }
  )

  calculation_results <- eventReactive(input$plot_button, {
    # One dispatcher handles both app modes. Manual inputs always create the
    # plotted Primary scenario; uploaded temperature rows create export-only
    # scenarios using the same selected assumptions.
    uploaded_temperatures <- tryCatch(
      read_temperature_upload(input$temperature_csv),
      error = function(e) {
        validate(need(FALSE, e$message))
      }
    )

    if (identical(input$calculation_section, "housing")) {
      # Housing branch: Total N basis, housing correction-factor table,
      # no storage/removal controls.
      factor_row <- selected_housing_factor()
      ct_value <- if (identical(input$housing_ct_basis, "wet")) factor_row$CT_wet else factor_row$CT_dry
      validate(need(!is.na(ct_value), "Selected CT is not applicable for this climate."))

      if (identical(input$total_n_mode, "annual")) {
        validate(need(is.na(input$annual_total_n) || input$annual_total_n >= 0, "Annual Total N must be non-negative."))
        total_n_values <- if (is.na(input$annual_total_n)) rep(NA_real_, 12) else rep(input$annual_total_n / 12, 12)
      } else {
        total_n_values <- get_month_values(input, "total_n")
        if (all(is.na(total_n_values))) {
          total_n_values <- rep(NA_real_, 12)
        } else {
          validate(
            need(!any(is.na(total_n_values)), "Enter Total N for all 12 months, or leave all 12 blank."),
            need(all(total_n_values >= 0), "Monthly Total N values must be non-negative.")
          )
        }
      }

      if (identical(input$housing_temp_mode, "annual")) {
        validate(need(!is.na(input$housing_annual_temp), "Enter an annual housing temperature."))
        primary <- calculate_housing_scenario(
          scenario = "Primary",
          temps = rep(input$housing_annual_temp, 12),
          temp_basis = "Annual",
          n_values = total_n_values,
          factor_row = factor_row,
          ct_basis = input$housing_ct_basis,
          annual_temperature = input$housing_annual_temp
        )
      } else {
        temps <- get_month_values(input, "housing_temp")
        validate(need(!any(is.na(temps)), "Enter monthly housing temperatures for all 12 months."))
        primary <- calculate_housing_scenario(
          scenario = "Primary",
          temps = temps,
          temp_basis = "Monthly",
          n_values = total_n_values,
          factor_row = factor_row,
          ct_basis = input$housing_ct_basis
        )
      }

      batch_parts <- list()
      if (!is.null(uploaded_temperatures)) {
        # Uploaded housing scenarios always use monthly temperatures, even if
        # the Primary scenario uses annual temperature.
        for (i in seq_len(nrow(uploaded_temperatures))) {
          batch_parts[[i]] <- calculate_housing_scenario(
            scenario = uploaded_temperatures$Scenario[i],
            temps = as.numeric(uploaded_temperatures[i, month_labels]),
            temp_basis = "Monthly",
            n_values = total_n_values,
            factor_row = factor_row,
            ct_basis = input$housing_ct_basis
          )
        }
      }

      export_rows <- c(list(primary$results, primary$summary), unlist(lapply(batch_parts, function(x) list(x$results, x$summary)), recursive = FALSE))
      list(
        section = "housing",
        primary = primary,
        results_df = primary$results,
        export_df = do.call(rbind, export_rows)
      )
    } else {
      # Storage branch: TAN basis and the existing storage/removal mass flow.
      temps <- get_month_values(input, "storage_temp")
      validate(
        need(!any(is.na(temps)), "Enter monthly storage temperatures for all 12 months."),
        need(!is.na(input$storage_tref), "Enter a numeric storage Tref value."),
        need(!is.na(input$storage_eftref), "Enter a numeric storage EFTref value."),
        need(input$storage_eftref >= 0, "Storage EFTref must be non-negative."),
        need(!is.na(input$removal_efficiency), "Enter a numeric removal efficiency."),
        need(input$removal_efficiency >= 0 && input$removal_efficiency <= 100, "Removal efficiency must be between 0 and 100 percent.")
      )

      if (identical(input$tan_mode, "annual")) {
        validate(need(is.na(input$annual_tan) || input$annual_tan >= 0, "Annual TAN must be non-negative."))
        generated_tan <- if (is.na(input$annual_tan)) rep(NA_real_, 12) else rep(input$annual_tan / 12, 12)
      } else {
        generated_tan <- get_month_values(input, "tan")
        if (all(is.na(generated_tan))) {
          generated_tan <- rep(NA_real_, 12)
        } else {
          validate(
            need(!any(is.na(generated_tan)), "Enter monthly TAN for all 12 months, or leave all 12 blank."),
            need(all(generated_tan >= 0), "Monthly TAN values must be non-negative.")
          )
        }
      }

      removal_months <- as.integer(input$removal_months)
      primary <- calculate_storage_scenario(
        scenario = "Primary",
        temps = temps,
        generated_tan = generated_tan,
        defaults = selected_storage_defaults(),
        tref = input$storage_tref,
        eftref = input$storage_eftref,
        storage = storage_multiplier(),
        removal_months = removal_months,
        removal_efficiency = input$removal_efficiency / 100
      )

      batch_parts <- list()
      if (!is.null(uploaded_temperatures)) {
        # Uploaded storage scenarios use the same TAN and removal assumptions
        # as the Primary scenario, changing only the temperature profile.
        for (i in seq_len(nrow(uploaded_temperatures))) {
          batch_parts[[i]] <- calculate_storage_scenario(
            scenario = uploaded_temperatures$Scenario[i],
            temps = as.numeric(uploaded_temperatures[i, month_labels]),
            generated_tan = generated_tan,
            defaults = selected_storage_defaults(),
            tref = input$storage_tref,
            eftref = input$storage_eftref,
            storage = storage_multiplier(),
            removal_months = removal_months,
            removal_efficiency = input$removal_efficiency / 100
          )
        }
      }

      export_rows <- c(list(primary$results, primary$summary), unlist(lapply(batch_parts, function(x) list(x$results, x$summary)), recursive = FALSE))
      list(
        section = "storage",
        primary = primary,
        results_df = primary$results,
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
    if (identical(input$calculation_section, "housing")) "Annual Housing EF" else "Adjusted Annual EF"
  })

  output$summary_ef <- renderText({
    req(input$plot_button > 0)
    results <- calculation_results()
    if (identical(results$section, "housing")) {
      sprintf("%.4f", results$primary$annual_ef)
    } else {
      sprintf("%.2f%%", results$primary$annual_effective_ef)
    }
  })

  output$summary_nh3 <- renderUI({
    req(input$plot_button > 0)
    results <- calculation_results()
    annual_nh3 <- results$primary$annual_nh3
    if (is.na(annual_nh3)) {
      span(if (identical(results$section, "housing")) "Total N needed" else "TAN needed")
    } else {
      HTML(sprintf("%.2f kg NH<sub>3</sub>-N", annual_nh3))
    }
  })

  output$summary_mass_label <- renderText({
    if (identical(input$calculation_section, "housing")) "Input Basis" else "Ending Stored TAN"
  })

  output$summary_mass <- renderText({
    req(input$plot_button > 0)
    results <- calculation_results()
    if (identical(results$section, "housing")) {
      "Total N"
    } else if (is.na(results$primary$end_stored_tan)) {
      "TAN needed"
    } else {
      sprintf("%.2f kg N", results$primary$end_stored_tan)
    }
  })

  output$plot_caption <- renderText({
    if (identical(input$calculation_section, "housing")) {
      "Primary scenario only. Uploaded temperature scenarios are exported but not plotted."
    } else {
      "Solid line: adjusted EF (%). Thin line: base EF (%). Dashed line: manure temperature (deg C). Primary scenario only."
    }
  })

  output$ef_plot <- renderPlot({
    req(input$plot_button > 0)
    results <- calculation_results()
    df <- results$results_df
    x_vals <- seq_along(month_labels)

    if (identical(results$section, "housing")) {
      ef_values <- df$Corrected_EF_TotalN
      temp_values <- df$Temperature_C
      ef_range <- range(ef_values, na.rm = TRUE)
      if (diff(ef_range) == 0) ef_range <- ef_range + c(-0.01, 0.01)
      par(mar = c(4.5, 4.8, 4.5, 4.8))
      plot(
        x_vals, ef_values,
        type = "o", pch = 16, lwd = 2.8, col = "#2E5E4E",
        xaxt = "n", xlab = "Month", ylab = "EF (kg NH3-N/kg Total N)",
        ylim = ef_range + c(-0.1, 0.1) * diff(ef_range),
        main = expression("Monthly Housing " * NH[3] * " Emission Factor")
      )
      axis(1, at = x_vals, labels = month_labels)
      temp_range <- range(temp_values, na.rm = TRUE)
      if (diff(temp_range) == 0) temp_range <- temp_range + c(-1, 1)
      temp_to_ef <- function(x) (x - temp_range[1]) / diff(temp_range) * diff(ef_range) + ef_range[1]
      temp_breaks <- pretty(temp_range)
      lines(x_vals, temp_to_ef(temp_values), type = "o", pch = 1, lwd = 2, lty = 2, col = "#C06A2B")
      axis(side = 4, at = temp_to_ef(temp_breaks), labels = round(temp_breaks, 1), col.axis = "#C06A2B")
      mtext("Temperature (deg C)", side = 4, line = 3, col = "#C06A2B")
      legend(
        "topleft",
        legend = c("Corrected EF", "Temperature"),
        col = c("#2E5E4E", "#C06A2B"),
        lty = c(1, 2),
        lwd = c(2.8, 2),
        pch = c(16, 1),
        bty = "n"
      )
      box()
    } else {
      adjusted_ef <- df$Adjusted_EF_percent
      base_ef <- df$Base_EF_percent
      temp_values <- df$Temperature_C
      ef_range <- range(c(adjusted_ef, base_ef), na.rm = TRUE)
      temp_range <- range(temp_values, na.rm = TRUE)
      if (diff(ef_range) == 0) ef_range <- ef_range + c(-1, 1)
      if (diff(temp_range) == 0) temp_range <- temp_range + c(-1, 1)
      ef_padding <- diff(ef_range) * 0.12
      temp_breaks <- pretty(temp_range)
      temp_to_ef <- function(x) (x - temp_range[1]) / diff(temp_range) * diff(ef_range) + ef_range[1]

      par(mar = c(4.5, 4.8, 4.5, 4.8))
      plot(
        x_vals, adjusted_ef,
        type = "o", pch = 16, lwd = 2.8, col = "#2E5E4E",
        xaxt = "n", xlab = "Month", ylab = "EF (%)",
        ylim = c(ef_range[1] - ef_padding * 0.25, ef_range[2] + ef_padding),
        main = expression("Monthly Storage " * NH[3] * " Emission Factor")
      )
      axis(1, at = x_vals, labels = month_labels)
      lines(x_vals, base_ef, type = "o", pch = 16, lwd = 1.5, col = "#8FA99A")
      lines(x_vals, temp_to_ef(temp_values), type = "o", pch = 1, lwd = 2, lty = 2, col = "#C06A2B")
      axis(side = 4, at = temp_to_ef(temp_breaks), labels = round(temp_breaks, 1), col.axis = "#C06A2B")
      mtext("Manure temperature (deg C)", side = 4, line = 3, col = "#C06A2B")
      legend(
        "topleft",
        legend = c("Adjusted EF (%)", "Base EF (%)", "Manure temperature"),
        col = c("#2E5E4E", "#8FA99A", "#C06A2B"),
        lty = c(1, 1, 2),
        lwd = c(2.8, 1.5, 2),
        pch = c(16, 16, 1),
        bty = "n"
      )
      box()
    }
  })

  output$results_table <- renderTable({
    req(input$plot_button > 0)
    results <- calculation_results()
    df <- rbind(results$results_df, results$primary$summary)
    numeric_cols <- vapply(df, is.numeric, logical(1))
    df[numeric_cols] <- lapply(df[numeric_cols], function(col) round(col, 4))
    df
  }, striped = TRUE, bordered = TRUE, spacing = "s")

  output$download_results <- downloadHandler(
    filename = function() {
      paste0("nh3_results_", input$calculation_section, "_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(calculation_results()$export_df, file, row.names = FALSE, na = "")
    }
  )
}

