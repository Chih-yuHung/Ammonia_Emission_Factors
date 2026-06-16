# Create value-labelled heat maps for the annual housing EF comparison tables.
# The four output PNGs correspond to the four capped EF columns used on P2-P5
# of Proposed_Tier2_Housing_Annual_Detailed.pptx.

library(grid)

input_csv <- file.path("Outputs", "Proposed_Tier2_Housing_Annual_Detail.csv")
output_dir <- file.path("Outputs", "Heatmaps")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)

detail <- read.csv(
  input_csv,
  stringsAsFactors = FALSE,
  check.names = FALSE,
  na.strings = c("", "NA")
)

climate_order <- unique(detail$Climate_Column)

display_na <- function(x) {
  ifelse(is.na(x) | x == "", "NA", x)
}

row_key <- function(data) {
  paste(
    display_na(data$Animal_Type),
    display_na(data$Housing),
    display_na(data$Manure_type),
    sep = "||"
  )
}

detail$row_key <- row_key(detail)
row_meta <- detail[!duplicated(detail$row_key), c("row_key", "Animal_Type", "Housing", "Manure_type")]
row_meta$Animal_Type <- display_na(row_meta$Animal_Type)
row_meta$Housing <- display_na(row_meta$Housing)
row_meta$Manure_type <- display_na(row_meta$Manure_type)

short_climate <- c(
  Boreal_Moist = "Boreal\nMoist",
  Boreal_Dry = "Boreal\nDry",
  Cool_Temp_Moist = "Cool T.\nMoist",
  Cool_Temp_Dry = "Cool T.\nDry",
  Warm_Temp_Moist = "Warm T.\nMoist",
  Warm_Temp_Dry = "Warm T.\nDry",
  Tropical_Wet = "Tropical\nWet",
  Tropical_Moist = "Tropical\nMoist",
  Tropical_Dry = "Tropical\nDry"
)

palette_fun <- colorRampPalette(c(
  "#dce9db",
  "#f0e3b5",
  "#d6a84f",
  "#b85c38"
))
heat_palette <- palette_fun(101)

cell_fill <- function(value) {
  if (is.na(value)) {
    return("#efebe3")
  }
  if (value >= 1) {
    return("#7a2630")
  }
  index <- round(pmin(pmax(value, 0), 0.75) / 0.75 * 100) + 1
  heat_palette[index]
}

cell_text_color <- function(value) {
  if (is.na(value)) {
    return("#66717a")
  }
  if (value >= 0.5) {
    return("#ffffff")
  }
  "#1f252b"
}

format_ef <- function(value) {
  ifelse(is.na(value), "NA", sprintf("%.3f", value))
}

build_matrix <- function(value_col) {
  values <- matrix(
    NA_real_,
    nrow = nrow(row_meta),
    ncol = length(climate_order),
    dimnames = list(row_meta$row_key, climate_order)
  )

  for (i in seq_len(nrow(detail))) {
    values[detail$row_key[i], detail$Climate_Column[i]] <- as.numeric(detail[[value_col]][i])
  }

  values
}

draw_text <- function(label, x, y, width, height, size, color = "#1f252b",
                      fontface = "plain", just = "centre", vjust = "centre") {
  grid.text(
    label,
    x = unit(x + width / 2, "npc"),
    y = unit(y - height / 2, "npc"),
    just = just,
    gp = gpar(fontsize = size, col = color, fontface = fontface, lineheight = 0.95)
  )
}

draw_left_text <- function(label, x, y, width, height, size, color = "#1f252b",
                           fontface = "plain") {
  grid.text(
    label,
    x = unit(x + 0.004, "npc"),
    y = unit(y - height / 2, "npc"),
    just = c("left", "centre"),
    gp = gpar(fontsize = size, col = color, fontface = fontface, lineheight = 0.95)
  )
}

draw_rect <- function(x, y, width, height, fill, border = "#ffffff", lwd = 0.5) {
  grid.rect(
    x = unit(x, "npc"),
    y = unit(y, "npc"),
    width = unit(width, "npc"),
    height = unit(height, "npc"),
    just = c("left", "top"),
    gp = gpar(fill = fill, col = border, lwd = lwd)
  )
}

draw_heatmap <- function(value_col, title, subtitle, file_name, accent) {
  values <- build_matrix(value_col)
  output_path <- file.path(output_dir, file_name)

  png(output_path, width = 3200, height = 1900, res = 350, bg = "#f7f6f1")
  grid.newpage()

  # Page title and diagnostics.
  # grid.text(
  #   title,
  #   x = unit(0.035, "npc"),
  #   y = unit(0.955, "npc"),
  #   just = c("left", "top"),
  #   gp = gpar(fontsize = 18, fontface = "bold", col = "#1f252b")
  # )
  # grid.text(
  #   subtitle,
  #   x = unit(0.035, "npc"),
  #   y = unit(0.922, "npc"),
  #   just = c("left", "top"),
  #   gp = gpar(fontsize = 9.5, col = "#66717a")
  # )

  flat_values <- as.numeric(values)
  capped_count <- sum(flat_values >= 1, na.rm = TRUE)
  max_value <- max(flat_values, na.rm = TRUE)
  avg_value <- mean(flat_values, na.rm = TRUE)
  metric_labels <- c("capped cells", "max EF", "mean EF")
  metric_values <- c(capped_count, sprintf("%.3f", max_value), sprintf("%.3f", avg_value))

  for (i in seq_along(metric_labels)) {
    x <- 0.68 + (i - 1) * 0.10
    grid.lines(
      x = unit(c(x, x), "npc"),
      y = unit(c(0.905, 0.965), "npc"),
      gp = gpar(col = if (i == 1) accent else "#d8d1c5", lwd = 1)
    )
    grid.text(
      metric_values[i],
      x = unit(x + 0.012, "npc"),
      y = unit(0.952, "npc"),
      just = c("left", "top"),
      gp = gpar(fontsize = 15, fontface = "bold", col = "#1f252b")
    )
    grid.text(
      metric_labels[i],
      x = unit(x + 0.012, "npc"),
      y = unit(0.923, "npc"),
      just = c("left", "top"),
      gp = gpar(fontsize = 12, col = "#66717a")
    )
  }

  # Table geometry.
  x0 <- 0.025
  y_top <- 0.855
  y_bottom <- 0.070
  header_h <- 0.052
  row_h <- (y_top - y_bottom - header_h) / nrow(values)
  w_animal <- 0.125
  w_housing <- 0.125
  w_manure <- 0.065
  w_cell <- (0.975 - x0 - w_animal - w_housing - w_manure) / ncol(values)
  widths <- c(w_animal, w_housing, w_manure, rep(w_cell, ncol(values)))

  # Header row.
  draw_rect(x0, y_top, sum(widths), header_h, accent, border = accent, lwd = 0)
  headers <- c("Animal type", "Housing", "Manure", unname(short_climate[climate_order]))
  x <- x0
  for (j in seq_along(headers)) {
    draw_text(headers[j], x, y_top, widths[j], header_h, 7.3, "#ffffff", "bold")
    x <- x + widths[j]
  }

  # Body rows.
  for (i in seq_len(nrow(values))) {
    y <- y_top - header_h - (i - 1) * row_h
    label_fill <- if (i %% 2 == 0) "#ffffff" else "#f1eee7"
    draw_rect(x0, y, w_animal + w_housing + w_manure, row_h, label_fill, border = "#ffffff")
    draw_left_text(row_meta$Animal_Type[i], x0, y, w_animal, row_h, 6.8, "#1f252b", "bold")
    draw_left_text(row_meta$Housing[i], x0 + w_animal, y, w_housing, row_h, 6.5, "#1f252b")
    draw_text(row_meta$Manure_type[i], x0 + w_animal + w_housing, y, w_manure, row_h, 6.4, "#66717a")

    x <- x0 + w_animal + w_housing + w_manure
    for (j in seq_len(ncol(values))) {
      value <- values[i, j]
      draw_rect(x, y, w_cell, row_h, cell_fill(value), border = "#ffffff", lwd = 0.35)
      draw_text(format_ef(value), x, y, w_cell, row_h, 6.8, cell_text_color(value),
                ifelse(!is.na(value) && value >= 1, "bold", "plain"))
      x <- x + w_cell
    }
  }

  legend_y <- 0.035
  grid.text(
    "Heat scale: green = low EF, gold = moderate, orange = high, red = capped at 1.000. Values are EFs_TotalN rounded to 3 decimals.",
    x = unit(0.025, "npc"),
    y = unit(legend_y, "npc"),
    just = c("left", "centre"),
    gp = gpar(fontsize = 12.0 , col = "#66717a")
  )

  dev.off()
  output_path
}

heatmap_specs <- data.frame(
  value_col = c(
    "EF_CurrentCT_CoolRef_Capped",
    "EF_CurrentCT_15CRef_Capped",
    "EF_FigCT_CoolRef_Capped",
    "EF_FigCT_15CRef_Capped"
  ),
  title = c(
    "Current CT | Cool Temperate Moist reference",
    "Current CT | 15 C reference",
    "Fig. 4B.1 CT | Cool Temperate Moist reference",
    "Fig. 4B.1 CT | 15 C reference"
  ),
  subtitle = c(
    "Annual EF by animal, housing, manure type, and climate zone; current correction factors and annual Tref = 6.858 C.",
    "Annual EF by animal, housing, manure type, and climate zone; current correction factors and annual Tref = 15 C.",
    "Annual EF by animal, housing, manure type, and climate zone; approximate Fig. 4B.1 CT values and annual Tref = 6.858 C.",
    "Annual EF by animal, housing, manure type, and climate zone; approximate Fig. 4B.1 CT values and annual Tref = 15 C."
  ),
  file_name = c(
    "P2_CurrentCT_CoolRef_Heatmap.png",
    "P3_CurrentCT_15CRef_Heatmap.png",
    "P4_FigCT_CoolRef_Heatmap.png",
    "P5_FigCT_15CRef_Heatmap.png"
  ),
  accent = c("#176b87", "#b85c38", "#d6a84f", "#4f7c67"),
  stringsAsFactors = FALSE
)

created_files <- vapply(
  seq_len(nrow(heatmap_specs)),
  function(i) {
    draw_heatmap(
      value_col = heatmap_specs$value_col[i],
      title = heatmap_specs$title[i],
      subtitle = heatmap_specs$subtitle[i],
      file_name = heatmap_specs$file_name[i],
      accent = heatmap_specs$accent[i]
    )
  },
  character(1)
)

cat(paste(created_files, collapse = "\n"), "\n")
