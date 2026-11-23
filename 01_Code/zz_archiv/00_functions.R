########################################################################
# Libraries
########################################################################
library(data.table)   
library(openxlsx)     
library(DBI)         
library(dplyr)        
library(glue)        
library(duckdb)
library(fst)
library(magrittr)
library(ggplot2)
library(arrow)
library(stringr)
library(fca)
library(sf)
library(terra)
library(tmap)
library(fs)
library(qs)
library(readxl)
library(writexl)
library(digest)
library(rlang)
library(tidyr)
library(lubridate)
library(clipr)
library(forcats)
library (janitor)
library(DT)
library(DiagrammeR)
library(DiagrammeRsvg)
library(rsvg)
library(patchwork)
library(survival)
library(survminer)
library(purrr)

########################################################################
# Options
########################################################################
options(scipen = 999)


########################################################################
# Global ggplot defaults 
########################################################################
ggplot2::theme_set(
  ggplot2::theme_minimal(
    base_size   = 9,
    base_family = "sans"
  )
)
########################################################################
# general helpers
########################################################################
require_params <- function(vars) {
  missing <- vars[!vapply(vars, exists, logical(1), inherits = TRUE)]
  if (length(missing) > 0) {
    stop("Missing parameter(s): ", paste(missing, collapse = ", "))
  }
}

########################################################################
# freq table
########################################################################
freq_table <- function(data, var, by = NULL, totals = "row") {
  
  var <- rlang::enquo(var)
  by  <- rlang::enquo(by)
  
  data <- data |>
    mutate(
      !!var := if (is.factor(!!var)) {
        forcats::fct_na_value_to_level(!!var, level = "Missing")
      } else {
        tidyr::replace_na(as.character(!!var), "Missing")
      }
    )
  
  if (rlang::quo_is_null(by)) {
    
    tab <- data |>
      count(!!var, .drop = FALSE) |>
      mutate(percent = round(n / sum(n) * 100, 1))
    
    if (totals %in% c("row", "both")) {
      tab <- tab |>
        add_row(
          !!var := "Total",
          n = sum(tab$n),
          percent = 100
        )
    }
    
    return(tab)
  }
  
  tab <- data |>
    count(!!var, !!by, .drop = FALSE) |>
    tidyr::pivot_wider(
      names_from  = !!by,
      values_from = n,
      values_fill = 0
    )
  
  if (totals == "row") {
    tab <- tab |> janitor::adorn_totals("row")
  }
  
  if (totals == "col") {
    tab <- tab |> janitor::adorn_totals("col")
  }
  
  if (totals == "both") {
    tab <- tab |> janitor::adorn_totals(c("row", "col"))
  }
  
  tab
}
########################################################################
# data.table helpers
########################################################################
char_cols <- function(dt) {
  sapply(dt, is.character) %>%
    which() %>%
    names(dt)[.]
}

show_na <- function(dt) {
  stopifnot(is.data.table(dt))
  out <- dt[, lapply(.SD, function(x) sum(is.na(x)))]
  out <- unlist(out)
  out <- out[out > 0]
  out
}

show_duplicated <- function(dt, by, ...) {
  stopifnot(is.data.table(dt))
  dt2 <- copy(dt)
  setorderv(dt2, cols = by)
  dt2[, dups := .N, by = eval(by)]
  dt2[dups > 1]
}

firstnm <- function(x) {
  x <- data.table::first(x[!is.na(x)])
  if (length(x) == 0) x <- NA
  return(x)
}

dt_table <- function(dt, cols = NULL, pattern = NULL, ...) {
  if (is.null(cols) && is.null(pattern)) stop("use either 'col' oder 'pattern'")
  if (!is.null(cols)) vars <- cols
  if (!is.null(pattern)) vars <- str_subset(names(dt), pattern)
  for (v in vars) {
    cat(v, ":\n")
    print(table(dt[[v]], ...))
  }
}

duplicatedN <- function(dt, by, ...) {
  x <- sum(duplicated(dt, by = by, ...), na.rm = TRUE)
  cat(x, " duplicates, ", dt[, .N], " observations.\n")
}

st_merge <- function(x, y, by, gen = NULL, verbose = TRUE, ...) {
  
  stopifnot(is.data.table(x) || is.data.table(y))
  
  setkeyv(x, by)
  setkeyv(y, by)
  x[, x_m := 1]
  y[, y_m := 2]
  
  dt <- merge.data.table(x, y, by = by, ...)
  dt[, .merge := fifelse(is.na(x_m), 0, x_m) + fifelse(is.na(y_m), 0, y_m)]
  dt[, c("x_m", "y_m") := NULL]
  x[, c("x_m") := NULL]
  y[, c("y_m") := NULL]
  
  
  # print statistics
  cat("no. of duplicate keys in x:", sum(duplicated(x, by = by), na.rm = TRUE), "\n")
  cat("no. of duplicate keys in y:", sum(duplicated(y, by = by), na.rm = TRUE), "\n")
  
  N <- sapply(1:3, dt[, function(x) sum(.merge == x)])
  if (N[[3]] > 0) cat("merged:", N[[3]], "\n")
  if (N[[1]] > 0) cat("unmatched in x:", N[[1]], "\n")
  if (N[[2]] > 0) cat("unmatched in y:", N[[2]], "\n")
  
  # print unmatched
  if (verbose == TRUE) {
    if (N[[1]] > 0) print(dt[.merge == 1][1:(min(.N, 5)), ])
    if (N[[2]] > 0) print(dt[.merge == 2][1:(min(.N, 5)), ])
  }
  
  return(dt)
}

########################################################################
# Excel helpers
########################################################################

# ------------------------------------------------------------
# Autostyle (typ-basiert)
# ------------------------------------------------------------
autostyle <- function(wb, sheet, data, start_row = 1) {
  
  percent <- createStyle(numFmt = "0.0%")
  large   <- createStyle(numFmt = "###,##0")
  medium  <- createStyle(numFmt = "##0")
  small   <- createStyle(numFmt = "##0.00")
  datefmt <- createStyle(numFmt = "yyyy-mm-dd")
  
  font  <- createStyle(fontName = "Arial", fontSize = 10)
  right <- createStyle(halign = "right")
  left  <- createStyle(halign = "left")
  
  header_row     <- start_row
  first_data_row <- start_row + 1
  last_row       <- start_row + nrow(data)
  
  # Schrift und Grösse
  addStyle(
    wb, sheet, style = font,
    rows = header_row:(last_row + 10),
    cols = 1:ncol(data),
    gridExpand = TRUE, stack = TRUE
  )
  
  lapply(seq_len(ncol(data)), function(j) {
    
    cat(sheet, ":", names(data)[j], "\n")
    x <- data[[j]]
    
    # factor wie character behandeln
    if (is.factor(x)) x <- as.character(x)
    
    # --- Ausrichtung nach Typ ---
    if (is.character(x)) {
      addStyle(wb, sheet, style = left, rows = header_row:last_row, cols = j, stack = TRUE)
      return(NULL)
    }
    
    if (inherits(x, "Date") || inherits(x, "POSIXct") || inherits(x, "POSIXt")) {
      addStyle(wb, sheet, style = right, rows = header_row:last_row, cols = j, stack = TRUE)
      addStyle(wb, sheet, style = datefmt, rows = first_data_row:last_row, cols = j, stack = TRUE)
      return(NULL)
    }
    
    if (is.logical(x)) {
      addStyle(wb, sheet, style = right, rows = header_row:last_row, cols = j, stack = TRUE)
      return(NULL)
    }
    
    if (is.numeric(x) || is.integer(x)) {
      
      addStyle(wb, sheet, style = right, rows = header_row:last_row, cols = j, stack = TRUE)
      
      x2 <- x[!is.na(x)]
      if (length(x2) > 0) x2 <- sample(x2, size = min(20, length(x2)))
      
      if (length(x2) == 0) {
        NULL
      } else if (setequal(unique(x2), c(0, 1))) {
        NULL
      } else if (max(x2) <= 1 && min(x2) >= 0 && data.table::uniqueN(x2) > 2) {
        addStyle(wb, sheet, style = percent, rows = first_data_row:last_row, cols = j, stack = TRUE)
      } else if (max(x2) >= 10000) {
        addStyle(wb, sheet, style = large, rows = first_data_row:last_row, cols = j, stack = TRUE)
      } else if (sum(x2) == sum(floor(x2)) && max(x2) < 10000) {
        addStyle(wb, sheet, style = medium, rows = first_data_row:last_row, cols = j, stack = TRUE)
      } else {
        addStyle(wb, sheet, style = small, rows = first_data_row:last_row, cols = j, stack = TRUE)
      }
      
      return(NULL)
    }
    
    # Fallback: links
    addStyle(wb, sheet, style = left, rows = header_row:last_row, cols = j, stack = TRUE)
    NULL
  })
  
  # freezePane wird im Writer gesetzt (damit Titelzeile korrekt berücksichtigt wird)
}


# ------------------------------------------------------------
# Writer: Update/Replace pro Sheet, optional Titelzeile(n)
# - title kann NULL, ein String oder ein Character-Vektor sein
# - Tabelle beginnt automatisch unter dem Titel
# ------------------------------------------------------------
write_excel_tables_update <- function(list, file, autostyle = TRUE,
                                      title = NULL,
                                      title_style = NULL,
                                      freeze_after_header = TRUE,
                                      verbose = FALSE) {
  
  stopifnot(is.list(list))
  if (is.data.frame(list)) list <- list(list)
  if (is.null(names(list))) names(list) <- paste0("Tabelle", 1:length(list))
  
  # Namen kürzen auf 31 Zeichen
  names(list) <- sapply(names(list), function(x) substring(x, 1, 31))
  
  wb <- if (file.exists(file)) loadWorkbook(file) else createWorkbook()
  
  if (is.null(title_style)) {
    title_style <- createStyle(
      fontName = "Arial",
      fontSize = 10,
      textDecoration = "bold",
      valign = "top",
      wrapText = TRUE
    )
  }
  
  for (t in names(list)) {
    
    if (isTRUE(verbose)) cat(t, "\n")
    
    # Sheet ersetzen falls vorhanden
    if (t %in% names(wb)) removeWorksheet(wb, t)
    addWorksheet(wb, t)
    
    # Titel schreiben (optional)
    title_rows <- 0
    if (!is.null(title)) {
      title_vec <- as.character(title)
      title_rows <- length(title_vec)
      
      # Titel in Spalte A, mehrere Zeilen möglich
      for (i in seq_along(title_vec)) {
        writeData(wb, sheet = t, x = title_vec[i], startRow = i, startCol = 1, colNames = FALSE)
        addStyle(wb, sheet = t, style = title_style, rows = i, cols = 1, stack = TRUE)
      }
      
      # Spalte A breiter (hilft für Titel)
      setColWidths(wb, t, cols = 1, widths = 60)
    }
    
    # Tabellenstart: Header-Zeile
    table_start <- if (title_rows > 0) title_rows + 1 else 1
    
    writeDataTable(
      wb, sheet = t, x = list[[t]],
      startRow = table_start,
      startCol = 1
    )
    
    if (autostyle) autostyle(wb, t, list[[t]], start_row = table_start)
    
    # Freeze: oben bleibt Titel + Header sichtbar
    if (freeze_after_header) {
      freezePane(wb, t, firstActiveRow = table_start + 1)
    }
  }
  
  saveWorkbook(wb, file = file, overwrite = TRUE)
}



#######################################################################
# flow chart functionn
#######################################################################
# ---------------------------------------------------------------------
# Helper: add one step to a participant-flow object
# ---------------------------------------------------------------------
add_flow_step <- function(
    flow_step,
    current_flow = NULL,
    data = NULL,
    output_tibble = "analysis_data",
    keep_criteria_label,
    keep_criteria = TRUE,
    keep_criteria_type = c("source", "eligibility", "analysis"),
    population = NA_character_,
    exclude_criteria_label = "Excluded",
    by_year = TRUE,
    by_year_var = survey_year
) {
  
  keep_criteria_type <- match.arg(keep_criteria_type)
  
  step_id <- flow_step
  by_year_var_name <- rlang::as_name(rlang::enquo(by_year_var))
  
  if (step_id == 0) {
    
    if (!is.null(current_flow)) {
      stop("'current_flow' must be NULL for flow_step = 0.")
    }
    
    if (is.null(data)) {
      stop("'data' must be supplied for flow_step = 0.")
    }
    
    analysis_data <- data
    flow_log <- tibble()
    
  } else {
    
    if (is.null(current_flow)) {
      stop("'current_flow' must be supplied for flow_step > 0.")
    }
    
    expected_step <- max(current_flow$flow_log$flow_step) + 1L
    
    if (step_id != expected_step) {
      stop(
        "Expected flow_step = ", expected_step,
        ", but received ", step_id, "."
      )
    }
    
    if (!identical(by_year, current_flow$by_year)) {
      stop("'by_year' must be identical across all flow steps.")
    }
    
    if (!identical(by_year_var_name, current_flow$by_year_var)) {
      stop("'by_year_var' must be identical across all flow steps.")
    }
    
    analysis_data <- current_flow[[current_flow$output_tibble]]
    flow_log <- current_flow$flow_log
  }
  
  keep_criteria_text <- rlang::expr_text(
    rlang::enexpr(keep_criteria)
  )
  
  if (by_year) {
    
    if (step_id == 0) {
      
      flow_step_tbl <- analysis_data |>
        count({{ by_year_var }}, name = "remaining") |>
        rename(year = {{ by_year_var }}) |>
        mutate(excluded = 0L)
      
    } else {
      
      n_before <- analysis_data |>
        count({{ by_year_var }}, name = "n_before") |>
        rename(year = {{ by_year_var }})
      
      analysis_data <- analysis_data |>
        filter({{ keep_criteria }})
      
      n_after <- analysis_data |>
        count({{ by_year_var }}, name = "remaining") |>
        rename(year = {{ by_year_var }})
      
      flow_step_tbl <- n_before |>
        full_join(n_after, by = "year") |>
        mutate(
          remaining = tidyr::replace_na(remaining, 0L),
          excluded = n_before - remaining
        ) |>
        select(year, excluded, remaining)
    }
    
  } else {
    
    if (step_id == 0) {
      
      flow_step_tbl <- tibble(
        excluded = 0L,
        remaining = nrow(analysis_data)
      )
      
    } else {
      
      n_before <- nrow(analysis_data)
      
      analysis_data <- analysis_data |>
        filter({{ keep_criteria }})
      
      flow_step_tbl <- tibble(
        excluded = n_before - nrow(analysis_data),
        remaining = nrow(analysis_data)
      )
    }
  }
  
  flow_step_tbl <- flow_step_tbl |>
    mutate(
      flow_step = step_id,
      keep_criteria_label = keep_criteria_label,
      keep_criteria = if (step_id == 0) "TRUE" else keep_criteria_text,
      keep_criteria_type = keep_criteria_type,
      population = population,
      exclude_criteria_label = exclude_criteria_label,
      .before = 1
    )
  
  output <- list(
    flow_log = bind_rows(flow_log, flow_step_tbl),
    by_year = by_year,
    by_year_var = by_year_var_name,
    output_tibble = output_tibble
  )
  
  output[[output_tibble]] <- analysis_data
  
  output
}

# ---------------------------------------------------------------------
# Helper: plot STROBE-style participant flow
# ---------------------------------------------------------------------
plot_strobe_flow <- function(
    flow,
    file_png,
    file_pdf = NULL,
    title = NULL,
    by_year = FALSE,
    
    main_box_fill = "white",
    exclusion_box_fill = "white",
    main_box_border = "black",
    exclusion_box_border = "black",
    
    color_source_population = "#F5F5F5",
    color_study_population = "#FAF6E8",
    color_analysis_population = "#EEF5EC",
    
    source_strip_fill = "#808080",
    eligibility_strip_fill = "#005095",
    analysis_strip_fill = "#5F8E5B",
    strip_width = 0.15,
    
    show_percentages = TRUE,
    percentage_accuracy = 0.1,
    
    show_legend = TRUE,
    legend_text_size = 3.0,
    legend_y_gap = 1.5,
    bottom_margin = 0.6
) {
  
  if (!"flow_log" %in% names(flow)) {
    stop("'flow' must contain a 'flow_log' element.")
  }
  
  if (by_year && !isTRUE(flow$by_year)) {
    stop("The flow object was created with by_year = FALSE.")
  }
  
  flow_log <- flow$flow_log
  
  required_vars <- c(
    "flow_step",
    "keep_criteria_label",
    "keep_criteria_type",
    "population",
    "exclude_criteria_label",
    "excluded",
    "remaining"
  )
  
  missing_vars <- setdiff(required_vars, names(flow_log))
  
  if (length(missing_vars) > 0) {
    stop(
      "flow$flow_log is missing required variable(s): ",
      paste(missing_vars, collapse = ", ")
    )
  }
  
  if (by_year && !"year" %in% names(flow_log)) {
    stop("by_year = TRUE requires a 'year' variable in flow$flow_log.")
  }
  
  make_single_chart <- function(
    flow_plot,
    file_png,
    file_pdf = NULL,
    title = NULL
  ) {
    
    flow_plot <- flow_plot |>
      arrange(flow_step) |>
      mutate(
        previous_remaining = lag(remaining)
      )
    
    source_n <- flow_plot |>
      filter(population == "Source population") |>
      slice(1) |>
      pull(remaining)
    
    study_row <- flow_plot |>
      filter(population == "Study population") |>
      slice(1)
    
    study_n <- if (nrow(study_row) == 0) NA_real_ else study_row$remaining
    study_step <- if (nrow(study_row) == 0) Inf else study_row$flow_step
    
    df <- flow_plot |>
      mutate(
        y = rev(row_number()),
        
        box_fill = case_when(
          population == "Source population" ~ color_source_population,
          population == "Study population" ~ color_study_population,
          population == "Analysis population" ~ color_analysis_population,
          TRUE ~ main_box_fill
        ),
        
        strip_fill = case_when(
          keep_criteria_type == "source" ~ source_strip_fill,
          keep_criteria_type == "eligibility" ~ eligibility_strip_fill,
          keep_criteria_type == "analysis" ~ analysis_strip_fill,
          TRUE ~ NA_character_
        ),
        
        pct_source_label = case_when(
          show_percentages &
            length(source_n) == 1 &
            !is.na(source_n) ~
            paste0(
              scales::number(
                100 * remaining / source_n,
                accuracy = percentage_accuracy
              ),
              "% of source population"
            ),
          TRUE ~ NA_character_
        ),
        
        pct_study_label = case_when(
          show_percentages &
            !is.na(study_n) &
            flow_step >= study_step ~
            paste0(
              scales::number(
                100 * remaining / study_n,
                accuracy = percentage_accuracy
              ),
              "% of study population"
            ),
          TRUE ~ NA_character_
        ),
        
        pct_excluded_previous = case_when(
          show_percentages &
            !is.na(previous_remaining) &
            previous_remaining > 0 ~
            paste0(
              scales::number(
                100 * excluded / previous_remaining,
                accuracy = percentage_accuracy
              ),
              "% of previous population"
            ),
          TRUE ~ NA_character_
        ),
        
        percentage_label = case_when(
          !is.na(pct_source_label) &
            !is.na(pct_study_label) ~
            paste0(
              "\n",
              pct_source_label,
              "\n",
              pct_study_label
            ),
          
          !is.na(pct_source_label) ~
            paste0(
              "\n",
              pct_source_label
            ),
          
          TRUE ~ ""
        ),
        
        excl_percentage_label = case_when(
          !is.na(pct_excluded_previous) ~
            paste0(
              "\n",
              pct_excluded_previous
            ),
          TRUE ~ ""
        ),
        
        main_label = paste0(
          keep_criteria_label,
          "\n",
          "n = ", scales::comma(remaining),
          percentage_label
        ),
        
        excl_label = paste0(
          exclude_criteria_label,
          "\n",
          "n = ", scales::comma(excluded),
          excl_percentage_label
        )
      )
  
    
    main_boxes <- df
    excl_boxes <- df |>
      filter(excluded > 0)
    
    text_x <- (strip_width + 4.2) / 2
    legend_space <- if (show_legend) legend_y_gap else 0
    
    p <- ggplot() +
      
      geom_rect(
        data = main_boxes,
        aes(
          xmin = 0,
          xmax = 4.2,
          ymin = y - 0.36,
          ymax = y + 0.36,
          fill = box_fill
        ),
        colour = main_box_border,
        linewidth = 0.4
      ) +
      
      geom_rect(
        data = main_boxes,
        aes(
          xmin = 0,
          xmax = strip_width,
          ymin = y - 0.36,
          ymax = y + 0.36,
          fill = strip_fill
        ),
        colour = NA
      ) +
      
      scale_fill_identity() +
      
      geom_text(
        data = main_boxes,
        aes(
          x = text_x,
          y = y,
          label = main_label
        ),
        size = 2.9,
        lineheight = 1.05
      ) +
      
      geom_segment(
        data = main_boxes |>
          filter(flow_step < max(flow_step)),
        aes(
          x = 2.1,
          xend = 2.1,
          y = y - 0.38,
          yend = y - 0.62
        ),
        arrow = arrow(length = unit(0.10, "inches")),
        linewidth = 0.35
      ) +
      
      geom_segment(
        data = excl_boxes,
        aes(
          x = 4.2,
          xend = 5.2,
          y = y,
          yend = y
        ),
        arrow = arrow(length = unit(0.10, "inches")),
        linewidth = 0.35
      ) +
      
      geom_rect(
        data = excl_boxes,
        aes(
          xmin = 5.2,
          xmax = 7.8,
          ymin = y - 0.28,
          ymax = y + 0.28
        ),
        fill = exclusion_box_fill,
        colour = exclusion_box_border,
        linewidth = 0.4
      ) +
      
      geom_text(
        data = excl_boxes,
        aes(
          x = 6.5,
          y = y,
          label = excl_label
        ),
        size = 2.9,
        lineheight = 1.05
      )
    
    if (show_legend) {
      
      p <- p +
        geom_segment(
          aes(x = -0.2, xend = 8.0, y = 0.15, yend = 0.15),
          linewidth = 0.35
        ) +
        
        annotate(
          "text",
          x = 0,
          y = -0.15,
          label = "Criterion category",
          hjust = 0,
          fontface = "bold",
          size = legend_text_size
        ) +
        annotate(
          "rect",
          xmin = 0,
          xmax = 0.25,
          ymin = -0.45,
          ymax = -0.30,
          fill = source_strip_fill,
          colour = "black",
          linewidth = 0.25
        ) +
        annotate(
          "text",
          x = 0.35,
          y = -0.375,
          label = "Source",
          hjust = 0,
          size = legend_text_size
        ) +
        annotate(
          "rect",
          xmin = 0,
          xmax = 0.25,
          ymin = -0.70,
          ymax = -0.55,
          fill = eligibility_strip_fill,
          colour = "black",
          linewidth = 0.25
        ) +
        annotate(
          "text",
          x = 0.35,
          y = -0.625,
          label = "Eligibility",
          hjust = 0,
          size = legend_text_size
        ) +
        annotate(
          "rect",
          xmin = 0,
          xmax = 0.25,
          ymin = -0.95,
          ymax = -0.80,
          fill = analysis_strip_fill,
          colour = "black",
          linewidth = 0.25
        ) +
        annotate(
          "text",
          x = 0.35,
          y = -0.875,
          label = "Analysis",
          hjust = 0,
          size = legend_text_size
        ) +
        
        annotate(
          "text",
          x = 2.6,
          y = -0.15,
          label = "Population",
          hjust = 0,
          fontface = "bold",
          size = legend_text_size
        ) +
        annotate(
          "rect",
          xmin = 2.6,
          xmax = 2.85,
          ymin = -0.45,
          ymax = -0.30,
          fill = color_source_population,
          colour = "black",
          linewidth = 0.25
        ) +
        annotate(
          "text",
          x = 2.95,
          y = -0.375,
          label = "Source population",
          hjust = 0,
          size = legend_text_size
        ) +
        annotate(
          "rect",
          xmin = 2.6,
          xmax = 2.85,
          ymin = -0.70,
          ymax = -0.55,
          fill = color_study_population,
          colour = "black",
          linewidth = 0.25
        ) +
        annotate(
          "text",
          x = 2.95,
          y = -0.625,
          label = "Study population",
          hjust = 0,
          size = legend_text_size
        ) +
        annotate(
          "rect",
          xmin = 2.6,
          xmax = 2.85,
          ymin = -0.95,
          ymax = -0.80,
          fill = color_analysis_population,
          colour = "black",
          linewidth = 0.25
        ) +
        annotate(
          "text",
          x = 2.95,
          y = -0.875,
          label = "Analysis population",
          hjust = 0,
          size = legend_text_size
        ) +
        
        annotate(
          "text",
          x = 5.4,
          y = -0.15,
          label = "Other",
          hjust = 0,
          fontface = "bold",
          size = legend_text_size
        ) +
        annotate(
          "rect",
          xmin = 5.4,
          xmax = 5.65,
          ymin = -0.45,
          ymax = -0.30,
          fill = exclusion_box_fill,
          colour = exclusion_box_border,
          linewidth = 0.25
        ) +
        annotate(
          "text",
          x = 5.75,
          y = -0.375,
          label = "Excluded records",
          hjust = 0,
          size = legend_text_size
        )
    }
    
    p <- p +
      coord_cartesian(
        xlim = c(-0.2, 8.0),
        ylim = c(
          0.4 - legend_space - bottom_margin,
          max(df$y) + 0.7
        ),
        expand = FALSE
      ) +
      theme_void()
    
    if (!is.null(title)) {
      p <- p +
        labs(title = title) +
        theme(
          plot.title = element_text(
            hjust = 0.5,
            face = "bold",
            size = 11
          )
        )
    }
    
    plot_height <- max(
      4,
      nrow(df) * 1.0 +
        ifelse(show_legend, 1.4, 0) +
        bottom_margin * 0.8
    )
    
    ggsave(
      filename = file_png,
      plot = p,
      width = 9,
      height = plot_height,
      dpi = 300,
      bg = "white"
    )
    
    if (!is.null(file_pdf)) {
      ggsave(
        filename = file_pdf,
        plot = p,
        width = 9,
        height = plot_height,
        bg = "white"
      )
    }
    
    invisible(p)
  }
  
  if (!by_year) {
    
    flow_plot <- flow_log |>
      summarise(
        excluded = sum(excluded, na.rm = TRUE),
        remaining = sum(remaining, na.rm = TRUE),
        .by = c(
          flow_step,
          keep_criteria_label,
          keep_criteria_type,
          population,
          exclude_criteria_label
        )
      ) |>
      arrange(flow_step)
    
    make_single_chart(
      flow_plot = flow_plot,
      file_png = file_png,
      file_pdf = file_pdf,
      title = title
    )
    
  } else {
    
    years <- sort(unique(flow_log$year))
    plots <- list()
    
    for (yr in years) {
      
      file_png_year <- sub(
        "\\.png$",
        paste0("_", yr, ".png"),
        file_png
      )
      
      file_pdf_year <- if (!is.null(file_pdf)) {
        sub("\\.pdf$", paste0("_", yr, ".pdf"), file_pdf)
      } else {
        NULL
      }
      
      title_year <- if (!is.null(title)) {
        paste0(title, " - ", yr)
      } else {
        as.character(yr)
      }
      
      flow_plot <- flow_log |>
        filter(year == yr) |>
        arrange(flow_step)
      
      plots[[as.character(yr)]] <- make_single_chart(
        flow_plot = flow_plot,
        file_png = file_png_year,
        file_pdf = file_pdf_year,
        title = title_year
      )
    }
    
    invisible(plots)
  }
}
