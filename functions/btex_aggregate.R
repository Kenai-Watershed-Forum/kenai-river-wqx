# Aggregate volatile (BTEX) results into one total-BTEX row per sampling event.
#
# Rules (plan: .posit/assistant/plans/2026-11-02-btex-alignment-across-years.md)
# - Event = monitoring_location_name + activity_start_date + sample type
#   (field sample vs trip blank; trip blanks have an Activity ID ending in "-Blank").
# - Rows are rounded to 2 decimals. Within an event, results for the same analyte
#   (duplicates) are averaged, with a non-detect counted as 0.
# - If the laboratory-reported total exists for the event, it is used.
#   Otherwise total = benzene + toluene + ethylbenzene + xylenes, where xylenes =
#   total xylene if reported, else o-xylene + m,p-xylene.
# - An event is a non-detect (ND) when every contributing result is ND. ND rows carry
#   an NA value and result_detection_condition_text = "Not Detected" (the plot code
#   draws them at 0).
# - Non-detect results are rows with result_detection_condition_text == "Not Detected".
#   Rows with no value and no detection condition are ignored.

btex_total_name <- "Benzene, toluene, ethyl benzene, xylenes mix"

btex_component_names <- c(
  btex_total_name, "Benzene", "Toluene", "Ethylbenzene",
  "Xylene", "o-Xylene", "m,p-Xylene"
)

# dat: data frame with the columns listed in `needed` below (extra columns are kept).
# trip_blanks: FALSE returns field-sample events, TRUE returns trip-blank events.
# Returns a data frame with the columns of `dat` plus btex_basis and is_trip_blank.
btex_aggregate <- function(dat, trip_blanks = FALSE) {
  needed <- c(
    "monitoring_location_name", "activity_start_date", "activity_identifier",
    "characteristic_name", "result_sample_fraction_text", "result_measure_value",
    "result_detection_condition_text"
  )
  missing_cols <- setdiff(needed, names(dat))
  if (length(missing_cols) > 0) {
    stop("btex_aggregate(): missing columns: ", paste(missing_cols, collapse = ", "))
  }

  vol <- dat |>
    dplyr::filter(
      .data$result_sample_fraction_text %in% "Volatile",
      .data$characteristic_name %in% btex_component_names
    ) |>
    dplyr::mutate(
      is_trip_blank = grepl("-Blank$", .data$activity_identifier),
      nd = .data$result_detection_condition_text %in% "Not Detected",
      usable = .data$nd | !is.na(.data$result_measure_value)
    ) |>
    dplyr::filter(.data$usable, .data$is_trip_blank == trip_blanks)

  if (nrow(vol) == 0) {
    return(dat[0, ] |> dplyr::mutate(btex_basis = character(), is_trip_blank = logical()))
  }

  vol <- vol |>
    dplyr::mutate(val = dplyr::if_else(.data$nd, 0, round(.data$result_measure_value, 2)))

  keys <- c("monitoring_location_name", "activity_start_date")

  # One value per event and analyte (duplicates averaged)
  per_analyte <- vol |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys)), .data$characteristic_name) |>
    dplyr::summarise(
      val = mean(.data$val),
      all_nd = all(.data$nd),
      .groups = "drop"
    )

  pick <- function(name) {
    per_analyte |>
      dplyr::filter(.data$characteristic_name == name) |>
      dplyr::select(dplyr::all_of(keys), val, all_nd) |>
      dplyr::rename_with(~ paste0(.x, "_", name), c("val", "all_nd"))
  }

  events <- per_analyte |> dplyr::distinct(dplyr::across(dplyr::all_of(keys)))
  for (nm in btex_component_names) {
    events <- dplyr::left_join(events, pick(nm), by = keys)
  }

  v <- function(nm) events[[paste0("val_", nm)]]
  n <- function(nm) events[[paste0("all_nd_", nm)]]

  has_total <- !is.na(v(btex_total_name))

  # xylenes: reported total if present, else sum of isomers
  has_xyl_total <- !is.na(v("Xylene"))
  has_iso <- !is.na(v("o-Xylene")) | !is.na(v("m,p-Xylene"))
  xyl_val <- dplyr::case_when(
    has_xyl_total ~ v("Xylene"),
    has_iso ~ rowSums(cbind(v("o-Xylene"), v("m,p-Xylene")), na.rm = TRUE),
    TRUE ~ NA_real_
  )
  xyl_nd <- dplyr::case_when(
    has_xyl_total ~ n("Xylene"),
    has_iso ~ rowSums(cbind(!n("o-Xylene"), !n("m,p-Xylene")), na.rm = TRUE) == 0,
    TRUE ~ NA
  )

  comp_vals <- cbind(v("Benzene"), v("Toluene"), v("Ethylbenzene"), xyl_val)
  comp_nd <- cbind(n("Benzene"), n("Toluene"), n("Ethylbenzene"), xyl_nd)
  has_comp <- rowSums(!is.na(comp_vals)) > 0
  sum_val <- rowSums(comp_vals, na.rm = TRUE)
  sum_nd <- rowSums(!comp_nd, na.rm = TRUE) == 0

  events$btex_value <- dplyr::case_when(
    has_total ~ v(btex_total_name),
    has_comp ~ sum_val,
    TRUE ~ NA_real_
  )
  events$btex_nd <- dplyr::case_when(
    has_total ~ n(btex_total_name),
    has_comp ~ sum_nd,
    TRUE ~ NA
  )
  events$btex_basis <- dplyr::case_when(
    has_total ~ "reported total",
    has_comp ~ "sum of components",
    TRUE ~ NA_character_
  )

  events <- events |>
    dplyr::filter(!is.na(.data$btex_basis)) |>
    dplyr::select(dplyr::all_of(keys), "btex_value", "btex_nd", "btex_basis")

  # Representative row per event: the reported total if present, else first row
  rep_rows <- vol |>
    dplyr::mutate(is_total = .data$characteristic_name == btex_total_name) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(keys)), dplyr::desc(.data$is_total)) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::slice(1) |>
    dplyr::ungroup() |>
    dplyr::select(-dplyr::any_of(c("nd", "usable", "val", "is_total")))

  out <- dplyr::inner_join(rep_rows, events, by = keys) |>
    dplyr::mutate(
      characteristic_name = btex_total_name,
      result_measure_value = dplyr::if_else(.data$btex_nd, NA_real_, .data$btex_value),
      result_detection_condition_text = dplyr::if_else(.data$btex_nd, "Not Detected", NA_character_),
      result_measure_measure_unit_code = "ug/L"
    ) |>
    dplyr::select(-"btex_value", -"btex_nd")

  # Result-level fields from the representative row no longer describe the new row
  blank_cols <- intersect(
    c("result_identifier", "measure_qualifier_code", "result_comment_text"),
    names(out)
  )
  for (cl in blank_cols) out[[cl]] <- NA

  out |>
    dplyr::relocate("btex_basis", "is_trip_blank", .after = dplyr::last_col())
}
