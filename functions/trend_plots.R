# ==============================================================================
# TREND ANALYSIS HELPERS FOR PARAMETER CHAPTERS (opt-in; see
# templates/_parameter_chunk.Rmd and templates/_trend_chunk.Rmd)
#
# Descriptive trends over time (value vs. date, per site) and space (value vs.
# river mile, mainstem only), in the spirit of Guerron-Orejuela (2016).
#
# Design notes
#   - Mainstem and tributary sites are separate geographies and are never
#     combined in one plot. Each figure is faceted by season (spring/summer).
#   - Non-detects are drawn as hollow points at their detection limit and are
#     EXCLUDED from all smooths and trend statistics (as in the 2016 report).
#     Detection limits come from the cached WQP detection_limits.csv
#     (see functions/refresh_wqp_data.R).
#   - Smooths are fit on log10(concentration) and back-transformed, so fitted
#     curves cannot go negative (the 2016 polynomial trendlines sometimes did)
#     and skewed data do not let a few large values dominate.
#   - Smooths are DESCRIPTIVE. Formal trend tests are provided separately by
#     trend_stats() (Mann-Kendall with Benjamini-Hochberg adjusted p-values).
# ==============================================================================

library(tidyverse)

# Minimum number of detected values needed to draw a smooth or test a trend
TREND_MIN_DETECTS <- 8

TRIBUTARY_ORDER <- c("No Name Creek", "Beaver Creek", "Slikok Creek",
                     "Soldotna Creek", "Funny River", "Moose River",
                     "Killey River", "Russian River", "Juneau Creek")


# ---- Data -------------------------------------------------------------------

# Returns one row per result with: date, season, site_type ("Mainstem" or
# "Tributary"), site_label, river_mile (numeric, mainstem only), value (the
# result, or the detection limit for non-detects), nd (logical), unit.
load_trend_data <- function(characteristic,
                            sample_fraction = character(0),
                            data_path = "other/output/analysis_format/baseline_analysis_format.csv",
                            dl_path   = "other/input/WQX_downloads/wqp_live/detection_limits.csv") {

  if (!file.exists(dl_path)) {
    stop("Detection limit cache not found at '", dl_path, "'. ",
         "Run functions/refresh_wqp_data.R to create it.")
  }

  # Read as character, then convert: WQP currently holds at least one
  # non-numeric limit (result 56514557, "O.18" with a letter O). Such values
  # become NA here, so that non-detect is dropped from the plots rather than
  # guessed at; the underlying WQP record should be corrected.
  dl <- read_csv(dl_path, col_types = cols(.default = col_character())) |>
    transmute(result_key = ResultIdentifier,
              dl_value   = suppressWarnings(as.numeric(DetectionQuantitationLimitMeasureValue)),
              dl_unit    = DetectionQuantitationLimitMeasureUnitCode)

  dat <- read_csv(data_path, col_types = cols(.default = col_character())) |>
    filter(characteristic_name == characteristic,
           result_status_identifier != "Rejected",
           !is.na(season))

  # Same sample fraction rule as the boxplots: keep listed fractions plus rows
  # where the fraction was not recorded
  if (length(sample_fraction) > 0) {
    dat <- dat |>
      filter(result_sample_fraction_text %in% sample_fraction |
               is.na(result_sample_fraction_text))
  }

  dat <- dat |>
    mutate(result_key = sub("^STORET-", "", result_identifier),
           date       = as.Date(activity_start_date),
           measured   = suppressWarnings(as.numeric(result_measure_value)),
           nd         = result_detection_condition_text %in% "Not Detected" &
                          is.na(measured)) |>
    left_join(dl, by = "result_key")

  unit <- names(which.max(table(dat$result_measure_measure_unit_code[!is.na(dat$measured)])))

  dat |>
    mutate(value = if_else(nd, dl_value, measured),
           unit  = unit) |>
    # drop non-detects lacking a usable limit in the plotted unit, and any
    # other results without a numeric value
    filter(!is.na(value), value > 0, !nd | dl_unit %in% unit) |>
    mutate(
      site_type  = if_else(trib_mainstem == "m", "Mainstem", "Tributary"),
      river_mile = if_else(site_type == "Mainstem", suppressWarnings(as.numeric(river_mile)), NA_real_),
      site_label = if_else(site_type == "Mainstem", paste("RM", river_mile), tributary_name),
      season     = factor(season, levels = c("Spring", "Summer"))
    ) |>
    select(date, season, site_type, site_label, river_mile, value, nd, unit)
}

# Adds two-line facet labels, e.g. "Spring" / "115 detected, 7 non-detect".
# With include_nd = FALSE (spatial plot, which excludes non-detects) the second
# line reports detected values and the number of sites instead.
add_facet_labels <- function(df, include_nd = TRUE) {
  labs_tbl <- df |>
    group_by(season) |>
    summarise(n_det = sum(!nd), n_nd = sum(nd), n_sites = n_distinct(site_label[!nd]),
              .groups = "drop") |>
    mutate(facet = if (include_nd) {
      sprintf("%s
%d detected, %d non-detect", season, n_det, n_nd)
    } else {
      sprintf("%s
%d detected values, %d sites", season, n_det, n_sites)
    })
  df |>
    left_join(labs_tbl |> select(season, facet), by = "season") |>
    mutate(facet = factor(facet, levels = labs_tbl$facet))
}


# ---- Plots ------------------------------------------------------------------

# Value vs. date, one smoothed line per site, faceted by season.
# site_type: "Mainstem" or "Tributary".
trend_plot_time <- function(df, site_type) {
  d <- df |> filter(site_type == !!site_type)
  if (nrow(d) == 0) return(NULL)

  site_levels <- if (site_type == "Mainstem") {
    d |> distinct(site_label, river_mile) |> arrange(river_mile) |> pull(site_label)
  } else {
    intersect(TRIBUTARY_ORDER, unique(d$site_label))
  }
  d <- d |> mutate(site_label = factor(site_label, levels = site_levels)) |> add_facet_labels()

  # Descriptive LOESS per site x season, on log10 scale. span = 1 and degree = 1
  # keep the curve gentle given ~10-15 samples per site.
  smooths <- d |>
    filter(!nd) |>
    group_by(facet, season, site_label) |>
    filter(n() >= TREND_MIN_DETECTS) |>
    group_modify(function(x, key) {
      x$t <- as.numeric(x$date)
      fit <- tryCatch(loess(log10(value) ~ t, data = x, span = 1, degree = 1),
                      error = function(e) NULL)
      if (is.null(fit)) return(tibble(date = as.Date(character()), fitted = numeric()))
      grid <- seq(min(x$t), max(x$t), length.out = 60)
      tibble(date = as.Date(grid), fitted = 10^predict(fit, data.frame(t = grid)))
    }) |>
    ungroup()

  ggplot(d, aes(date, value, color = site_label)) +
    geom_point(data = ~ filter(.x, !nd), shape = 16, alpha = 0.7, size = 1.6) +
    geom_point(data = ~ filter(.x, nd),  shape = 1,  size = 1.8) +
    geom_line(data = smooths, aes(y = fitted), linewidth = 0.7) +
    scale_y_log10() +
    scale_color_viridis_d() +
    facet_wrap(~ facet) +
    labs(title = paste(site_type, "Sites"), x = NULL,
         y = paste0(unique(d$unit)[1], " (log scale)"), color = "Site",
         caption = str_wrap("Filled points: detected. Hollow points: non-detects plotted at the detection limit (not used in lines). Lines: descriptive LOESS, sites with at least 8 detected values.", 90)) +
    theme(plot.caption = element_text(hjust = 0))
}

# Mainstem only: median (or mean) by river mile with a degree-2 polynomial fit
# on log10 scale, faceted by season.
trend_plot_space <- function(df, statistic = c("median", "mean")) {
  statistic <- match.arg(statistic)
  d <- df |> filter(site_type == "Mainstem", !nd) |> add_facet_labels(include_nd = FALSE)
  if (nrow(d) == 0) return(NULL)
  fun <- match.fun(statistic)

  by_mile <- d |> group_by(facet, river_mile) |>
    summarise(stat = fun(value), n = n(), .groups = "drop")

  fits <- by_mile |>
    group_by(facet) |>
    filter(n() >= 4) |>
    group_modify(function(x, key) {
      fit <- lm(log10(stat) ~ poly(river_mile, 2), data = x)
      grid <- tibble(river_mile = seq(min(x$river_mile), max(x$river_mile), length.out = 100))
      grid$fitted <- 10^predict(fit, grid)
      grid
    }) |>
    ungroup()

  ggplot(by_mile, aes(river_mile, stat)) +
    geom_point(aes(size = n), alpha = 0.8) +
    geom_line(data = fits, aes(y = fitted)) +
    scale_y_log10() +
    scale_size_continuous(name = "Samples at site", range = c(1.5, 4)) +
    facet_wrap(~ facet) +
    labs(title = "Mainstem Sites, by River Mile",
         x = "River mile", y = paste0(statistic, " ", unique(d$unit)[1], " (log scale)"),
         caption = str_wrap(paste0("Points: ", statistic, " of detected values at each site. Line: descriptive degree-2 polynomial fit on log10 concentration. Non-detects excluded."), 90)) +
    theme(plot.caption = element_text(hjust = 0))
}


# ---- Statistics -------------------------------------------------------------

# Mann-Kendall trend test (Kendall's tau vs. time) and Theil-Sen slope for each
# site x season, detected values only. p-values are Benjamini-Hochberg adjusted
# across all tested site x season combinations for the parameter.
trend_stats <- function(df, min_detects = TREND_MIN_DETECTS) {
  df |>
    group_by(site_type, site_label, season) |>
    group_modify(function(x, key) {
      det <- x |> filter(!nd)
      out <- tibble(n_detected = nrow(det), n_nondetect = sum(x$nd),
                    years = if (nrow(det) > 0) paste(range(year(det$date)), collapse = "-") else NA_character_,
                    tau = NA_real_, p_value = NA_real_, sen_slope_per_year = NA_real_)
      if (nrow(det) >= min_detects) {
        t_yr <- as.numeric(det$date) / 365.25
        kt <- suppressWarnings(cor.test(t_yr, det$value, method = "kendall", exact = FALSE))
        i <- combn(nrow(det), 2)
        dt <- t_yr[i[2, ]] - t_yr[i[1, ]]
        slopes <- (det$value[i[2, ]] - det$value[i[1, ]])[dt != 0] / dt[dt != 0]
        out$tau <- unname(kt$estimate); out$p_value <- kt$p.value
        out$sen_slope_per_year <- median(slopes)
      }
      out
    }) |>
    ungroup() |>
    mutate(p_adjusted = p.adjust(p_value, method = "BH"),
           direction = case_when(is.na(p_adjusted) ~ NA_character_,
                                 p_adjusted < 0.05 & tau > 0 ~ "increasing",
                                 p_adjusted < 0.05 & tau < 0 ~ "decreasing",
                                 TRUE ~ "no significant trend")) |>
    arrange(site_type, season, desc(!is.na(p_value)), p_value)
}
