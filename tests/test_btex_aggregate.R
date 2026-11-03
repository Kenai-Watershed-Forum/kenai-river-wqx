# Run with: testthat::test_file("tests/test_btex_aggregate.R")
library(testthat)
library(dplyr)

# testthat runs from the test file's directory; find the repo root either way
root <- if (file.exists("functions/btex_aggregate.R")) "." else ".."
source(file.path(root, "functions/btex_aggregate.R"))

mk <- function(site = "KBL_m_01.5", date = "2010-07-01", id = "a", name, value = NA,
               nd = FALSE) {
  tibble(
    monitoring_location_name = site,
    activity_start_date = as.Date(date),
    activity_identifier = id,
    characteristic_name = name,
    result_sample_fraction_text = "Volatile",
    result_measure_value = value,
    result_detection_condition_text = if (nd) "Not Detected" else NA_character_,
    result_measure_measure_unit_code = if (nd) NA_character_ else "ug/L",
    result_identifier = "r1",
    measure_qualifier_code = NA_character_,
    result_comment_text = NA_character_
  )
}
tot <- btex_total_name

test_that("reported total is used even when components exist", {
  d <- bind_rows(
    mk(name = tot, value = 3),
    mk(name = "Benzene", value = 1),
    mk(name = "Toluene", value = 1)
  )
  out <- btex_aggregate(d)
  expect_equal(nrow(out), 1)
  expect_equal(out$result_measure_value, 3)
  expect_equal(out$btex_basis, "reported total")
  expect_equal(out$characteristic_name, tot)
  expect_equal(out$result_measure_measure_unit_code, "ug/L")
})

test_that("components are summed with ND counted as 0", {
  d <- bind_rows(
    mk(name = "Benzene", value = 1.5),
    mk(name = "Toluene", nd = TRUE),
    mk(name = "Ethylbenzene", value = 0.5)
  )
  out <- btex_aggregate(d)
  expect_equal(out$result_measure_value, 2)
  expect_equal(out$btex_basis, "sum of components")
  expect_true(is.na(out$result_detection_condition_text))
})

test_that("total xylene is preferred over isomers (no double counting)", {
  d <- bind_rows(
    mk(name = "Benzene", value = 1),
    mk(name = "Xylene", value = 2),
    mk(name = "o-Xylene", value = 1),
    mk(name = "m,p-Xylene", value = 1)
  )
  expect_equal(btex_aggregate(d)$result_measure_value, 3)
})

test_that("isomers are summed when no total xylene is reported", {
  d <- bind_rows(
    mk(name = "o-Xylene", value = 1),
    mk(name = "m,p-Xylene", value = 2)
  )
  expect_equal(btex_aggregate(d)$result_measure_value, 3)
})

test_that("an event with only ND results is ND with NA value", {
  d <- bind_rows(
    mk(name = "Benzene", nd = TRUE),
    mk(name = "o-Xylene", nd = TRUE),
    mk(name = "m,p-Xylene", nd = TRUE)
  )
  out <- btex_aggregate(d)
  expect_equal(nrow(out), 1)
  expect_true(is.na(out$result_measure_value))
  expect_equal(out$result_detection_condition_text, "Not Detected")
})

test_that("a detected isomer makes the event detected", {
  d <- bind_rows(
    mk(name = "o-Xylene", value = 0.7),
    mk(name = "m,p-Xylene", nd = TRUE)
  )
  out <- btex_aggregate(d)
  expect_equal(out$result_measure_value, 0.7)
  expect_true(is.na(out$result_detection_condition_text))
})

test_that("duplicates are averaged (ND as 0) and events keyed by site and date", {
  d <- bind_rows(
    mk(id = "t1", name = tot, value = 2),
    mk(id = "t2", name = tot, value = 4),
    mk(id = "t3", date = "2010-07-02", name = tot, value = 5)
  )
  out <- btex_aggregate(d) |> arrange(activity_start_date)
  expect_equal(out$result_measure_value, c(3, 5))

  d2 <- bind_rows(mk(id = "t1", name = tot, value = 2), mk(id = "t2", name = tot, nd = TRUE))
  expect_equal(btex_aggregate(d2)$result_measure_value, 1)
})

test_that("analyte-embedded activity IDs (2021 style) group into one event", {
  d <- bind_rows(
    mk(id = "KBL-20210727-Benzene", name = "Benzene", nd = TRUE),
    mk(id = "KBL-20210727-Toluene", name = "Toluene", nd = TRUE),
    mk(id = "KBL-20210727-Xylene", name = "Xylene", nd = TRUE)
  )
  expect_equal(nrow(btex_aggregate(d)), 1)
})

test_that("trip blanks are separated from field samples", {
  d <- bind_rows(
    mk(id = "KBL-20210727-Benzene", name = "Benzene", nd = TRUE),
    mk(id = "KBL-20210727-Benzene-Blank", name = "Benzene", value = 0.3)
  )
  field <- btex_aggregate(d)
  blank <- btex_aggregate(d, trip_blanks = TRUE)
  expect_equal(nrow(field), 1)
  expect_false(field$is_trip_blank)
  expect_true(is.na(field$result_measure_value))
  expect_equal(nrow(blank), 1)
  expect_true(blank$is_trip_blank)
  expect_equal(blank$result_measure_value, 0.3)
})

test_that("rows with no value and no detection condition are ignored", {
  d <- bind_rows(mk(name = "Benzene", value = NA), mk(name = "Toluene", value = 1))
  expect_equal(btex_aggregate(d)$result_measure_value, 1)
  expect_equal(nrow(btex_aggregate(mk(name = "Benzene", value = NA))), 0)
})

test_that("non-volatile and non-BTEX rows are ignored; missing columns error", {
  d <- bind_rows(mk(name = "Benzene", value = 1), mk(name = "Zinc", value = 9))
  d$result_sample_fraction_text[2] <- "Dissolved"
  expect_equal(nrow(btex_aggregate(d)), 1)
  expect_error(btex_aggregate(select(d, -activity_identifier)), "missing columns")
})

test_that("real 2000-2021 data: counts, 2012 and 2021 present, blanks excluded", {
  f <- file.path(root, "other/output/analysis_format/baseline_export_format.csv")
  skip_if_not(file.exists(f))
  x <- readr::read_csv(f, show_col_types = FALSE)
  out <- btex_aggregate(x)
  yrs <- table(as.integer(format(out$activity_start_date, "%Y")))
  expect_true(all(c("2012", "2021") %in% names(yrs)))
  expect_false(any(out$is_trip_blank))
  expect_equal(anyDuplicated(out[c("monitoring_location_name", "activity_start_date")]), 0)
  expect_false(any(!is.na(out$result_measure_value) & out$result_measure_value < 0))
  # reported total used wherever one exists
  tot_events <- x |>
    filter(characteristic_name == tot, !is.na(result_measure_value) |
             result_detection_condition_text %in% "Not Detected") |>
    distinct(monitoring_location_name, activity_start_date)
  chk <- inner_join(tot_events, out, by = c("monitoring_location_name", "activity_start_date"))
  expect_true(all(chk$btex_basis == "reported total"))
  expect_equal(nrow(btex_aggregate(x, trip_blanks = TRUE)), 4)
})
