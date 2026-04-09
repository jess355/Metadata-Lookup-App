# tests/testthat/test-app.R
# Shiny app integration tests using shinytest2.
# Run with: Rscript tests/testthat.R  (from project root)

library(testthat)
library(shinytest2)

app_dir <- if (file.exists("app.R")) "." else file.path("..", "..")

test_that("app launches and loads all three tabs", {
  app <- AppDriver$new(app_dir, name = "launch")
  on.exit(app$stop())

  expect_equal(app$get_value(output = "pt_summary"),  "Showing 25 of 25 patients")

  app$set_inputs(main_tabs = "Office Visits")
  expect_equal(app$get_value(output = "ov_summary"),  "Showing 50 of 50 visits")

  app$set_inputs(main_tabs = "Samples")
  expect_equal(app$get_value(output = "sm_summary"),  "Showing 99 of 99 samples")
})

# ── Patients tab ──────────────────────────────────────────────────────────────

test_that("Patients tab: initial summary shows all 25 patients", {
  app <- AppDriver$new(app_dir, name = "pt-initial")
  on.exit(app$stop())

  summary <- app$get_value(output = "pt_summary")
  expect_equal(summary, "Showing 25 of 25 patients")
})

test_that("Patients tab: search by exact Patient_ID returns 1 result", {
  app <- AppDriver$new(app_dir, name = "pt-id-search")
  on.exit(app$stop())

  app$set_inputs(pt_id = "P1298")
  app$click("pt_search")
  summary <- app$get_value(output = "pt_summary")
  expect_equal(summary, "Showing 1 of 25 patients")
})

test_that("Patients tab: search by partial name filters correctly", {
  app <- AppDriver$new(app_dir, name = "pt-name-search")
  on.exit(app$stop())

  app$set_inputs(pt_name = "Anderson")
  app$click("pt_search")
  summary <- app$get_value(output = "pt_summary")
  expect_match(summary, "^Showing [0-9]+ of 25 patients$")
  n <- as.integer(regmatches(summary, regexpr("[0-9]+", summary)))
  expect_lt(n, 25)
})

test_that("Patients tab: filter by Sex = M returns correct count", {
  app <- AppDriver$new(app_dir, name = "pt-sex-filter")
  on.exit(app$stop())

  app$set_inputs(pt_sex = "M")
  app$click("pt_search")
  summary <- app$get_value(output = "pt_summary")
  expect_equal(summary, "Showing 12 of 25 patients")
})

test_that("Patients tab: filter by Ancestry returns subset", {
  app <- AppDriver$new(app_dir, name = "pt-ancestry-filter")
  on.exit(app$stop())

  app$set_inputs(pt_ancestry = "East Asian")
  app$click("pt_search")
  summary <- app$get_value(output = "pt_summary")
  expect_equal(summary, "Showing 9 of 25 patients")
})

test_that("Patients tab: reset restores all patients", {
  app <- AppDriver$new(app_dir, name = "pt-reset")
  on.exit(app$stop())

  app$set_inputs(pt_id = "P1298", pt_sex = "M")
  app$click("pt_search")
  app$click("pt_reset"); app$wait_for_idle()
  summary <- app$get_value(output = "pt_summary")
  expect_equal(summary, "Showing 25 of 25 patients")
})

# ── Office Visits tab ─────────────────────────────────────────────────────────

test_that("Office Visits tab: initial summary shows all 50 visits", {
  app <- AppDriver$new(app_dir, name = "ov-initial")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Office Visits")
  summary <- app$get_value(output = "ov_summary")
  expect_equal(summary, "Showing 50 of 50 visits")
})

test_that("Office Visits tab: filter by Patient_ID returns only their visits", {
  app <- AppDriver$new(app_dir, name = "ov-ptid-filter")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Office Visits")
  app$set_inputs(ov_pt_id = "P9794")
  app$click("ov_search")
  summary <- app$get_value(output = "ov_summary")
  expect_equal(summary, "Showing 3 of 50 visits")
})

test_that("Office Visits tab: narrow date range returns fewer visits", {
  app <- AppDriver$new(app_dir, name = "ov-date-filter")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Office Visits")
  app$set_inputs(ov_dates = c("2025-01-01", "2025-06-30"))
  app$click("ov_search")
  summary <- app$get_value(output = "ov_summary")
  expect_match(summary, "^Showing [0-9]+ of 50 visits$")
  n <- as.integer(regmatches(summary, regexpr("[0-9]+", summary)))
  expect_lt(n, 50)
})

test_that("Office Visits tab: reset restores all visits", {
  app <- AppDriver$new(app_dir, name = "ov-reset")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Office Visits")
  app$set_inputs(ov_pt_id = "P9794")
  app$click("ov_search")
  app$click("ov_reset"); app$wait_for_idle()
  summary <- app$get_value(output = "ov_summary")
  expect_equal(summary, "Showing 50 of 50 visits")
})

# ── Samples tab ───────────────────────────────────────────────────────────────

test_that("Samples tab: initial summary shows all 99 samples", {
  app <- AppDriver$new(app_dir, name = "sm-initial")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Samples")
  summary <- app$get_value(output = "sm_summary")
  expect_equal(summary, "Showing 99 of 99 samples")
})

test_that("Samples tab: search by Sample_ID returns 1 result", {
  app <- AppDriver$new(app_dir, name = "sm-id-search")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Samples")
  app$set_inputs(sm_id = "S_10140")
  app$click("sm_search")
  summary <- app$get_value(output = "sm_summary")
  expect_equal(summary, "Showing 1 of 99 samples")
})

test_that("Samples tab: filter by Tissue_Type = Blood returns correct count", {
  app <- AppDriver$new(app_dir, name = "sm-tissue-filter")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Samples")
  app$set_inputs(sm_tissue = "Blood")
  app$click("sm_search")
  summary <- app$get_value(output = "sm_summary")
  expect_equal(summary, "Showing 67 of 99 samples")
})

test_that("Samples tab: filter by Assay_Type = WGS returns correct count", {
  app <- AppDriver$new(app_dir, name = "sm-assay-filter")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Samples")
  app$set_inputs(sm_assay = "WGS")
  app$click("sm_search")
  summary <- app$get_value(output = "sm_summary")
  expect_equal(summary, "Showing 29 of 99 samples")
})

test_that("Samples tab: search by Patient_ID returns only their samples", {
  app <- AppDriver$new(app_dir, name = "sm-ptid-search")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Samples")
  app$set_inputs(sm_pt_id = "P2362")
  app$click("sm_search")
  summary <- app$get_value(output = "sm_summary")
  expect_match(summary, "^Showing [0-9]+ of 99 samples$")
  n <- as.integer(regmatches(summary, regexpr("[0-9]+", summary)))
  expect_lt(n, 99)
})

test_that("Samples tab: reset restores all 99 samples", {
  app <- AppDriver$new(app_dir, name = "sm-reset")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Samples")
  app$set_inputs(sm_id = "S_10140", sm_assay = "WGS")
  app$click("sm_search")
  app$click("sm_reset"); app$wait_for_idle()
  summary <- app$get_value(output = "sm_summary")
  expect_equal(summary, "Showing 99 of 99 samples")
})

# ── Edge cases ────────────────────────────────────────────────────────────────

test_that("Patients tab: search with no matches shows 0 results", {
  app <- AppDriver$new(app_dir, name = "pt-no-match")
  on.exit(app$stop())

  app$set_inputs(pt_id = "ZZZZZZ")
  app$click("pt_search")
  summary <- app$get_value(output = "pt_summary")
  expect_equal(summary, "Showing 0 of 25 patients")
})

test_that("Patients tab: name search is case-insensitive", {
  app <- AppDriver$new(app_dir, name = "pt-case-insensitive")
  on.exit(app$stop())

  app$set_inputs(pt_name = "anderson")
  app$click("pt_search")
  lower_summary <- app$get_value(output = "pt_summary")

  app$click("pt_reset"); app$wait_for_idle()
  app$set_inputs(pt_name = "Anderson")
  app$click("pt_search")
  upper_summary <- app$get_value(output = "pt_summary")

  expect_equal(lower_summary, upper_summary)
  expect_match(lower_summary, "^Showing [1-9][0-9]* of 25 patients$")
})

test_that("Patients tab: combined Sex + Ancestry filter returns intersection", {
  app <- AppDriver$new(app_dir, name = "pt-combined-filter")
  on.exit(app$stop())

  app$set_inputs(pt_sex = "M", pt_ancestry = "East Asian")
  app$click("pt_search")
  combined <- app$get_value(output = "pt_summary")
  n_combined <- as.integer(regmatches(combined, regexpr("[0-9]+", combined)))

  app$click("pt_reset"); app$wait_for_idle()
  app$set_inputs(pt_sex = "M")
  app$click("pt_search")
  male_only <- app$get_value(output = "pt_summary")
  n_male <- as.integer(regmatches(male_only, regexpr("[0-9]+", male_only)))

  expect_lte(n_combined, n_male)
})

test_that("Office Visits tab: search with no matches shows 0 results", {
  app <- AppDriver$new(app_dir, name = "ov-no-match")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Office Visits")
  app$set_inputs(ov_pt_id = "ZZZZZZ")
  app$click("ov_search")
  summary <- app$get_value(output = "ov_summary")
  expect_equal(summary, "Showing 0 of 50 visits")
})

test_that("Samples tab: combined Patient_ID + Assay_Type filter returns intersection", {
  app <- AppDriver$new(app_dir, name = "sm-combined-filter")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Samples")
  app$set_inputs(sm_pt_id = "P2362", sm_assay = "WGS")
  app$click("sm_search")
  combined <- app$get_value(output = "sm_summary")
  n_combined <- as.integer(regmatches(combined, regexpr("[0-9]+", combined)))

  app$click("sm_reset"); app$wait_for_idle()
  app$set_inputs(main_tabs = "Samples")
  app$set_inputs(sm_pt_id = "P2362")
  app$click("sm_search")
  pt_only <- app$get_value(output = "sm_summary")
  n_pt <- as.integer(regmatches(pt_only, regexpr("[0-9]+", pt_only)))

  expect_lte(n_combined, n_pt)
})

test_that("Samples tab: search with no matches shows 0 results", {
  app <- AppDriver$new(app_dir, name = "sm-no-match")
  on.exit(app$stop())

  app$set_inputs(main_tabs = "Samples")
  app$set_inputs(sm_id = "ZZZZZZ")
  app$click("sm_search")
  summary <- app$get_value(output = "sm_summary")
  expect_equal(summary, "Showing 0 of 99 samples")
})
