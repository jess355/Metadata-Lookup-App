# tests/test_normalization.R
# Run with: Rscript -e "testthat::test_file('tests/test_normalization.R')"
# from the project root directory.

library(testthat)
library(DBI)
library(RSQLite)
library(dplyr)

# Support running from project root, tests/, or tests/testthat/
db_path <- "data/genomic_metadata.sqlite"
if (!file.exists(db_path)) db_path <- file.path("..", "data/genomic_metadata.sqlite")
if (!file.exists(db_path)) db_path <- file.path("..", "..", "data/genomic_metadata.sqlite")

con <- dbConnect(SQLite(), db_path)
patients <- dbReadTable(con, "Patients")
visits   <- dbReadTable(con, "Office_Visits")
samples  <- dbReadTable(con, "Samples")
dbDisconnect(con)

# ── Patients ───────────────────────────────────────────────────────────────────
test_that("Patients table has correct row count", {
  expect_equal(nrow(patients), 25)
})

test_that("Patient_ID is the unique primary key", {
  expect_false(anyDuplicated(patients$Patient_ID) > 0)
})

test_that("No patient has a missing Patient_ID", {
  expect_true(all(!is.na(patients$Patient_ID)))
})

test_that("Patient DOB values are plausible dates (strings in YYYY-MM-DD format)", {
  expect_true(all(grepl("^\\d{4}-\\d{2}-\\d{2}", na.omit(patients$DOB))))
})

# ── Office Visits ──────────────────────────────────────────────────────────────
test_that("Office_Visits table has correct row count", {
  expect_equal(nrow(visits), 50)
})

test_that("Visit_ID is the unique primary key", {
  expect_false(anyDuplicated(visits$Visit_ID) > 0)
})

test_that("Each Patient_ID in Office_Visits exists in Patients", {
  expect_true(all(visits$Patient_ID %in% patients$Patient_ID))
})

test_that("Patient + Encounter_Date combinations are unique per visit", {
  combos <- paste(visits$Patient_ID, visits$Encounter_Date)
  expect_false(anyDuplicated(combos) > 0)
})

test_that("Weight discrepancies were resolved (no duplicate patient+date rows)", {
  n_unique_combos <- nrow(distinct(visits, Patient_ID, Encounter_Date))
  expect_equal(n_unique_combos, nrow(visits))
})

# ── Samples ────────────────────────────────────────────────────────────────────
test_that("Samples table has correct row count", {
  expect_equal(nrow(samples), 99)
})

test_that("Sample_ID is the unique primary key", {
  expect_false(anyDuplicated(samples$Sample_ID) > 0)
})

test_that("Each Visit_ID in Samples exists in Office_Visits", {
  expect_true(all(samples$Visit_ID %in% visits$Visit_ID))
})

test_that("No sample has a missing Visit_ID foreign key", {
  expect_true(all(!is.na(samples$Visit_ID)))
})

test_that("Coverage_Mean is numeric (not character strings like '24x')", {
  expect_true(is.numeric(samples$Coverage_Mean))
})

test_that("Coverage_Mean values are positive where present", {
  vals <- na.omit(samples$Coverage_Mean)
  expect_true(all(vals > 0))
})

test_that("No 'N/A' string remains in Coverage_Mean", {
  # After cleaning, Coverage_Mean should be numeric NA — not the string "N/A"
  expect_false(any(samples$Coverage_Mean == "N/A", na.rm = TRUE))
})

# ── Patients (additional) ──────────────────────────────────────────────────────
test_that("Sex values are within allowed set", {
  valid_sex <- c("M", "F", "U", "Other")
  expect_true(all(patients$Sex %in% valid_sex))
})

test_that("Full_Name is not missing for any patient", {
  expect_true(all(!is.na(patients$Full_Name) & nzchar(patients$Full_Name)))
})

test_that("Sex is not missing for any patient", {
  expect_true(all(!is.na(patients$Sex)))
})

test_that("DOB is before each patient's earliest encounter date", {
  earliest_visits <- visits %>%
    group_by(Patient_ID) %>%
    summarise(earliest = min(as.Date(Encounter_Date)), .groups = "drop")
  check <- patients %>%
    inner_join(earliest_visits, by = "Patient_ID") %>%
    mutate(dob_ok = as.Date(DOB) < earliest)
  expect_true(all(check$dob_ok))
})

test_that("Every patient has at least one visit", {
  expect_true(all(patients$Patient_ID %in% visits$Patient_ID))
})

# ── Office Visits (additional) ─────────────────────────────────────────────────
test_that("Encounter_Date is not missing for any visit", {
  expect_true(all(!is.na(visits$Encounter_Date)))
})

test_that("Weight_kg is positive where present", {
  vals <- na.omit(visits$Weight_kg)
  expect_true(all(vals > 0))
})

# ── Samples (additional) ───────────────────────────────────────────────────────
test_that("No 'N/A' string remains in Genome_Build", {
  expect_false(any(samples$Genome_Build == "N/A", na.rm = TRUE))
})

test_that("Genome_Build values are within allowed set", {
  valid_builds <- c("hg19", "hg38", "GRCh37", "GRCh38")
  actual <- samples$Genome_Build[!is.na(samples$Genome_Build) & samples$Genome_Build != "N/A"]
  expect_true(all(actual %in% valid_builds))
})

test_that("Assay_Type values are within allowed set", {
  valid_assays <- c("WGS", "WES", "RNA-seq", "scRNA-seq", "SomaScan", NA)
  expect_true(all(is.na(samples$Assay_Type) | samples$Assay_Type %in% valid_assays))
})

test_that("Coverage_Mean is within a plausible range (0–1000x)", {
  vals <- na.omit(samples$Coverage_Mean)
  expect_true(all(vals > 0 & vals <= 1000))
})

test_that("Sample enrichment join leaves no unmatched Patient_IDs", {
  enriched <- samples %>%
    left_join(visits %>% select(Visit_ID, Patient_ID), by = "Visit_ID")
  expect_true(all(!is.na(enriched$Patient_ID)))
})

test_that("Timepoint_Relative values are within allowed set", {
  valid_timepoints <- c("Day_0", "Day_90", "Day_180", NA)
  expect_true(all(is.na(samples$Timepoint_Relative) | samples$Timepoint_Relative %in% valid_timepoints))
})

test_that("File_Path is present for every sample", {
  expect_true(all(!is.na(samples$File_Path) & nzchar(samples$File_Path)))
})

test_that("File_Path extension matches Assay_Type", {
  ext_map <- list(
    WGS      = c(".bam", ".vcf.gz"),
    WES      = c(".bam", ".vcf.gz"),
    `RNA-seq`  = c(".bam"),
    `scRNA-seq` = c(".bam"),
    SomaScan = c(".csv")
  )
  for (assay in names(ext_map)) {
    rows <- samples[!is.na(samples$Assay_Type) & samples$Assay_Type == assay, ]
    if (nrow(rows) == 0) next
    valid_exts <- ext_map[[assay]]
    matches <- sapply(rows$File_Path, function(p) any(endsWith(p, valid_exts)))
    expect_true(all(matches),
      info = sprintf("File_Path extension mismatch for Assay_Type '%s'", assay))
  }
})

test_that("Coverage_Mean is present for DNA sequencing assays (WGS, WES)", {
  dna_seq_assays <- c("WGS", "WES")
  dna_samples <- samples[!is.na(samples$Assay_Type) & samples$Assay_Type %in% dna_seq_assays, ]
  expect_true(all(!is.na(dna_samples$Coverage_Mean)))
})

test_that("Coverage_Mean is NA for non-coverage assays (SomaScan, RNA-seq, scRNA-seq)", {
  no_cov_assays <- c("SomaScan", "RNA-seq", "scRNA-seq")
  no_cov_samples <- samples[!is.na(samples$Assay_Type) & samples$Assay_Type %in% no_cov_assays, ]
  expect_true(all(is.na(no_cov_samples$Coverage_Mean)))
})

test_that("Every patient has at least one sample", {
  patient_ids_with_samples <- unique(
    (samples %>% left_join(visits %>% select(Visit_ID, Patient_ID), by = "Visit_ID"))$Patient_ID
  )
  expect_true(all(patients$Patient_ID %in% patient_ids_with_samples))
})

# ── ID format checks ───────────────────────────────────────────────────────────
test_that("Patient_ID values match expected format (P followed by digits)", {
  expect_true(all(grepl("^P\\d+$", patients$Patient_ID)))
})

test_that("Visit_ID values match expected format (V followed by digits)", {
  expect_true(all(grepl("^V\\d+$", visits$Visit_ID)))
})

test_that("Sample_ID values match expected format (S_ followed by digits)", {
  expect_true(all(grepl("^S_\\d+$", samples$Sample_ID)))
})

# ── Ancestry ───────────────────────────────────────────────────────────────────
test_that("Ancestry values are within allowed set", {
  valid_ancestry <- c("European", "East Asian", "South Asian", "African",
                      "Admixed American", NA)
  expect_true(all(is.na(patients$Ancestry) | patients$Ancestry %in% valid_ancestry))
})

# ── Date sanity ────────────────────────────────────────────────────────────────
test_that("Encounter dates are not unreasonably far in the future (within 2 years)", {
  future_limit <- Sys.Date() + 365 * 2
  expect_true(all(as.Date(visits$Encounter_Date) <= future_limit))
})

test_that("No patient was born after year 2020 (minimum age for genomic study)", {
  expect_true(all(as.Date(patients$DOB) <= as.Date("2020-01-01")))
})

# ── Edge cases ─────────────────────────────────────────────────────────────────
test_that("No empty strings in required text columns", {
  expect_false(any(patients$Patient_ID == "" | patients$Full_Name == "" | patients$Sex == ""))
  expect_false(any(visits$Visit_ID == "" | visits$Patient_ID == "" | visits$Encounter_Date == ""))
  expect_false(any(samples$Sample_ID == "" | samples$Visit_ID == "" | samples$File_Path == ""))
})

test_that("File_Path is unique across all samples", {
  expect_false(anyDuplicated(samples$File_Path) > 0)
})

test_that("DNA sequencing assays always have a Genome_Build", {
  dna_samples <- samples[!is.na(samples$Assay_Type) & samples$Assay_Type %in% c("WGS", "WES"), ]
  expect_true(all(!is.na(dna_samples$Genome_Build)))
})

test_that("Weight_kg is within a plausible human range (20–300 kg)", {
  vals <- na.omit(visits$Weight_kg)
  expect_true(all(vals >= 20 & vals <= 300))
})

test_that("DOB values are valid calendar dates (not e.g. Feb 30)", {
  parsed <- as.Date(patients$DOB, format = "%Y-%m-%d")
  expect_true(all(!is.na(parsed)))
})

test_that("Encounter_Date values are valid calendar dates", {
  parsed <- as.Date(visits$Encounter_Date, format = "%Y-%m-%d")
  expect_true(all(!is.na(parsed)))
})

test_that("DOB is not implausibly old (after 1900-01-01)", {
  expect_true(all(as.Date(patients$DOB) >= as.Date("1900-01-01")))
})

test_that("Every visit has at least one sample", {
  expect_true(all(visits$Visit_ID %in% samples$Visit_ID))
})
