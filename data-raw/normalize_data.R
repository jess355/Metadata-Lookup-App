# data-raw/normalize_data.R
# Run from project root: Rscript data-raw/normalize_data.R
#
# Reads the master "in" sheet from data-raw/08_genomic_metadata.xlsx,
# produces normalized Office_Visits and Samples tables,
# writes them back into the Excel workbook, and creates data/genomic_metadata.sqlite.

library(readxl)
library(openxlsx)
library(dplyr)
library(DBI)
library(RSQLite)

# ── 1. Load data ──────────────────────────────────────────────────────────────
xlsx_path <- "data-raw/08_genomic_metadata.xlsx"
raw <- read_excel(xlsx_path, sheet = "in")

# ── 2. Clean raw data ─────────────────────────────────────────────────────────
# Parse Coverage_Mean: strip trailing "x", coerce "N/A" / other strings → NA
# Coerce Genome_Build: convert "N/A" strings → NA
raw <- raw %>%
  mutate(
    Coverage_Mean = as.numeric(
      ifelse(grepl("^[0-9.]+x$", Coverage_Mean),
             sub("x$", "", Coverage_Mean),
             NA_character_)
    ),
    Genome_Build = ifelse(Genome_Build == "N/A", NA_character_, Genome_Build)
  )

# ── 3. Build Patients table (already in Excel; rebuild for DB consistency) ────
# Some patients have NA Ancestry in a subset of rows — coalesce to non-NA value.
patients <- raw %>%
  select(Patient_ID, Full_Name, DOB, Sex, Ancestry) %>%
  group_by(Patient_ID) %>%
  summarise(
    Full_Name = first(na.omit(Full_Name)),
    DOB       = as.Date(first(na.omit(DOB))),
    Sex       = first(na.omit(Sex)),
    Ancestry  = first(na.omit(Ancestry)),
    .groups   = "drop"
  ) %>%
  arrange(Patient_ID)

stopifnot("Duplicate Patient_IDs in patients table" = !anyDuplicated(patients$Patient_ID))

# ── 4. Build Office_Visits table ──────────────────────────────────────────────
# One row per patient+date visit. Coalesce weight where same visit has one
# real value and one NA (data entry inconsistency in 3 rows).
office_visits <- raw %>%
  select(Patient_ID, Encounter_Date, Weight_kg) %>%
  group_by(Patient_ID, Encounter_Date) %>%
  summarise(Weight_kg = {
    vals <- na.omit(Weight_kg)
    if (length(vals) > 0) vals[[1]] else NA_real_
  }, .groups = "drop") %>%
  arrange(Patient_ID, Encounter_Date) %>%
  mutate(
    Visit_ID = sprintf("V%03d", row_number()),
    Encounter_Date = as.Date(Encounter_Date)
  ) %>%
  select(Visit_ID, Patient_ID, Encounter_Date, Weight_kg)

stopifnot("Duplicate Visit_IDs" = !anyDuplicated(office_visits$Visit_ID))
stopifnot("Duplicate Patient+Date combos" = !anyDuplicated(
  paste(office_visits$Patient_ID, office_visits$Encounter_Date)
))

# ── 5. Build Samples table ────────────────────────────────────────────────────
# Join visit lookup to get Visit_ID foreign key
visit_lookup <- office_visits %>%
  select(Visit_ID, Patient_ID, Encounter_Date)

samples <- raw %>%
  mutate(Encounter_Date = as.Date(Encounter_Date)) %>%
  left_join(visit_lookup, by = c("Patient_ID", "Encounter_Date")) %>%
  select(Sample_ID, Visit_ID, Tissue_Type, Assay_Type,
         Timepoint_Relative, File_Path, Genome_Build, Coverage_Mean) %>%
  distinct()

stopifnot("Duplicate Sample_IDs" = !anyDuplicated(samples$Sample_ID))
stopifnot("Samples missing Visit_ID FK" = !any(is.na(samples$Visit_ID)))

# ── 6. Write normalized sheets back into the workbook ─────────────────────────
wb <- loadWorkbook(xlsx_path)

# Remove old (empty) sheets and rewrite
removeWorksheet(wb, "Office visits")
removeWorksheet(wb, "Samples")
addWorksheet(wb, "Office visits")
addWorksheet(wb, "Samples")
writeData(wb, "Office visits", office_visits)
writeData(wb, "Samples", samples)
saveWorkbook(wb, xlsx_path, overwrite = TRUE)

cat("Excel workbook updated.\n")
cat(sprintf("  Patients   : %d rows\n", nrow(patients)))
cat(sprintf("  Visits     : %d rows\n", nrow(office_visits)))
cat(sprintf("  Samples    : %d rows\n", nrow(samples)))

# ── 7. Create SQLite database ─────────────────────────────────────────────────
db_path <- "data/genomic_metadata.sqlite"
if (file.exists(db_path)) file.remove(db_path)

con <- dbConnect(SQLite(), db_path)

# Patients
dbExecute(con, "
  CREATE TABLE Patients (
    Patient_ID  TEXT PRIMARY KEY,
    Full_Name   TEXT NOT NULL,
    DOB         TEXT,
    Sex         TEXT,
    Ancestry    TEXT
  );
")

# Office_Visits
dbExecute(con, "
  CREATE TABLE Office_Visits (
    Visit_ID       TEXT PRIMARY KEY,
    Patient_ID     TEXT NOT NULL,
    Encounter_Date TEXT,
    Weight_kg      REAL,
    FOREIGN KEY (Patient_ID) REFERENCES Patients(Patient_ID)
  );
")

# Samples
dbExecute(con, "
  CREATE TABLE Samples (
    Sample_ID          TEXT PRIMARY KEY,
    Visit_ID           TEXT NOT NULL,
    Tissue_Type        TEXT,
    Assay_Type         TEXT,
    Timepoint_Relative TEXT,
    File_Path          TEXT,
    Genome_Build       TEXT,
    Coverage_Mean      REAL,
    FOREIGN KEY (Visit_ID) REFERENCES Office_Visits(Visit_ID)
  );
")

# Format Date columns as ISO strings for SQLite TEXT storage
patients_db <- patients %>%
  mutate(DOB = format(as.Date(DOB), "%Y-%m-%d"))

office_visits_db <- office_visits %>%
  mutate(Encounter_Date = format(as.Date(Encounter_Date), "%Y-%m-%d"))

dbWriteTable(con, "Patients",      patients_db,      append = TRUE, row.names = FALSE)
dbWriteTable(con, "Office_Visits", office_visits_db, append = TRUE, row.names = FALSE)
dbWriteTable(con, "Samples",       samples,       append = TRUE, row.names = FALSE)

dbDisconnect(con)
cat(sprintf("SQLite database created: %s\n", db_path))
