####################################
## FuSion - Plate layout creation ##
## Author:      Laurens Hahn      ##
## Last Update: 21.09.2024        ##
####################################

# LIBRARIES ---------------------------------------------------------------

library(xml2)
library(dplyr)
library(tidyr)
library(purrr)
library(readr)
library(stringr)
library(writexl)

# FUNCTIONS ---------------------------------------------------------------

# Function for splitting the file name
get_prefix <- function(filename) {
  strsplit(filename, "_")[[1]][1]
}

# PARAMETERS --------------------------------------------------------------

# plate_layout: Name of the MF plate used for the screen
# Has to match with the sample metadata, Tecan print template and print report
plate_layout <- "MFXVX"

# DIRECTORIES -------------------------------------------------------------

# Input
tdd_input_dir <- "YOUR_FILE_PATH"
print_report_input_dir <- "YOUR_FILE_PATH"

# Output
plate_layout_output_dir <- "YOUR_FILE_PATH"
plate_output_dir <- file.path(plate_layout_output_dir, plate_layout)
plate_layout_output <- file.path(plate_output_dir, paste0(plate_layout, "_Layout.xlsx"))

# SCRIPT ------------------------------------------------------------------

# If not run in pipeline change value in parameters
if (!exists("plate_layout")) {
  plate_layout <- plate_layout
}

# File import

# List all available files
tdd_file_names <- list.files(tdd_input_dir)
print_report_names <- list.files(print_report_input_dir)

# Empty vector for matching file names
matching_tdd_file_name <- c()
matching_print_report_name <- c()

# Loop for matching file names to plate layout in TDD-Files and print reports
for (file in tdd_file_names) {
  prefix <- get_prefix(file)
  if (prefix == plate_layout) {
    matching_tdd_file_name <- c(matching_tdd_file_name, file)
  }
}

for (file in print_report_names) {
  prefix <- get_prefix(file)
  if (prefix == plate_layout) {
    matching_print_report_name <- c(matching_print_report_name, file)
  }
}

# Paths of required files
tdd_file_path <- file.path(tdd_input_dir, matching_tdd_file_name)
print_report_path <- file.path(print_report_input_dir, matching_print_report_name)

# Load and adapt tdd-file
tdd_file_xml <- read_xml(tdd_file_path)

# Parse fluids information into a data frame
tdd_fluids <- tdd_file_xml %>%
  xml_find_all("//Fluid") %>%
  map_df(~{
    fluid_id <- xml_attr(.x, "ID")
    name <- xml_text(xml_find_first(.x, "Name"))
    data.frame(fluid_id = fluid_id, name = name, stringsAsFactors = FALSE)
  })

# Remove rows with missing names
tdd_fluids <- na.omit(tdd_fluids, cols = c("name"))

# Create an empty plate layout data frame
tdd_file <- expand.grid(column = 1:24, row = 1:16) %>%
  mutate(
    fluid_id = NA_character_,
    concentration = NA_real_
  ) %>%
  arrange(row, column)

# Parse wells and fill in the plate layout
tdd_wells <- tdd_file_xml %>%
  xml_find_all("//Well") %>%
  map_df(~{
    row <- as.numeric(xml_attr(.x, "Row")) + 1
    col <- as.numeric(xml_attr(.x, "Col")) + 1
    fluid_node <- xml_find_first(.x, "Fluid")
    fluid_id <- xml_attr(fluid_node, "ID")
    concentration <- as.numeric(xml_text(fluid_node))
    data.frame(row = row, column = col, fluid_id = fluid_id, concentration = concentration, stringsAsFactors = FALSE)
  })

# Merge well data into tdd_file
tdd_file <- tdd_file %>%
  select(-fluid_id, -concentration) %>%
  left_join(tdd_wells, by = c("row", "column"))

# Replace fluid_id with name from fluids and remove fluid_id
tdd_file <- tdd_file %>%
  left_join(tdd_fluids, by = c("fluid_id" = "fluid_id")) %>%
  mutate(
    name = ifelse(is.na(name), "empty", name)
  )

# Process concentration
tdd_file$concentration <- tdd_file$concentration/200

tdd_file <- tdd_file %>%
  mutate(
    concentration = ifelse(is.na(concentration), 0, concentration)
    )

# Select required columns, set column names and order
tdd_file <- tdd_file[, -3]
colnames(tdd_file) <- c('column','row', 'tdd_concentration', 'tdd_fluid')
tdd_file <- tdd_file[, c(2,1,4,3)]

# Load and adapt print report
print_report_xml <- read_xml(print_report_path)

# Extract worksheet "Tabular"
print_report_xml_tabular <- xml_find_first(print_report_xml, '//ss:Worksheet[@ss:Name="Tabular"]', ns = c(ss = 'urn:schemas-microsoft-com:office:spreadsheet'))

# Extract all values marked with "Row"
print_report_xml_tabular_rows <- xml_find_all(print_report_xml_tabular, './/ss:Row', ns = c(ss = 'urn:schemas-microsoft-com:office:spreadsheet'))

# Function for extracting cell values
extract_cell_values <- function(row) {
  cells <- xml_find_all(row, './/ss:Cell/ss:Data', ns = c(ss = 'urn:schemas-microsoft-com:office:spreadsheet'))
  values <- xml_text(cells)
  return(values)
  }

# Extract values for each row
print_report_xml_tabular_row_values <- lapply(print_report_xml_tabular_rows, extract_cell_values)

# Find the maximum number of columns
max_cols <- max(sapply(print_report_xml_tabular_row_values, length))

# Create a data frame with NA values for missing cells
print_report <- do.call(rbind, lapply(print_report_xml_tabular_row_values, function(x) {
  length(x) <- max_cols
  return(x)
}))

# Convert to a data frame 
print_report <- as.data.frame(print_report, stringsAsFactors = FALSE)
colnames(print_report) <- paste0('V', seq_len(ncol(print_report)))

# Convert the first row to column headers
colnames(print_report) <- as.character(unlist(print_report[1, ]))
print_report <- print_report[-1,]
rownames(print_report) <- NULL

# Select required columns, set column names and order
print_report <- print_report[, -c(1:3, 6, 11:ncol(print_report))]
colnames(print_report) <- c('row_rand', 'column_rand', 'row', 'column', 'pr_fluid', 'pr_concentration')
print_report <- print_report[, c(3,4,1,2,5,6)]

# Replace NA with "untreated"
print_report <- print_report %>%
  mutate(
    pr_fluid = ifelse(is.na(pr_fluid), "empty", pr_fluid)
  )

# Correct value types and concentration
print_report$row <- as.numeric(print_report$row)
print_report$column <- as.numeric(print_report$column)
print_report$row_rand <- as.numeric(print_report$row_rand)
print_report$column_rand <- as.numeric(print_report$column_rand)
print_report$pr_concentration <- as.numeric(print_report$pr_concentration)
print_report$pr_concentration <- print_report$pr_concentration/200
print_report <- print_report %>%
  mutate(
    pr_concentration = ifelse(is.na(pr_concentration), 0, pr_concentration)
  )
  
# Extract units
volume_unit <- xml_text(xml_find_first(tdd_file_xml, "//VolumeUnit"))
molarity_unit <- xml_text(xml_find_first(tdd_file_xml, "//MolarityConcentrationUnit"))
mass_concentration_unit <- xml_text(xml_find_first(tdd_file_xml, "//MassConcentrationUnit"))

# Merge tdd-file with print_report
plate_layout_template <- merge(tdd_file, print_report, by = c("row", "column"))
plate_layout_template <- plate_layout_template[,c(1,2,5,6,3,7,4,8)]

# Check if fluids are the same
# Loop through the rows and compares tdd_fluid with pr_fluid
for (i in 1:nrow(plate_layout_template)) {
  if (plate_layout_template$tdd_fluid[i] != plate_layout_template$pr_fluid[i]) {
    print(paste("Row", i, ": The fluid from the Tecan template =", plate_layout_template$tdd_fluid[i], "is not equal to fluid in the print report =", plate_layout_template$pr_fluid[i]))
  }
}

# Loop through the rows and compares tdd_concentration with pr_concentration
for (i in 1:nrow(plate_layout_template)) {
  lower_bound <- plate_layout_template$tdd_concentration[i] * 0.9
  upper_bound <- plate_layout_template$tdd_concentration[i] * 1.1
  
  if (plate_layout_template$pr_concentration[i] < lower_bound | plate_layout_template$pr_concentration[i] > upper_bound) {
    print(paste("Row", i, ": The print report concentration =", plate_layout_template$pr_concentration[i], "is not within 10% of the Tecan templates concentration =", plate_layout_template$tdd_concentration[i]))
  }
}

# Select and format concentration and fluids
plate_layout_template <- plate_layout_template[, -c(6,8)]
colnames(plate_layout_template) <- c('row', 'column', 'row_rand', 'column_rand', 'name', 'concentration')

# Create wellID and change rows to letter
plate_layout_template <- plate_layout_template %>%
  mutate(
    row = LETTERS[row], 
    row_rand = LETTERS[row_rand],
    wellID = ifelse(
      column_rand < 10,
      sprintf("%s0 %d", row_rand, column_rand),
      sprintf("%s0%d", row_rand, column_rand)
    )
  ) %>%
  select(row, column, row_rand, column_rand, name, concentration, wellID)

# Arrange by WellID and order
plate_layout_template <- plate_layout_template %>%
  arrange(wellID)

# Rename replicates
# Create a column to track if "rep_" should be added
plate_layout_template$modified_name <- plate_layout_template$name

# Create a vector to track duplicates based on both name and concentration
name_concentration_combo <- paste(plate_layout_template$name, plate_layout_template$concentration, sep = "_")

# Loop through each row to apply the "rep_" prefix where needed
for (i in 1:nrow(plate_layout_template)) {
  # Skip names that shouldn't get "rep_"
  if (plate_layout_template$name[i] %in% c("Benzethonium Chloride", "BzCl", "Benzethonium Cl", "untreated", "empty","Staurosporine")) {
    next
  }
  
  # Identify the second occurrence based on the name-concentration combination
  if (sum(name_concentration_combo[1:i] == name_concentration_combo[i]) == 2) {
    plate_layout_template$modified_name[i] <- paste0("rep_", plate_layout_template$name[i])
  }
}

# Apply "rep_" to half of the "DMSO" rows at random
dmso_indices <- which(plate_layout_template$name == "DMSO")

# Randomly select half of these rows
random_dmso_indices <- sample(dmso_indices, length(dmso_indices) / 2)

# Apply "rep_" to the randomly selected "DMSO" rows
plate_layout_template$modified_name[random_dmso_indices] <- paste0("rep_", plate_layout_template$name[random_dmso_indices])

# Apply "rep_" to half of the "Water + Tween20 0,5%" rows at random
wt_indices <- which(plate_layout_template$name == "Water + Tween20 0,5%")

# Randomly select half of these rows
random_wt_indices <- sample(wt_indices, length(wt_indices) / 2)

# Apply "rep_" to the randomly selected "Water + Tween20 0,5%" rows
plate_layout_template$modified_name[random_wt_indices] <- paste0("rep_", plate_layout_template$name[random_wt_indices])

# Remove old name and reorder
plate_layout_template <- plate_layout_template[, -5]
colnames(plate_layout_template) <- c('row', 'column', 'row_rand', 'column_rand', 'concentration', 'wellID', 'name')
plate_layout_template <- plate_layout_template[, c(6,1,2,3,4,7,5)]

# Replace "Benzethonium Chloride" and "Benzethonium Cl" with "BzCl" in the 'name' column
plate_layout_template$name[plate_layout_template$name %in% c("Benzethonium Chloride", "Benzethonium Cl")] <- "BzCl"

# Replace "Benzethonium Chloride" and "Benzethonium Cl" with "BzCl" in the 'name' column
plate_layout_template$name[plate_layout_template$name %in% "Water + T"] <- "WT"
plate_layout_template$name[plate_layout_template$name %in% "rep_Water + T"] <- "rep_WT"

# Create wellAnno files
wellAnno <- plate_layout_template
wellAnno <- wellAnno[, -c(2,3,4,5)]
wellAnno <- wellAnno %>%
  arrange(wellID)
  
# Create plateAnno files
plateAnno_matrix <- matrix(data = '', nrow = 1, ncol = 4)
plateAnno <- data.frame(plateAnno_matrix, row.names = NULL)
names(plateAnno) <- c('fileName', 'batch', 'sampleID', 'patientID')

# Ensure the output directory exists
if (!dir.exists(plate_output_dir)) {
  dir.create(plate_output_dir, recursive = TRUE)
  cat("Directory created at:", plate_output_dir, "\n")
} else {
  cat("Directory already exists at:", plate_output_dir, "\n")
}

# Ensure the subdirectories exist
plateAnno_dir <- file.path(plate_output_dir, 'plateAnno')
if (!dir.exists(plateAnno_dir)) {
  dir.create(plateAnno_dir, recursive = TRUE)
  cat("Subdirectory 'plateAnno' created at:", plateAnno_dir, "\n")
}

wellAnno_dir <- file.path(plate_output_dir, 'wellAnno')
if (!dir.exists(wellAnno_dir)) {
  dir.create(wellAnno_dir, recursive = TRUE)
  cat("Subdirectory 'wellAnno' created at:", wellAnno_dir, "\n")
}

# Save the plate_layout_template data frame as an Excel file if it doesn't exist
if (!file.exists(plate_layout_output)) {
  write_xlsx(plate_layout_template, plate_layout_output)
  cat("Excel file created at:", plate_layout_output, "\n")
} else {
  cat("Excel file already exists at:", plate_layout_output, "\n")
}

# Save the wellAnno data frame as a CSV file if it doesn't exist
wellAnno_output <- file.path(wellAnno_dir, 'wellAnno_final.csv')
if (!file.exists(wellAnno_output)) {
  write.csv2(wellAnno, file = wellAnno_output, row.names = FALSE)
  cat("CSV file created at:", wellAnno_output, "\n")
} else {
  cat("CSV file already exists at:", wellAnno_output, "\n")
}

# Save the plateAnno data frame as a CSV file if it doesn't exist
plateAnno_output <- file.path(plateAnno_dir, 'plateAnno.csv')
if (!file.exists(plateAnno_output)) {
  write.csv2(plateAnno, file = plateAnno_output, row.names = FALSE)
  cat("CSV file created at:", plateAnno_output, "\n")
} else {
  cat("CSV file already exists at:", plateAnno_output, "\n")
}
