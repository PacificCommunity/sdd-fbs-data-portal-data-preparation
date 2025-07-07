# Load libraries
library(dplyr)
library(data.table)
library(readxl)


# Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

# Path to your Excel file
file_path <- "../data/na_data.xlsx"

# Get list of sheet names
sheet_names <- excel_sheets(file_path)

i = 1
numsheet = length(sheet_names)

while (i <= numsheet) {
  # Process tables
  table <- read_excel(file_path, sheet = sheet_names[i])
  # Reshape from wide to long format
  table_long <- table %>%
    pivot_longer(
      cols = -c(DATAFLOW:DECIMALS),
      names_to = "TIME_PERIOD",
      values_to = "OBS_VALUE"
    )
  
  # Re-order the columns in the proper order
  table_long <- table_long |>
    mutate(across(everything(), ~replace(., is.na(.), "")),
           TRANSFORMATION = ifelse(TIME_PERIOD == "Weight", "WGT", TRANSFORMATION),
           TIME_PERIOD = ifelse(TIME_PERIOD == "Weight", "2014", TIME_PERIOD),
           OBS_STATUS = case_when(
             str_detect(TIME_PERIOD, "\\(YTD\\)") ~ "YTD",
             str_detect(TIME_PERIOD, "\\(P\\)") ~ "P",
             str_detect(TIME_PERIOD, "\\(R\\)") ~ "R",
             TRUE ~ ""),
           TIME_PERIOD = str_trim(str_remove_all(TIME_PERIOD, "\\s*\\(YTD\\)|\\s*\\(P\\)|\\s*\\(R\\)")),
           across(everything(), ~replace(., is.na(.), ""))
    ) |>
    relocate(OBS_VALUE, .before = UNIT_MEASURE) |>
    relocate(TIME_PERIOD, .before = TRANSFORMATION) |>
    filter(!is.na(OBS_VALUE) & OBS_VALUE != "")
  
  sheetName <- paste0("../output/na/",sheet_names[i],".csv")
  
  # Output table1 to output csv file
  write.csv(table_long, sheetName, row.names = FALSE)
  
  i <- i + 1
}