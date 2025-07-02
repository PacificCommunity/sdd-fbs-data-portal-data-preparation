# Load libraries
library(dplyr)
library(data.table)
library(readxl)

#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

# Path to your Excel file
file_path <- "../data/cpiData.xlsx"

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
        cols = -c(DATAFLOW:BASE_PER),
        names_to = "TIME_PERIOD",
        values_to = "OBS_VALUE"
      )
    
    # Re-order the columns in the proper order
    table_long <- table_long |>
      mutate(FREQ = case_when(
             grepl("-Q[1-4]", TIME_PERIOD) ~ "Q",
             grepl("-0[1-9]|-1[0-2]", TIME_PERIOD) ~ "M",
             TRUE ~ "A"
            ),
            across(everything(), ~replace(., is.na(.), ""))) |>
      relocate(OBS_VALUE, .before = UNIT_MEASURE) |>
      relocate(TIME_PERIOD, .before = OBS_VALUE)
    
    sheetName <- paste0("../output/cpi/",sheet_names[i],".csv")
    
    # Output table1 to output csv file
    write.csv(table_long, sheetName, row.names = FALSE)
    
    i <- i + 1
}
