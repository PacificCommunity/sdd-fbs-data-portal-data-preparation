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
        names_to = "ITEM",
        values_to = "OBS_VALUE"
      )
    
    # Re-order the columns in the proper order
    table_long <- table_long |>
      mutate(across(everything(), ~replace(., is.na(.), "")),
             SEASONAL_ADJUST = ifelse(ITEM == "S_T", "S", SEASONAL_ADJUST),
             ITEM = ifelse(ITEM == "S_T", "_T", ITEM)
             ) |>
      relocate(OBS_VALUE, .before = UNIT_MEASURE) |>
      relocate(ITEM, .before = TRANSFORMATION)
    
    sheetName <- paste0("../output/cpi/",sheet_names[i],".csv")
    
    # Output table1 to output csv file
    write.csv(table_long, sheetName, row.names = FALSE)
    
    i <- i + 1
}
