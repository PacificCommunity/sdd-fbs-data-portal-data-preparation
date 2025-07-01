# Load libraries
library(dplyr)
library(readxl)
library(tidyverse)

#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

# Path to your Excel file
file_path <- "../data/visitor_Data.xlsx"

# Get list of sheet names
sheet_names <- excel_sheets(file_path)

i = 1
numsheet = length(sheet_names)

while (i <= numsheet) {
  # Process tables
  if (sheet_names[i] == "DF_VISITORS_TABLE5"){
    table <- read_excel(file_path, sheet = sheet_names[i])
    # Reshape from wide to long format
    table_long <- table %>%
      pivot_longer(
        cols = -c(DATAFLOW:DECIMALS),
        names_to = "AGE",
        values_to = "OBS_VALUE"
      )
    
    # Re-order the columns in the proper order
    table_long <- table_long |>
      mutate(across(everything(), ~replace(., is.na(.), ""))) |>
      relocate(OBS_VALUE, .before = UNIT_MEASURE) |>
      relocate(AGE, .before = TIME_PERIOD)
  }else if(sheet_names[i] == "DF_VISITORS_TABLE6") {
    table <- read_excel(file_path, sheet = sheet_names[i])
    # Reshape from wide to long format
    table_long <- table %>%
      pivot_longer(
        cols = -c(DATAFLOW:DECIMALS),
        names_to = "PURPOSE",
        values_to = "OBS_VALUE"
      )
    
    # Re-order the columns in the proper order
    table_long <- table_long |>
      mutate(across(everything(), ~replace(., is.na(.), ""))) |>
      relocate(OBS_VALUE, .before = UNIT_MEASURE) |>
      relocate(PURPOSE, .before = COUNTRY_RESIDENCE)
    
  } else{
  
  table <- read_excel(file_path, sheet = sheet_names[i])
  # Reshape from wide to long format
  table_long <- table %>%
    pivot_longer(
      cols = -c(DATAFLOW:DECIMALS),
      names_to = "TIME_PERIOD_ORIG",
      values_to = "OBS_VALUE"
    )
  
  # Re-order the columns in the proper order
  table_long <- table_long |>
    mutate(FREQ = case_when(
      grepl("-Q[1-4]", TIME_PERIOD_ORIG) ~ "Q",
      grepl("-0[1-9]|-1[0-2]", TIME_PERIOD_ORIG) ~ "M",
      TRUE ~ "A"
    ),
    
    OBS_STATUS = ifelse(grepl("\\(P\\)", TIME_PERIOD_ORIG), "P", ""),
    TIME_PERIOD = gsub(" \\(P\\)", "", TIME_PERIOD_ORIG),
    across(everything(), ~replace(., is.na(.), ""))) |>
    select(-TIME_PERIOD_ORIG) |>
    relocate(OBS_VALUE, .before = UNIT_MEASURE) |>
    relocate(TIME_PERIOD, .before = OBS_VALUE) |>
    relocate(FREQ, .before = REF_AREA) |>
    relocate(OBS_STATUS, .before = COMMENT)
  }
  
  sheetName <- paste0("../output/visitors/",sheet_names[i],".csv")
  
  # Output table1 to output csv file
  write.csv(table_long, sheetName, row.names = FALSE)
  
  i <- i + 1
}
