# Load libraries
library(readxl)
library(dplyr)
library(data.table)

#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Read in the codelist csv file
bopAcc <- read.csv("../data/bopAccounts.csv")

# Path to the Excel workbook
file_path <- "../data/bopData.xlsx"

# List all sheet names
sheet_names <- excel_sheets(file_path)

# Initialize counter
i <- 1

# Loop through all sheets
while (i <= length(sheet_names)) {
  # Read the current sheet
  table <- read_excel(file_path, sheet = sheet_names[i])
  table <- merge(table, bopAcc, by = "label")
  table <- table |> 
    relocate(ACCOUNT, .before = UNIT_MEASURE) |>
    arrange(order) |>
    select(-order, -label)
  
  # Re-organise the data using pivot_long
  table_long <- table |>
    pivot_longer(
      cols = -c(DATAFLOW:DECIMALS),
      names_to = "TIME_PERIOD",
      values_to = "OBS_VALUE"
    )
  
  # Re-organising the columns and imputing the columns accordingly
  table_long <- table_long |>
          mutate(across(everything(), ~replace(., is.na(.), "")),
                 # Determine whether FREQ is A, Q or M depending on the time_period
                 FREQ = case_when(
                   grepl("-Q[1-4]", TIME_PERIOD) ~ "Q",
                   grepl("-0[1-9]|-1[0-2]", TIME_PERIOD) ~ "M",
                   TRUE ~ "A"
                 ),
                 # Populating the obs_status based on what's been specified in the time_period
                 OBS_STATUS = case_when(
                   str_detect(TIME_PERIOD, "\\(YTD\\)") ~ "YTD",
                   str_detect(TIME_PERIOD, "\\(P\\)") ~ "P",
                   str_detect(TIME_PERIOD, "\\(R\\)") ~ "R",
                   TRUE ~ ""),
                 # remove the brackets with the contents from the TIME_PERIOD values
                 TIME_PERIOD = str_trim(str_remove_all(TIME_PERIOD, "\\s*\\(YTD\\)|\\s*\\(P\\)|\\s*\\(R\\)"))
          ) |>
          relocate(OBS_VALUE, .before = UNIT_MEASURE) |>
          relocate(TIME_PERIOD, .before = OBS_VALUE)    
    
  sheetName <- paste0("../output/bop/",sheet_names[i],".csv")
  
  # Output table1 to output csv file
  write.csv(table_long, sheetName, row.names = FALSE)
  
  i <- i + 1
  
}
