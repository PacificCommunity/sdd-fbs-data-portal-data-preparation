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
file_path <- "../data/bop_data.xlsx"

# List all sheet names
sheets <- excel_sheets(file_path)

# Initialize counter
i <- 1

# Loop through all sheets
while (i <= length(sheets)) {
  # Read the current sheet
  sheet <- read_excel(file_path, sheet = sheets[i])
  sheet <- merge(sheet, bopAcc, by = "label")
  #Move id column to the first column
  sheet <- sheet |> select(last_col(), everything())
  
  colHeader <- colnames(sheet)[4]
  selection <- sheet |>
    select(Id, colHeader, order) |>
    rename(ACCOUNT = Id)
  
  selection$TIME_PERIOD <- colHeader
  colnames(selection)[2] <- "OBS_VALUE"
  selection$FREQ = ifelse(nchar(selection$TIME_PERIOD)==4, "A", "Q")
  selection$OBS_STATUS <- ifelse(grepl("r", selection$TIME_PERIOD), "R",
                                ifelse(grepl("p", selection$TIME_PERIOD), "P", ""))
  selection$TIME_PERIOD <- gsub("\\s*\\[(r|p)\\]", "", selection$TIME_PERIOD)
  
  #Process first record
  
  datatable <- selection |>
    arrange(order) |>
    mutate(DATAFLOW = "FBOS:DF_BOP(1.0)",
           REF_AREA = "FJ",
           INDICATOR = "AMT",
           UNIT_MEASURE = "FJD",
           UNIT_MULT = 6,
           OBS_COMMENT = "",   
           DECIMALS = 1
           
    ) |>
    select(-order)
  
# Process the rest of the columns in the selected sheet  
  index = 5
  total_columns <- ncol(sheet)
  
  while (index <= total_columns){
    ncolHead <- colnames(sheet)[index]
    nextData <- sheet |> select(Id, ncolHead, order) |> rename(ACCOUNT = Id)
    nextData$TIME_PERIOD <- ncolHead
    colnames(nextData)[2] <- "OBS_VALUE"
    nextData$FREQ <- ifelse(nchar(nextData$TIME_PERIOD) ==4, "A", "Q")
    nextData$OBS_STATUS <- ifelse(grepl("r", nextData$TIME_PERIOD), "R",
                                  ifelse(grepl("p", nextData$TIME_PERIOD), "P", ""))
    nextData$TIME_PERIOD <- gsub("\\s*\\[(r|p)\\]", "", nextData$TIME_PERIOD)
    nextData <- nextData |>
      arrange(order) |>
      mutate(DATAFLOW = "FBOS:DF_BOP(1.0)",
             REF_AREA = "FJ",
             INDICATOR = "AMT",
             UNIT_MEASURE = "FJD",
             UNIT_MULT = 6,
             OBS_COMMENT = "",
             DECIMALS = 1,
      ) |>
      select(-order)
      
    datatable <- rbind(datatable, nextData)
    index <- index + 1
  }
  
  datatable <- datatable |>
    select(DATAFLOW, FREQ, REF_AREA, INDICATOR, ACCOUNT, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, OBS_COMMENT, DECIMALS)
  
  #Write final final to the csv ouput file
  output_file <- paste0("../output/bop/" ,sheets[i], ".csv")
  write.csv(datatable, output_file, row.names = FALSE)

  # Move to next sheet
  i <- i + 1
}
