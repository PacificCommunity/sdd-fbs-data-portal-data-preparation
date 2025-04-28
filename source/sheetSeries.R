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
  sheet <- merge(data, bopAcc, by = "label")  
  
  colHeader <- colnames(sheet)[3]
  selection <- sheet |>
    select(Id, colHeader) |>
    rename(ACCOUNT = Id)
  
  selection$TIME_PERIOD <- colHeader
  colnames(selection)[2] <- "OBS_VALUE"
  
  #Process first record
  
  datatable <- selection |>
    #arrange(order) |>
    mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "Q"),
           REF_AREA = "FJ",
           INDICATOR = "AMT",
           UNIT_MEASURE = "FJD",
           UNIT_MULT = 6,
           OBS_STATUS = "",
           OBS_COMMENT = "",   
           DECIMALS = 1
           
    )
  
# Process the rest of the columns in the selected sheet  
  index = 4
  total_columns <- ncol(sheet)
  
  while (index <= total_columns){
    ncolHead <- colnames(sheet)[index]
    nextData <- sheet |> select(Id, ncolHead) |> rename(ACCOUNT = Id)
    nextData$TIME_PERIOD <- ncolHead
    colnames(nextData)[2] <- "OBS_VALUE"
    nextData <- nextData |>
      #arrange(order) |>
      mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "Q"),
             REF_AREA = "FJ",
             INDICATOR = "AMT",
             UNIT_MEASURE = "FJD",
             UNIT_MULT = 6,
             OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                                 ifelse(grepl("p", TIME_PERIOD), "P", "")),
             OBS_COMMENT = "",
             DECIMALS = 1,
             TIME_PERIOD = gsub("\\s*\\[(r|p)\\]", "", TIME_PERIOD)
      )
      
    datatable <- rbind(datatable, nextData)
    index <- index + 1
  }
  
  datatable <- datatable |>
    select(FREQ, REF_AREA, INDICATOR, ACCOUNT, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, OBS_COMMENT, DECIMALS)
  
  #Write final final to the csv ouput file
  write.csv(datatable, "../output/bop/sheets/",sheets[i],".csv")

  # Process the data (example: just print first few rows)
  #cat("Processing sheet:", sheets[i], "\n")
  #print(head(data))
  
  # Move to next sheet
  i <- i + 1
}
