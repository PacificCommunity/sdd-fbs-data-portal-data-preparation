library(dplyr)
library(data.table)
library(readxl)

#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#Get the balance of paymwent accoutns
bopAcc <- read.csv("../data/bopAccounts.csv")

#### *************************** Process tab1_BOP ****************************************** ####

tab1_bop <- read_excel("../data/bop_data.xlsx", sheet = "tab1_BOP")

#merge with the bopAcc to get the account codes
tab1_bop_bopAcc <- merge(tab1_bop, bopAcc, by = "label")


colHeader <- colnames(tab1_bop_bopAcc)[2]
selection <- tab1_bop_bopAcc |>
  select(Id, colHeader) |>
  rename(ACCOUNT = Id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Process first record

tab1_bop_DT <- selection |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "Q"),
         REF_AREA = "FJ",
         INDICATOR = "AMT",
         UNIT_MEASURE = "FJD",
         UNIT_MULT = 6,
         OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                             ifelse(grepl("p", TIME_PERIOD), "P", "")),
         OBS_COMMENT = "",   
         DECIMAL = 1
         
         )

index = 3
total_columns <- ncol(tab1_bop_bopAcc)

while (index <= total_columns){
  ncolHead <- colnames(tab1_bop_bopAcc)[index]
  nextData <- tab1_bop_bopAcc |> select(Id, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData <- nextData |>
    mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "Q"),
           REF_AREA = "FJ",
           INDICATOR = "AMT",
           UNIT_MEASURE = "FJD",
           UNIT_MULT = 6,
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           OBS_COMMENT = "",   
           DECIMAL = 1
    ) |>
    rename(ACCOUNT = Id)
  tab1_bop_DT <- rbind(tab1_bop_DT, nextData)
  index <- index + 1
}



  
  





