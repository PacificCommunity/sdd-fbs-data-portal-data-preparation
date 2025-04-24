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
  select(Id, colHeader, label) |>
  rename(ACCOUNT = Id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Process first record





