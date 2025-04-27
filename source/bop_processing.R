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


colHeader <- colnames(tab1_bop_bopAcc)[3]
selection <- tab1_bop_bopAcc |>
  select(Id, colHeader, order) |>
  rename(ACCOUNT = Id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Process first record

tab1_bop_DT <- selection |>
  arrange(order) |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "Q"),
         REF_AREA = "FJ",
         INDICATOR = "AMT",
         UNIT_MEASURE = "FJD",
         UNIT_MULT = 6,
         OBS_STATUS = "",
         OBS_COMMENT = "",   
         DECIMALS = 1
         
         ) |>
  select(-order)

index = 4
total_columns <- ncol(tab1_bop_bopAcc)

while (index <= total_columns){
  ncolHead <- colnames(tab1_bop_bopAcc)[index]
  nextData <- tab1_bop_bopAcc |> select(Id, ncolHead, order) |> rename(ACCOUNT = Id)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData <- nextData |>
    arrange(order) |>
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
    ) |>
    select(-order)
    
  
  tab1_bop_DT <- rbind(tab1_bop_DT, nextData)
  index <- index + 1
}

#Reorder the columns into proper order

tab1_bop_DT <- tab1_bop_DT |>
  select(FREQ, REF_AREA, INDICATOR, ACCOUNT, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, OBS_COMMENT, DECIMALS)

#Write final final to the csv ouput file
write.csv(tab1_bop_DT, "../output/bop/1_BALANCE_OF_PAYMENT.csv", row.names = FALSE)

#### *************************** Process tab2_BOP ****************************************** ####


tab2_bop <- read_excel("../data/bop_data.xlsx", sheet = "tab2_BOP")

#merge with the bopAcc to get the account codes
tab2_bop_bopAcc <- merge(tab2_bop, bopAcc, by = "label")

colHeader <- colnames(tab2_bop_bopAcc)[3]
selection <- tab2_bop_bopAcc |>
  select(Id, colHeader, order) |>
  rename(ACCOUNT = Id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Process first record

tab2_bop_DT <- selection |>
  arrange(order) |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "Q"),
         REF_AREA = "FJ",
         INDICATOR = "AMT",
         UNIT_MEASURE = "FJD",
         UNIT_MULT = 6,
         OBS_STATUS = "",
         OBS_COMMENT = "",   
         DECIMALS = 1
         
  ) |>
  select(-order)

index = 4
total_columns <- ncol(tab2_bop_bopAcc)

while (index <= total_columns){
  ncolHead <- colnames(tab2_bop_bopAcc)[index]
  nextData <- tab2_bop_bopAcc |> select(Id, ncolHead, order) |> rename(ACCOUNT = Id)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData <- nextData |>
    arrange(order) |>
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
    ) |>
    select(-order)
  
  tab2_bop_DT <- rbind(tab2_bop_DT, nextData)
  index <- index + 1
}

#Reorder the columns into proper order

tab2_bop_DT <- tab2_bop_DT |>
  select(FREQ, REF_AREA, INDICATOR, ACCOUNT, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, OBS_COMMENT, DECIMALS)

#Write final final to the csv ouput file
write.csv(tab2_bop_DT, "../output/bop/2_CURRENT_ACCOUNTS_GOODS_SERVICES.csv", row.names = FALSE)


#### *************************** Process tab3_BOP ****************************************** ####

tab3_bop <- read_excel("../data/bop_data.xlsx", sheet = "tab3_BOP")

#merge with the bopAcc to get the account codes
tab3_bop_bopAcc <- merge(tab3_bop, bopAcc, by = "label")

colHeader <- colnames(tab3_bop_bopAcc)[3]
selection <- tab3_bop_bopAcc |>
  select(Id, colHeader, order) |>
  rename(ACCOUNT = Id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Process first record

tab3_bop_DT <- selection |>
  arrange(order) |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "Q"),
         REF_AREA = "FJ",
         INDICATOR = "AMT",
         UNIT_MEASURE = "FJD",
         UNIT_MULT = 6,
         OBS_STATUS = "",
         OBS_COMMENT = "",   
         DECIMALS = 1
         
  ) |>
  select(-order)

index = 4
total_columns <- ncol(tab3_bop_bopAcc)

while (index <= total_columns){
  ncolHead <- colnames(tab3_bop_bopAcc)[index]
  nextData <- tab3_bop_bopAcc |> select(Id, ncolHead, order) |> rename(ACCOUNT = Id)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData <- nextData |>
    arrange(order) |>
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
    ) |>
    select(-order)
  
  tab3_bop_DT <- rbind(tab3_bop_DT, nextData)
  index <- index + 1
}

#Reorder the columns into proper order

tab3_bop_DT <- tab3_bop_DT |>
  select(FREQ, REF_AREA, INDICATOR, ACCOUNT, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, OBS_COMMENT, DECIMALS)

#Write final final to the csv ouput file
write.csv(tab3_bop_DT, "../output/bop/3_PRIMARY_AND_SECONDARY_INCOME.csv", row.names = FALSE)


#### *************************** Process tab4_BOP ****************************************** ####

tab4_bop <- read_excel("../data/bop_data.xlsx", sheet = "tab4_BOP")

#merge with the bopAcc to get the account codes
tab4_bop_bopAcc <- merge(tab4_bop, bopAcc, by = "label")
tab4_bop_bopAcc <- tab4_bop_bopAcc[!grepl("^C", tab4_bop_bopAcc$Id), ]

colHeader <- colnames(tab4_bop_bopAcc)[3]
selection <- tab4_bop_bopAcc |>
  select(Id, colHeader, order) |>
  rename(ACCOUNT = Id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Process first record

tab4_bop_DT <- selection |>
  arrange(order) |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "Q"),
         REF_AREA = "FJ",
         INDICATOR = "AMT",
         UNIT_MEASURE = "FJD",
         UNIT_MULT = 6,
         OBS_STATUS = "",
         OBS_COMMENT = "",   
         DECIMALS = 1
         
  ) |>
  select(-order)

index = 4
total_columns <- ncol(tab4_bop_bopAcc)

while (index <= total_columns){
  ncolHead <- colnames(tab4_bop_bopAcc)[index]
  nextData <- tab4_bop_bopAcc |> select(Id, ncolHead, order) |> rename(ACCOUNT = Id)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData <- nextData |>
    arrange(order) |>
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
    ) |>
    select(-order)
  
  tab4_bop_DT <- rbind(tab4_bop_DT, nextData)
  index <- index + 1
}

#Reorder the columns into proper order

tab4_bop_DT <- tab4_bop_DT |>
  select(FREQ, REF_AREA, INDICATOR, ACCOUNT, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, OBS_COMMENT, DECIMALS)

#Write final final to the csv ouput file
write.csv(tab4_bop_DT, "../output/bop/4_CAPITAL_ACCOUNT.csv", row.names = FALSE)


#### *************************** Process tab5_BOP ****************************************** ####

tab5_bop <- read_excel("../data/bop_data.xlsx", sheet = "tab5_BOP")

#merge with the bopAcc to get the account codes
tab5_bop_bopAcc <- merge(tab5_bop, bopAcc, by = "label")

colHeader <- colnames(tab5_bop_bopAcc)[3]
selection <- tab5_bop_bopAcc |>
  select(Id, colHeader, order) |>
  rename(ACCOUNT = Id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Process first record

tab5_bop_DT <- selection |>
  arrange(order) |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "Q"),
         REF_AREA = "FJ",
         INDICATOR = "AMT",
         UNIT_MEASURE = "FJD",
         UNIT_MULT = 6,
         OBS_STATUS = "",
         OBS_COMMENT = "",   
         DECIMALS = 1
         
  ) |>
  select(-order)

index = 4
total_columns <- ncol(tab5_bop_bopAcc)

while (index <= total_columns){
  ncolHead <- colnames(tab5_bop_bopAcc)[index]
  nextData <- tab5_bop_bopAcc |> select(Id, ncolHead, order) |> rename(ACCOUNT = Id)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData <- nextData |>
    arrange(order) |>
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
           
    ) |>
    select(-order)
  
  tab5_bop_DT <- rbind(tab5_bop_DT, nextData)
  index <- index + 1
}

#Reorder the columns into proper order

tab5_bop_DT <- tab5_bop_DT |>
  select(FREQ, REF_AREA, INDICATOR, ACCOUNT, TIME_PERIOD, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, OBS_COMMENT, DECIMALS)

#Write final final to the csv ouput file
write.csv(tab5_bop_DT, "../output/bop/5_FINANCIAL_ACCOUNTS.csv", row.names = FALSE)

