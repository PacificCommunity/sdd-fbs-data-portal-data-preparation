library(dplyr)
library(data.table)
library(readxl)


#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#### ************************** Table base weight processing ********************************** ####

rgdpWeight <- read_excel("../data/rgdp_data.xlsx", sheet = "RGDP_VAL")
rgdpWeight_DT <- rgdpWeight |>
  select(id, bWeight) |>
  rename(INDUSTRY = id, OBS_VALUE = bWeight) |>
  mutate(FREQ = "A",
         TIME_PERIOD = "2014",
         REF_AREA = "FJ",
         INDICATOR = "WGT",
         GDP_BREAKDOWN = "_T",
         TRANSFORMATION = "N",
         UNIT_MEASURE = "FJD",
         UNIT_MULT  =6,
         OBS_STATUS  = "",
         BASE_PER = "",
         DATA_SOURCE = "",
         OBS_COMMENT = "",
         DECIMALS = 1
         )

#re-order the columns in the proper order
rgdpWeight_DT <- rgdpWeight_DT |>
  select(FREQ, REF_AREA, INDICATOR, INDUSTRY, GDP_BREAKDOWN, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, BASE_PER, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, DECIMALS)

#Remove the bWeight column from the dataframe

rgdpWeight_v2 <- rgdpWeight |> select(-bWeight) 

colHeader <- colnames(rgdpWeight_v2)[3]
selection <- rgdpWeight_v2 |>
  select(id, colHeader) |>
  rename(INDUSTRY = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
rgdp <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = "RGDP",
         GDP_BREAKDOWN = "_T",
         TRANSFORMATION = "N",
         UNIT_MEASURE = "FJD",
         UNIT_MULT = 6,
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         BASE_PER = "",
         DATA_SOURCE = "",
         OBS_COMMENT = "",
         DECIMALS = 1
  )

index = 4
total_columns <- ncol(rgdpWeight_v2)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(rgdpWeight_v2)[index]
  nextData <- rgdpWeight_v2 |> select(id, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    mutate(FREQ = "A",
           REF_AREA = "FJ",
           INDICATOR = "RLGDP",
           GDP_BREAKDOWN = "_T",
           TRANSFORMATION = "N",
           UNIT_MEASURE = "FJD",
           UNIT_MULT = 6,
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           BASE_PER = "",
           DATA_SOURCE = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
    ) |>
    rename(INDUSTRY = id)
  
  rgdp <- rbind(rgdp, nextData)
  index <- index + 1
}

rgdp_DT <- rgdp |>
  select(FREQ, REF_AREA, INDICATOR, INDUSTRY, GDP_BREAKDOWN, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, BASE_PER, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, DECIMALS)

#Combine the datatables together

realgdp <- rbind(rgdpWeight_DT, rgdp_DT)

#### ******************************** Real gdp percentage processing ***************************************** ####

gdpPercent <- read_excel("../data/rgdp_data.xlsx", sheet = "RGDP_PER")

colHeader <- colnames(gdpPercent)[3]
selection <- gdpPercent |>
  select(id, colHeader) |>
  rename(INDUSTRY = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
rgdpPercent <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = "RGDP",
         GDP_BREAKDOWN = "_T",
         TRANSFORMATION = "G1Y",
         UNIT_MEASURE = "PERCENT",
         UNIT_MULT = "",
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         BASE_PER = "",
         DATA_SOURCE = "",
         OBS_COMMENT = "",
         DECIMALS = 1,
         TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
  )

index = 4
total_columns <- ncol(gdpPercent)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(gdpPercent)[index]
  nextData <- gdpPercent |> select(id, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    mutate(FREQ = "A",
           REF_AREA = "FJ",
           INDICATOR = "RGDP",
           GDP_BREAKDOWN = "",
           TRANSFORMATION = "G1Y",
           UNIT_MEASURE = "PERCENT",
           UNIT_MULT = "",
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           BASE_PER = "",
           DATA_SOURCE = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
    ) |>
    rename(INDUSTRY = id)
  
  rgdpPercent <- rbind(rgdpPercent, nextData)
  index <- index + 1
}

#Append both the real gdp and percentage change together

realGDP_combine <- rbind(realgdp, rgdpPercent)

realGDP_combine <- realGDP_combine |>
  select(FREQ, REF_AREA, INDICATOR, INDUSTRY, GDP_BREAKDOWN, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, BASE_PER, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, DECIMALS)

#### ******************************** Norminal gdp processing ***************************************** ####

ngdp_val <- read_excel("../data/rgdp_data.xlsx", sheet = "NGDP_VAL")

colHeader <- colnames(ngdp_val)[3]
selection <- ngdp_val |>
  select(id, colHeader) |>
  rename(INDUSTRY = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
ngdpVal <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = "NGDP",
         GDP_BREAKDOWN = "_T",
         TRANSFORMATION = "N",
         UNIT_MEASURE = "FJD",
         UNIT_MULT = 6,
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         BASE_PER = "",
         DATA_SOURCE = "",
         OBS_COMMENT = "",
         DECIMALS = 1,
         TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
  )

index = 4
total_columns <- ncol(ngdp_val)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(ngdp_val)[index]
  nextData <- ngdp_val |> select(id, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    mutate(FREQ = "A",
           REF_AREA = "FJ",
           INDICATOR = "NGDP",
           GDP_BREAKDOWN = "_T",
           TRANSFORMATION = "N",
           UNIT_MEASURE = "FJD",
           UNIT_MULT = 6,
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           BASE_PER = "",
           DATA_SOURCE = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
    ) |>
    rename(INDUSTRY = id)
  
  ngdpVal <- rbind(ngdpVal, nextData)
  index <- index + 1
}

#### ******************************** Nominal gdp percentage processing ***************************************** ####

ngdp_per <- read_excel("../data/rgdp_data.xlsx", sheet = "NGDP_PER")

colHeader <- colnames(ngdp_per)[3]
selection <- ngdp_per |>
  select(id, colHeader) |>
  rename(INDUSTRY = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
ngdpPercent <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = "NGDP",
         GDP_BREAKDOWN = "_T",
         TRANSFORMATION = "G1Y",
         UNIT_MEASURE = "PERCENT",
         UNIT_MULT = "",
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         BASE_PER = "",
         DATA_SOURCE = "",
         OBS_COMMENT = "",
         DECIMALS = 1,
         TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
  )


index = 4
total_columns <- ncol(ngdp_per)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(ngdp_per)[index]
  nextData <- ngdp_per |> select(id, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    mutate(FREQ = "A",
           REF_AREA = "FJ",
           INDICATOR = "NGDP",
           GDP_BREAKDOWN = "_T",
           TRANSFORMATION = "G1Y",
           UNIT_MEASURE = "PERCENT",
           UNIT_MULT = "",
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           BASE_PER = "",
           DATA_SOURCE = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
    ) |>
    rename(INDUSTRY = id)
  
  ngdpPercent <- rbind(ngdpPercent, nextData)
  index <- index + 1
}

#Combing the nominal gdp and gdp percent change

combine_nominal_gdp <- rbind(ngdpVal, ngdpPercent)

combine_nominal_gdp <- combine_nominal_gdp |>
  select(FREQ, REF_AREA, INDICATOR, INDUSTRY, GDP_BREAKDOWN, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, BASE_PER, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, DECIMALS)


#Combining both the real and nominal gdp

real_nominal_gdp_merge <- rbind(realGDP_combine, combine_nominal_gdp)

#Write the final file to the output folder
#write.csv(real_nominal_gdp_merge, "../output/na/DF_GDP.csv", row.names = FALSE)

#### ***************************** Real GDP Contribution ******************************************** ####

rgdp_cnt <- read_excel("../data/rgdp_data.xlsx", sheet = "RGDP_CNT")

colHeader <- colnames(rgdp_cnt)[3]
selection <- rgdp_cnt |>
  select(id, colHeader) |>
  rename(INDUSTRY = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
rgdpContribution <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = "CRGDP",
         GDP_BREAKDOWN = "_T",
         TRANSFORMATION = "",
         UNIT_MEASURE = "PERCENT",
         UNIT_MULT = "",
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         BASE_PER = "",
         DATA_SOURCE = "",
         OBS_COMMENT = "",
         DECIMALS = 1,
         TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
  )

index = 4
total_columns <- ncol(rgdp_cnt)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(rgdp_cnt)[index]
  nextData <- rgdp_cnt |> select(id, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    mutate(FREQ = "A",
           REF_AREA = "FJ",
           INDICATOR = "CRGDP",
           GDP_BREAKDOWN = "_T",
           TRANSFORMATION = "N",
           UNIT_MEASURE = "PERCENT",
           UNIT_MULT = "",
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           BASE_PER = "",
           DATA_SOURCE = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
    ) |>
    rename(INDUSTRY = id)
  
  rgdpContribution <- rbind(rgdpContribution, nextData)
  index <- index + 1
}

#Combing the nominal gdp and gdp percent change
rgdpContribution <- rgdpContribution |>
  select(FREQ, REF_AREA, INDICATOR, INDUSTRY, GDP_BREAKDOWN, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, BASE_PER, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, DECIMALS)



real_nominal_rcont_gdp <- rbind(real_nominal_gdp_merge, rgdpContribution)

write.csv(real_nominal_rcont_gdp, "../output/na/DF_GDP.csv", row.names = FALSE)
