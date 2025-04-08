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
  rename(INDUSTRY_TYPE = id, OBS_VALUE = bWeight) |>
  mutate(FREQ = "A",
         TIME_PERIOD = "_T",
         REF_AREA = "FJ",
         INDICATOR = "NRWGT",
         TRANSFORMATION = "_T",
         UNIT_MEASURE = "",
         UNIT_MULT  ="",
         OBS_STATUS  = "",
         DATA_SOURCE = "",
         OBS_COMMENT = "",
         DECIMALS = 1
         )

#re-order the columns in the proper order
rgdpWeight_DT <- rgdpWeight_DT |>
  select(FREQ, TIME_PERIOD, REF_AREA, INDICATOR, TRANSFORMATION, INDUSTRY_TYPE, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, DECIMALS)

#Remove the bWeight column from the dataframe

rgdpWeight_v2 <- rgdpWeight |> select(-bWeight) 

colHeader <- colnames(rgdpWeight_v2)[3]
selection <- rgdpWeight_v2 |>
  select(id, colHeader) |>
  rename(INDUSTRY_TYPE = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
rgdp <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = "RLGDP",
         TRANSFORMATION = "",
         UNIT_MEASURE = "FJD",
         UNIT_MULT = 6,
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
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
           TRANSFORMATION = "",
           UNIT_MEASURE = "FJD",
           UNIT_MULT = 6,
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           DATA_SOURCE = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
    ) |>
    rename(INDUSTRY_TYPE = id)
  
  rgdp <- rbind(rgdp, nextData)
  index <- index + 1
}

rgdp_DT <- rgdp |>
  select(FREQ, TIME_PERIOD, REF_AREA, INDICATOR, TRANSFORMATION, INDUSTRY_TYPE, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, DECIMALS)

#Combine the datatables together

realgdp <- rbind(rgdpWeight_DT, rgdp_DT)

#### ******************************** Real gdp percentage processing ***************************************** ####

gdpPercent <- read_excel("../data/rgdp_data.xlsx", sheet = "RGDP_PER")

colHeader <- colnames(gdpPercent)[3]
selection <- gdpPercent |>
  select(id, colHeader) |>
  rename(INDUSTRY_TYPE = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
rgdpPercent <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = "RLGDP",
         TRANSFORMATION = "YM1",
         UNIT_MEASURE = "PERCENT",
         UNIT_MULT = "",
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
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
           INDICATOR = "RLGDP",
           TRANSFORMATION = "YM1",
           UNIT_MEASURE = "PERCENT",
           UNIT_MULT = "",
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           DATA_SOURCE = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
    ) |>
    rename(INDUSTRY_TYPE = id)
  
  rgdpPercent <- rbind(rgdpPercent, nextData)
  index <- index + 1
}

#Append both the real gdp and percentage change together

realGDP_combine <- rbind(realgdp, rgdpPercent)

realGDP_combine <- realGDP_combine |>
  select(FREQ, TIME_PERIOD, REF_AREA, INDICATOR, TRANSFORMATION, INDUSTRY_TYPE, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, DECIMALS)

#### ******************************** Norminal gdp percentage processing ***************************************** ####

ngdp_val <- read_excel("../data/rgdp_data.xlsx", sheet = "NGDP_VAL")

colHeader <- colnames(ngdp_val)[3]
selection <- ngdp_val |>
  select(id, colHeader) |>
  rename(INDUSTRY_TYPE = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
ngdpVal <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = "NRGDP",
         TRANSFORMATION = "",
         UNIT_MEASURE = "FJD",
         UNIT_MULT = "",
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         DATA_SOURCE = "",
         OBS_COMMENT = "",
         DECIMALS = 1,
         TIME_PERIOD = substr(TIME_PERIOD, 1, 4)
  )

index = 4
total_columns <- ncol(ngdp_val)







