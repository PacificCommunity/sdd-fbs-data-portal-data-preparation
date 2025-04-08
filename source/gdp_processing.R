library(dplyr)
library(data.table)
library(readxl)


#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#### ************************** Table base weight processing ********************************** ####

rgdpWeight <- read_excel("../data/gdp_data.xlsx", sheet = "RGDP_VAL")
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
         OBS_COMMENT = ""
         )

#re-order the columns in the proper order
rgdpWeight_DT <- rgdpWeight_DT |>
  select(FREQ, TIME_PERIOD, REF_AREA, INDICATOR, TRANSFORMATION, INDUSTRY_TYPE, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT)

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
         TRANSFORMATION = "YM1",
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
           TRANSFORMATION = "YM1",
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
  select(FREQ, TIME_PERIOD, REF_AREA, INDICATOR, TRANSFORMATION, INDUSTRY_TYPE, OBS_VALUE, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT)

#Combine the datatables together

realgdp <- rbind(rgdpWeight_DT, rgdp_DT)














