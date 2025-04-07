library(dplyr)
library(data.table)
library(readxl)


#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#### ************************** Table weight processing ********************************** ####

cpiWeight <- read_excel("../data/cpi_data.xlsx", sheet = "weight")

cpiWeight <- cpiWeight |>
  mutate(FREQ = "A",
         TIME_PERIOD = "_T",
         REF_AREA = "FJ",
         INDICATOR = "WGT",
         ITEM = code,
         TRANSFORMATION = "N",
         SEASONAL_ADJUST = "N",
         OBS_VALUE = Weight,
         UNIT_MEASURE = "INDEX",
         BASE_YEAR = "_T",
         OBS_STATUS = "",
         COMMENT = "",
         DECIMALS = 1
         ) |>
  select(-code, -Division, -Weight)

#### ************************** Table index processing ********************************** ####

cpi_index <- read_excel("../data/cpi_data.xlsx", sheet = "index")

colHeader <- colnames(cpi_index)[2]
selection <- cpi_index |>
  select(item, colHeader) |>
  rename(ITEM = item)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
cpiIndex <- selection |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
         REF_AREA = "FJ",
         INDICATOR = "IDX",
         TRANSFORMATION = ifelse(nchar(TIME_PERIOD)==4, "GIY", "G1M"),
         SEASONAL_ADJUST = "S",
         UNIT_MEASURE = "INDEX",
         BASE_YEAR = "",
         OBS_STATUS = "",
         COMMENT = "",
         DECIMALS = 1
         )

index = 3
total_columns <- ncol(cpi_index)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(cpi_index)[index]
  nextData <- cpi_index |> select(item, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    mutate(FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
    REF_AREA = "FJ",
    INDICATOR = "IDX",
    TRANSFORMATION = ifelse(nchar(TIME_PERIOD)==4, "GIY", "G1M"),
    SEASONAL_ADJUST = "S",
    UNIT_MEASURE = "INDEX",
    BASE_YEAR = "",
    OBS_STATUS = "",
    COMMENT = "",
    DECIMALS = 1
    ) |>
  rename(ITEM = item)
  
  cpiIndex <- rbind(cpiIndex, nextData)
  index <- index + 1
}

#### ************************** Table of seasonal index changes processing ********************************** ####

mmSeasonal <- read_excel("../data/cpi_data.xlsx", sheet = "mnthSeasonal")

mmSeasonal <- mmSeasonal |>
  mutate(FREQ = "M",
         TIME_PERIOD = period,
         REF_AREA = "FJ",
         INDICATOR = "IDX",
         ITEM = "_T",
         TRANSFORMATION = "N",
         SEASONAL_ADJUST = "S",
         OBS_VALUE = SAdjusted,
         UNIT_MEASURE = "INDEX",
         BASE_YEAR = "_T",
         OBS_STATUS = "",
         COMMENT = "",
         DECIMALS = 1
  ) |>
  select(-Division, -Month, -period, -SAdjusted)

#### ************************** Table monthly seasonal changes ********************************** ####

mmChange <- read_excel("../data/cpi_data.xlsx", sheet = "mmChange")

colHeader <- colnames(mmChange)[2]
selection <- mmChange |>
  select(ITEM, colHeader)
  #rename(ITEM = item)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
mmIndex <- selection |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
         REF_AREA = "FJ",
         INDICATOR = "IDX",
         TRANSFORMATION = ifelse(nchar(TIME_PERIOD)==4, "GIY", "G1M"),
         SEASONAL_ADJUST = "S",
         UNIT_MEASURE = "INDEX",
         BASE_YEAR = "",
         OBS_STATUS = "",
         COMMENT = "",
         DECIMALS = 1
  )

index = 3
total_columns <- ncol(mmChange)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(mmChange)[index]
  nextData <- mmChange |> select(ITEM, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    mutate(FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
           REF_AREA = "FJ",
           INDICATOR = "IDX",
           TRANSFORMATION = ifelse(nchar(TIME_PERIOD)==4, "GIY", "G1M"),
           SEASONAL_ADJUST = "S",
           UNIT_MEASURE = "INDEX",
           BASE_YEAR = "",
           OBS_STATUS = "",
           COMMENT = "",
           DECIMALS = 1
    )
  
  mmIndex <- rbind(mmIndex, nextData)
  index <- index + 1
}

#### ************************** Table of national seasonal index changes processing ********************************** ####

mmSeasonal <- read_excel("../data/cpi_data.xlsx", sheet = "natChange")

mmSeasonal <- mmSeasonal |>
  select(TIME_PERIOD, OBS_VALUE) |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "M"),
         REF_AREA = "FJ",
         INDICATOR = "IDX",
         ITEM = "_T",
         TRANSFORMATION = "N",
         SEASONAL_ADJUST = "S",
         UNIT_MEASURE = "INDEX",
         BASE_YEAR = "_T",
         OBS_STATUS = "",
         COMMENT = "",
         DECIMALS = 1
  )

#### ************************** Table of national inflation rates processing ********************************** ####

avgInflation <- read_excel("../data/cpi_data.xlsx", sheet = "natChange")

avgInflation <- avgInflation |>
  filter(!is.na(avgInflation)) |>
  select(TIME_PERIOD, avgInflation) |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "M"),
         REF_AREA = "FJ",
         INDICATOR = "IDX",
         ITEM = "_T",
         TRANSFORMATION = "N",
         SEASONAL_ADJUST = "S",
         UNIT_MEASURE = "INDEX",
         BASE_YEAR = "_T",
         OBS_STATUS = "",
         COMMENT = "",
         DECIMALS = 1
  )|>
  rename(OBS_VALUE = avgInflation)


#### ************************** Table of national CPI change processing ********************************** ####

natCPI <- read_excel("../data/cpi_data.xlsx", sheet = "natCPI")

colHeader <- colnames(natCPI)[2]
selection <- natCPI |>
  select(ITEM, colHeader)
#rename(ITEM = item)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
natCPIIndex <- selection |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
         REF_AREA = "FJ",
         INDICATOR = "IDX",
         TRANSFORMATION = ifelse(nchar(TIME_PERIOD)==4, "GIY", "G1M"),
         SEASONAL_ADJUST = "N",
         UNIT_MEASURE = "INDEX",
         BASE_YEAR = 2014,
         OBS_STATUS = "",
         COMMENT = "",
         DECIMALS = 1
  )

index = 3
total_columns <- ncol(natCPI)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(natCPI)[index]
  nextData <- natCPI |> select(ITEM, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    mutate(FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
           REF_AREA = "FJ",
           INDICATOR = "IDX",
           TRANSFORMATION = ifelse(nchar(TIME_PERIOD)==4, "GIY", "G1M"),
           SEASONAL_ADJUST = "N",
           UNIT_MEASURE = "INDEX",
           BASE_YEAR = 2014,
           OBS_STATUS = "",
           COMMENT = "",
           DECIMALS = 1
    )
  
  natCPIIndex <- rbind(natCPIIndex, nextData)
  index <- index + 1
}


#### ************************** Table of Central inflation rate change processing ********************************** ####

centralInflation <- read_excel("../data/cpi_data.xlsx", sheet = "centralInflation")

centralInflation <- centralInflation |>
  filter(!is.na(avgInflation)) |>
  select(TIME_PERIOD, avgInflation) |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "M"),
         REF_AREA = "FJCD",
         INDICATOR = "IDX",
         ITEM = "_T",
         TRANSFORMATION = "N",
         SEASONAL_ADJUST = "N",
         UNIT_MEASURE = "INDEX",
         BASE_YEAR = 2014,
         OBS_STATUS = "",
         COMMENT = "",
         DECIMALS = 1
  )|>
  rename(OBS_VALUE = avgInflation)

#### ************************** Table of Central division CPI change processing ********************************** ####

centralCPI <- read_excel("../data/cpi_data.xlsx", sheet = "centralCPI")

colHeader <- colnames(centralCPI)[2]
selection <- centralCPI |>
  select(ITEM, colHeader)
#rename(ITEM = item)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
centralCPIIndex <- selection |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
         REF_AREA = "FJCD",
         INDICATOR = "IDX",
         TRANSFORMATION = ifelse(nchar(TIME_PERIOD)==4, "GIY", "G1M"),
         SEASONAL_ADJUST = "N",
         UNIT_MEASURE = "INDEX",
         BASE_YEAR = 2014,
         OBS_STATUS = "",
         COMMENT = "",
         DECIMALS = 1
  )

index = 3
total_columns <- ncol(centralCPI)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(centralCPI)[index]
  nextData <- centralCPI |> select(ITEM, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    mutate(FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
           REF_AREA = "FJCD",
           INDICATOR = "IDX",
           TRANSFORMATION = ifelse(nchar(TIME_PERIOD)==4, "GIY", "G1M"),
           SEASONAL_ADJUST = "N",
           UNIT_MEASURE = "INDEX",
           BASE_YEAR = 2014,
           OBS_STATUS = "",
           COMMENT = "",
           DECIMALS = 1
    )
  
  centralCPIIndex <- rbind(centralCPIIndex, nextData)
  index <- index + 1
}


#### ************************** Table of western inflation rate change processing ********************************** ####

westernInflation <- read_excel("../data/cpi_data.xlsx", sheet = "westernInflation")

westernInflation <- westernInflation |>
  filter(!is.na(avgInflation)) |>
  select(TIME_PERIOD, avgInflation) |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD)==4, "A", "M"),
         REF_AREA = "FJWD",
         INDICATOR = "IDX",
         ITEM = "_T",
         TRANSFORMATION = "N",
         SEASONAL_ADJUST = "N",
         UNIT_MEASURE = "INDEX",
         BASE_YEAR = 2014,
         OBS_STATUS = "",
         COMMENT = "",
         DECIMALS = 1
  )|>
  rename(OBS_VALUE = avgInflation)


#### ************************** Table of Western division CPI change processing ********************************** ####

westernCPI <- read_excel("../data/cpi_data.xlsx", sheet = "westernCPI")

colHeader <- colnames(westernCPI)[2]
selection <- westernCPI |>
  select(ITEM, colHeader)
#rename(ITEM = item)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
westernCPIIndex <- selection |>
  mutate(FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
         REF_AREA = "FJWD",
         INDICATOR = "IDX",
         TRANSFORMATION = ifelse(nchar(TIME_PERIOD)==4, "GIY", "G1M"),
         SEASONAL_ADJUST = "N",
         UNIT_MEASURE = "INDEX",
         BASE_YEAR = 2014,
         OBS_STATUS = "",
         COMMENT = "",
         DECIMALS = 1
  )

index = 3
total_columns <- ncol(westernCPI)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(westernCPI)[index]
  nextData <- westernCPI |> select(ITEM, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    mutate(FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
           REF_AREA = "FJWD",
           INDICATOR = "IDX",
           TRANSFORMATION = ifelse(nchar(TIME_PERIOD)==4, "GIY", "G1M"),
           SEASONAL_ADJUST = "N",
           UNIT_MEASURE = "INDEX",
           BASE_YEAR = 2014,
           OBS_STATUS = "",
           COMMENT = "",
           DECIMALS = 1
    )
  
  westernCPIIndex <- rbind(westernCPIIndex, nextData)
  index <- index + 1
}



























