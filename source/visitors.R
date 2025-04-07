library(dplyr)
library(data.table)
library(readxl)


#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#### ************************** Table 1 processing ********************************** ####

visitors <- read_excel("../data/visitors_tables.xlsx", sheet = "T1")

colHeader <- colnames(visitors)[4]
selection <- visitors |>
  select(CODE, Indicator, TravelType, colHeader)

selection$TIME_PERIOD <- colHeader
colnames(selection)[4] <- "OBS_VALUE"

index = 5
total_columns <- ncol(visitors)

while (index <= total_columns){
  ncolHead <- colnames(visitors)[index]
  nextData <- visitors |> select(CODE, Indicator, TravelType, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[4] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  selection <- rbind(selection, nextData)
  index <- index +1
}

#Add additional columns
selection <- selection |>
  select(-CODE) |>
  mutate(DATAFLOW = "FBS:DF_VISITORS_TBL1(1.0)",
         FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
         REF_AREA = "FJ",
         UNIT_MEASURE = "N",
         UNIT_MULT = "",
         OBS_STATUS = "",
         COMMENT = "",
         ) |>
  rename(INDICATOR = Indicator, TRAVEL_TYPE = TravelType)

#Re-order the columns to a more logical order
selection <- selection |>
  select(
    DATAFLOW,
    FREQ,
    TIME_PERIOD,
    REF_AREA,
    INDICATOR,
    TRAVEL_TYPE,
    OBS_VALUE,
    UNIT_MEASURE,
    UNIT_MULT,
    OBS_STATUS,
    COMMENT
  )

#Write table to output csv file
write.csv(selection, "../output/visitors/DF_VISITORS_TBL1.csv", row.names = FALSE)


#### **************************** Table 2 processing ************************************* ####

visitors <- read_excel("../data/visitors_tables.xlsx", sheet = "T2")

colHeader <- colnames(visitors)[2]
selection <- visitors |>
  select(CODE, colHeader)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

index = 3
total_columns <- ncol(visitors)

while (index <= total_columns){
  ncolHead <- colnames(visitors)[index]
  nextData <- visitors |> select(CODE, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  selection <- rbind(selection, nextData)
  index <- index +1
}

#Add additional columns
selection <- selection |>
  mutate(DATAFLOW = "FBS:DF_VISITORS_TBL2(1.0)",
         FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
         REF_AREA = "FJ",
         UNIT_MEASURE = "N",
         INDICATOR = "ARR",
         UNIT_MULT = "",
         OBS_STATUS = "",
         COMMENT = "",
  ) |>
  rename(COUNTRY = CODE )

#Re-order the columns to a more logical order
selection <- selection |>
  select(
    DATAFLOW,
    FREQ,
    TIME_PERIOD,
    REF_AREA,
    INDICATOR,
    COUNTRY,
    OBS_VALUE,
    UNIT_MEASURE,
    UNIT_MULT,
    OBS_STATUS,
    COMMENT
  )

#Write table to output csv file
write.csv(selection, "../output/visitors/DF_VISITORS_TBL2.csv", row.names = FALSE)


#### **************************** Table 3 processing ************************************* ####

visitors <- read_excel("../data/visitors_tables.xlsx", sheet = "T3")

colHeader <- colnames(visitors)[2]
selection <- visitors |>
  select(CODE, colHeader)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

index = 3
total_columns <- ncol(visitors)

while (index <= total_columns){
  ncolHead <- colnames(visitors)[index]
  nextData <- visitors |> select(CODE, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  selection <- rbind(selection, nextData)
  index <- index +1
}

#Add additional columns
selection <- selection |>
  mutate(DATAFLOW = "FBS:DF_VISITORS_TBL3(1.0)",
         FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
         REF_AREA = "FJ",
         UNIT_MEASURE = "N",
         INDICATOR = "ARR",
         UNIT_MULT = "",
         OBS_STATUS = "",
         COMMENT = "",
  ) |>
  rename(TRAVEL_PURPOSE = CODE )

#Re-order the columns to a more logical order
selection <- selection |>
  select(
    DATAFLOW,
    FREQ,
    TIME_PERIOD,
    REF_AREA,
    INDICATOR,
    TRAVEL_PURPOSE,
    OBS_VALUE,
    UNIT_MEASURE,
    UNIT_MULT,
    OBS_STATUS,
    COMMENT
  )

#Write table to output csv file
write.csv(selection, "../output/visitors/DF_VISITORS_TBL3.csv", row.names = FALSE)


#### **************************** Table 4 processing ************************************* ####

visitors <- read_excel("../data/visitors_tables.xlsx", sheet = "T4")

colHeader <- colnames(visitors)[2]
selection <- visitors |>
  select(CODE, colHeader)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

index = 3
total_columns <- ncol(visitors)

while (index <= total_columns){
  ncolHead <- colnames(visitors)[index]
  nextData <- visitors |> select(CODE, ncolHead)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  selection <- rbind(selection, nextData)
  index <- index +1
}

#Add additional columns
selection <- selection |>
  mutate(DATAFLOW = "FBS:DF_VISITORS_TBL4(1.0)",
         FREQ = ifelse(nchar(TIME_PERIOD) == 4, "A", "M"),
         REF_AREA = "FJ",
         UNIT_MEASURE = "N",
         INDICATOR = "ARR",
         UNIT_MULT = "",
         OBS_STATUS = "",
         COMMENT = "",
  ) |>
  rename(COUNTRY = CODE)

#Re-order the columns to a more logical order
selection <- selection |>
  select(
    DATAFLOW,
    FREQ,
    TIME_PERIOD,
    REF_AREA,
    INDICATOR,
    COUNTRY,
    OBS_VALUE,
    UNIT_MEASURE,
    UNIT_MULT,
    OBS_STATUS,
    COMMENT
  )

#Write table to output csv file
write.csv(selection, "../output/visitors/DF_VISITORS_TBL4.csv", row.names = FALSE)
