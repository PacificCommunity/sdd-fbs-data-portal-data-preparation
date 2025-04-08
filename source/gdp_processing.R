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










