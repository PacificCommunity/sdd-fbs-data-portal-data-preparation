library(dplyr)
library(data.table)
library(readxl)


#Directory path
repository <- file.path(dirname(rstudioapi::getSourceEditorContext()$path))
setwd(repository)

#### ************************** Table 1 processing ********************************** ####

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
         )

