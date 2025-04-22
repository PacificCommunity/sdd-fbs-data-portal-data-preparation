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
           INDICATOR = "RGDP",
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
  
  rgdpPercent <- rbind(rgdpPercent, nextData)
  index <- index + 1
}

#Append both the real gdp and percentage change together

realGDP_combine <- rbind(realgdp, rgdpPercent)

realGDP_combine <- realGDP_combine |>
  select(FREQ, REF_AREA, INDICATOR, INDUSTRY, GDP_BREAKDOWN, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, BASE_PER, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, DECIMALS)

#Write the real gdp table to the output folder
write.csv(realGDP_combine, "../output/na/GDP_REAL.csv", row.names = FALSE)



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

#Write the final file to the output folder
write.csv(combine_nominal_gdp, "../output/na/GDP_NOMINAL.csv", row.names = FALSE)

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

#Write gdp contribution to the output folder
write.csv(rgdpContribution, "../output/na/GDP_CONTRIBUTION.csv", row.names = FALSE)

#### ************************** GDP market and non market contribution ***************************************** ####

gdp_market <- read_excel("../data/rgdp_data.xlsx", sheet = "GDP_MARKET")

colHeader <- colnames(gdp_market)[3]
selection <- gdp_market |>
  select(id, colHeader, label) |>
  rename(INDUSTRY = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
gdpMarket <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                                    ifelse(INDUSTRY == "GVA", "GVA", 
                            ifelse(INDUSTRY == "TAX", "TAX", "RGDP"))),
         GDP_BREAKDOWN = ifelse(label == "Market", "MKT",
                                ifelse(label == "Non-Market", "NMK", "_T")), 
         TRANSFORMATION = "N",
         UNIT_MEASURE = "FJD",
         UNIT_MULT = 6,
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         BASE_PER = "",
         DATA_SOURCE = "",
         OBS_COMMENT = "",
         DECIMALS = 1,
         TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4))
  ) |>
  select(-label)


index = 4
total_columns <- ncol(gdp_market)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(gdp_market)[index]
  nextData <- gdp_market |> select(id, ncolHead, label)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    rename(INDUSTRY = id) |>
    mutate(FREQ = "A",
           REF_AREA = "FJ",
           INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                              ifelse(INDUSTRY == "GVA", "GVA", 
                                     ifelse(INDUSTRY == "TAX", "TAX", "RGDP"))),
           GDP_BREAKDOWN = ifelse(label == "Market", "MKT",
                                  ifelse(label == "Non-Market", "NMK", "_T")),
           TRANSFORMATION = "N",
           UNIT_MEASURE = "FJD",
           UNIT_MULT = 6,
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           BASE_PER = "",
           DATA_SOURCE = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4))
    ) |>
    select(-label)
  
  
  gdpMarket <- rbind(gdpMarket, nextData)
  index <- index + 1
}


#Combing the nominal gdp and gdp percent change
gdpMarket <- gdpMarket |>
  select(FREQ, REF_AREA, INDICATOR, INDUSTRY, GDP_BREAKDOWN, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, BASE_PER, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, DATA_SOURCE, OBS_COMMENT, DECIMALS)


#### ************************** GDP market and non market contribution percentage***************************************** ####

gdp_market_per <- read_excel("../data/rgdp_data.xlsx", sheet = "GDP_MARKET_PER")

colHeader <- colnames(gdp_market_per)[3]
selection <- gdp_market_per |>
  select(id, colHeader, label) |>
  rename(INDUSTRY = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
gdpMarket_per <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                            ifelse(INDUSTRY == "GVA", "GVA", 
                                   ifelse(INDUSTRY == "TAX", "TAX", "RGDP"))),
         GDP_BREAKDOWN = ifelse(label == "Market", "MKT",
                                ifelse(label == "Non-Market", "NMK", "_T")), 
         TRANSFORMATION = "G1Y",
         UNIT_MEASURE = "PERCENT",
         UNIT_MULT = "",
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         BASE_PER = "",
         DATA_SOURCE = "",
         OBS_COMMENT = "",
         DECIMALS = 1,
         TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4))
  ) |>
  select(-label)


index = 4
total_columns <- ncol(gdp_market_per)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(gdp_market_per)[index]
  nextData <- gdp_market_per |> select(id, ncolHead, label)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    rename(INDUSTRY = id) |>
    mutate(FREQ = "A",
           REF_AREA = "FJ",
           INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                              ifelse(INDUSTRY == "GVA", "GVA", 
                                     ifelse(INDUSTRY == "TAX", "TAX", "RGDP"))),
           GDP_BREAKDOWN = ifelse(label == "Market", "MKT",
                                  ifelse(label == "Non-Market", "NMK", "_T")),
           TRANSFORMATION = "G1Y",
           UNIT_MEASURE = "PERCENT",
           UNIT_MULT = "",
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           BASE_PER = "",
           DATA_SOURCE = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4))
    ) |>
    select(-label)
    
  
  gdpMarket_per <- rbind(gdpMarket_per, nextData)
  index <- index + 1
}

#Write gdp contribution to the output folder
gdp_market_non_market <- rbind(gdpMarket, gdpMarket_per)

write.csv(gdp_market_non_market, "../output/na/GDP_MARKET_NON_MARKET.csv", row.names = FALSE)


#### ************************** GDP formal and informal contribution ***************************************** ####

gdp_formal_informal <- read_excel("../data/rgdp_data.xlsx", sheet = "GDP_FORMAL_INFORMAL")

colHeader <- colnames(gdp_formal_informal)[3]
selection <- gdp_formal_informal |>
  select(id, colHeader, label) |>
  rename(INDUSTRY = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
gdp_formal_informal_DT <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                            ifelse(INDUSTRY == "RGDP", "RGDP", 
                                   ifelse(INDUSTRY == "NTAX", "RTAX", 
                                          ifelse(INDUSTRY == "NME", "NME", "CRGDP")))),
         GDP_BREAKDOWN = ifelse(label == "Formal", "FOR",
                                ifelse(label == "Informal", "INF", 
                                       ifelse(label=="Subsistence", "SUB",
                                              ifelse(label =="Owner Occupied Dwellings", "OOD",
                                                     ifelse(label=="Activities of private households as employers of domestic staff", "PAS", 
                                                            ifelse(label == "Of which non-monetary economy", "NME",  "_T")))))), 
         
         TRANSFORMATION = "N",
         UNIT_MEASURE = ifelse(TIME_PERIOD == "Bweight", "PERCENT", "FJD"),
         UNIT_MULT = 6,
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         BASE_PER = "",
         OBS_COMMENT = "",
         DECIMALS = 1,
         TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4)),
         INDUSTRY = ifelse(nchar(INDUSTRY) > 2, "_T", INDUSTRY)
  ) |>
  select(-label)


index = 4
total_columns <- ncol(gdp_formal_informal)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(gdp_formal_informal)[index]
  nextData <- gdp_formal_informal |> select(id, ncolHead, label)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    rename(INDUSTRY = id) |>
    mutate(FREQ = "A",
           REF_AREA = "FJ",
           INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                              ifelse(INDUSTRY == "RGDP", "RGDP", 
                                     ifelse(INDUSTRY == "NTAX", "RTAX", 
                                            ifelse(INDUSTRY == "NME", "NME", "CRGDP")))),
           GDP_BREAKDOWN = ifelse(label == "Formal", "FOR",
                                  ifelse(label == "Informal", "INF", 
                                         ifelse(label=="Subsistence", "SUB",
                                                ifelse(label =="Owner Occupied Dwellings", "OOD",
                                                       ifelse(label=="Activities of private households as employers of domestic staff", "PAS", 
                                                              ifelse(label == "Of which non-monetary economy", "NME",  "_T")))))),
           TRANSFORMATION = "N",
           UNIT_MEASURE = ifelse(TIME_PERIOD == "Bweight", "PERCENT", "FJD"),
           UNIT_MULT = 6,
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           BASE_PER = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4)),
           INDUSTRY = ifelse(nchar(INDUSTRY) > 2, "_T", INDUSTRY)
    ) |>
    select(-label)
  
  
  gdp_formal_informal_DT <- rbind(gdp_formal_informal_DT, nextData)
  index <- index + 1
}


#Combing the nominal gdp and gdp percent change
gdp_formal_informal_DT <- gdp_formal_informal_DT |>
  select(FREQ, REF_AREA, INDICATOR, INDUSTRY, GDP_BREAKDOWN, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, BASE_PER, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, OBS_COMMENT, DECIMALS)


#### ************************** GDP formal and informal contribution percentages ***************************************** ####

gdp_formal_informal_per <- read_excel("../data/rgdp_data.xlsx", sheet = "GDP_FORMAL_INFORMAL_PER")

colHeader <- colnames(gdp_formal_informal_per)[3]
selection <- gdp_formal_informal_per |>
  select(id, colHeader, label) |>
  rename(INDUSTRY = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
gdp_formal_informal_per_DT <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                            ifelse(INDUSTRY == "RGDP", "RGDP", 
                                   ifelse(INDUSTRY == "NTAX", "RTAX", 
                                          ifelse(INDUSTRY == "NME", "NME", "CRGDP")))),
         GDP_BREAKDOWN = ifelse(label == "Formal", "FOR",
                                ifelse(label == "Informal", "INF", 
                                       ifelse(label=="Subsistence", "SUB",
                                              ifelse(label =="Owner Occupied Dwellings", "OOD",
                                                     ifelse(label=="Activities of private households as employers of domestic staff", "PAS", 
                                                            ifelse(label == "Of which non-monetary economy", "NME",  "_T")))))), 
         
         TRANSFORMATION = "G1Y",
         UNIT_MEASURE = "PERCENT",
         UNIT_MULT = 6,
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         BASE_PER = "",
         OBS_COMMENT = "",
         DECIMALS = 1,
         TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4)),
         INDUSTRY = ifelse(nchar(INDUSTRY) > 2, "_T", INDUSTRY)
  ) |>
  select(-label)


index = 4
total_columns <- ncol(gdp_formal_informal_per)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(gdp_formal_informal_per)[index]
  nextData <- gdp_formal_informal_per |> select(id, ncolHead, label)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    rename(INDUSTRY = id) |>
    mutate(FREQ = "A",
           REF_AREA = "FJ",
           INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                              ifelse(INDUSTRY == "RGDP", "RGDP", 
                                     ifelse(INDUSTRY == "NTAX", "RTAX", 
                                            ifelse(INDUSTRY == "NME", "NME", "CRGDP")))),
           GDP_BREAKDOWN = ifelse(label == "Formal", "FOR",
                                  ifelse(label == "Informal", "INF", 
                                         ifelse(label=="Subsistence", "SUB",
                                                ifelse(label =="Owner Occupied Dwellings", "OOD",
                                                       ifelse(label=="Activities of private households as employers of domestic staff", "PAS", 
                                                              ifelse(label == "Of which non-monetary economy", "NME",  "_T")))))),
           TRANSFORMATION = "G1Y",
           UNIT_MEASURE = ifelse(TIME_PERIOD == "Bweight", "PERCENT", "FJD"),
           UNIT_MULT = 6,
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           BASE_PER = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4)),
           INDUSTRY = ifelse(nchar(INDUSTRY) > 2, "_T", INDUSTRY)
    ) |>
    select(-label)
  
  
  gdp_formal_informal_per_DT <- rbind(gdp_formal_informal_per_DT, nextData)
  index <- index + 1
}


#Combing the nominal gdp and gdp percent change
gdp_formal_informal_per_DT <- gdp_formal_informal_per_DT |>
  select(FREQ, REF_AREA, INDICATOR, INDUSTRY, GDP_BREAKDOWN, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, BASE_PER, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, OBS_COMMENT, DECIMALS)

#Combine the two dataframes together

gdp_formal_informal_combine <- rbind(gdp_formal_informal_DT, gdp_formal_informal_per_DT)

#Write the final datafram to the csv ouput file

write.csv(gdp_formal_informal_combine, "../output/na/GDP_FORMAL_INFORMAL.csv", row.names = FALSE)


#### ************************** GDP Nominal cross output ***************************************** ####

gdp_gross_output <- read_excel("../data/rgdp_data.xlsx", sheet = "NOMINAL_GROSS_OUTPUT")

colHeader <- colnames(gdp_gross_output)[3]
selection <- gdp_gross_output |>
  select(id, colHeader, label) |>
  rename(INDUSTRY = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
gdp_gross_output_DT <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                            ifelse(INDUSTRY == "RGDP", "RGDP", 
                                   ifelse(INDUSTRY == "NTAX", "RTAX", 
                                          ifelse(INDUSTRY == "NME", "NME", "CRGDP")))),
         GDP_BREAKDOWN = ifelse(label == "Formal", "FOR",
                                ifelse(label == "Informal", "INF", 
                                       ifelse(label=="Subsistence", "SUB",
                                              ifelse(label =="Owner Occupied Dwellings", "OOD",
                                                     ifelse(label=="Activities of private households as employers of domestic staff", "PAS", 
                                                            ifelse(label == "Of which non-monetary economy", "NME",  "_T")))))), 
         
         TRANSFORMATION = "N",
         UNIT_MEASURE = ifelse(TIME_PERIOD == "Bweight", "PERCENT", "FJD"),
         UNIT_MULT = 6,
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         BASE_PER = "",
         OBS_COMMENT = "",
         DECIMALS = 1,
         TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4)),
         INDUSTRY = ifelse(nchar(INDUSTRY) > 2, "_T", INDUSTRY)
  ) |>
  select(-label)


index = 4
total_columns <- ncol(gdp_gross_output)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(gdp_gross_output)[index]
  nextData <- gdp_gross_output |> select(id, ncolHead, label)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    rename(INDUSTRY = id) |>
    mutate(FREQ = "A",
           REF_AREA = "FJ",
           INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                              ifelse(INDUSTRY == "RGDP", "RGDP", 
                                     ifelse(INDUSTRY == "NTAX", "RTAX", 
                                            ifelse(INDUSTRY == "NME", "NME", "CRGDP")))),
           GDP_BREAKDOWN = ifelse(label == "Formal", "FOR",
                                  ifelse(label == "Informal", "INF", 
                                         ifelse(label=="Subsistence", "SUB",
                                                ifelse(label =="Owner Occupied Dwellings", "OOD",
                                                       ifelse(label=="Activities of private households as employers of domestic staff", "PAS", 
                                                              ifelse(label == "Of which non-monetary economy", "NME",  "_T")))))),
           TRANSFORMATION = "N",
           UNIT_MEASURE = ifelse(TIME_PERIOD == "Bweight", "PERCENT", "FJD"),
           UNIT_MULT = 6,
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           BASE_PER = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4)),
           INDUSTRY = ifelse(nchar(INDUSTRY) > 2, "_T", INDUSTRY)
    ) |>
    select(-label)
  
  
  gdp_gross_output_DT <- rbind(gdp_gross_output_DT, nextData)
  index <- index + 1
}


#### ************************** GDP Nominal intermediate cost ***************************************** ####

gdp_intermediate_cost <- read_excel("../data/rgdp_data.xlsx", sheet = "NOMINAL_INTERMEDIATE_COST")

colHeader <- colnames(gdp_intermediate_cost)[3]
selection <- gdp_intermediate_cost |>
  select(id, colHeader, label) |>
  rename(INDUSTRY = id)

selection$TIME_PERIOD <- colHeader
colnames(selection)[2] <- "OBS_VALUE"

#Get first record
gdp_intermediate_cost_DT <- selection |>
  mutate(FREQ = "A",
         REF_AREA = "FJ",
         INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                            ifelse(INDUSTRY == "RGDP", "RGDP", 
                                   ifelse(INDUSTRY == "NTAX", "RTAX", 
                                          ifelse(INDUSTRY == "NME", "NME", "CRGDP")))),
         GDP_BREAKDOWN = ifelse(label == "Formal", "FOR",
                                ifelse(label == "Informal", "INF", 
                                       ifelse(label=="Subsistence", "SUB",
                                              ifelse(label =="Owner Occupied Dwellings", "OOD",
                                                     ifelse(label=="Activities of private households as employers of domestic staff", "PAS", 
                                                            ifelse(label == "Of which non-monetary economy", "NME",  "_T")))))), 
         
         TRANSFORMATION = "N",
         UNIT_MEASURE = ifelse(TIME_PERIOD == "Bweight", "PERCENT", "FJD"),
         UNIT_MULT = 6,
         OBS_STATUS =  ifelse(grepl("r", TIME_PERIOD), "R",
                              ifelse(grepl("p", TIME_PERIOD), "P", "")),
         BASE_PER = "",
         OBS_COMMENT = "",
         DECIMALS = 1,
         TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4)),
         INDUSTRY = ifelse(nchar(INDUSTRY) > 2, "_T", INDUSTRY)
  ) |>
  select(-label)


index = 4
total_columns <- ncol(gdp_intermediate_cost)

#Loop to get the other columns

while (index <= total_columns){
  ncolHead <- colnames(gdp_intermediate_cost)[index]
  nextData <- gdp_intermediate_cost |> select(id, ncolHead, label)
  nextData$TIME_PERIOD <- ncolHead
  colnames(nextData)[2] <- "OBS_VALUE"
  nextData$OBS_VALUE <- as.numeric(nextData$OBS_VALUE)
  nextData <- nextData |>
    rename(INDUSTRY = id) |>
    mutate(FREQ = "A",
           REF_AREA = "FJ",
           INDICATOR = ifelse(TIME_PERIOD =="Bweight", "WGT", 
                              ifelse(INDUSTRY == "RGDP", "RGDP", 
                                     ifelse(INDUSTRY == "NTAX", "RTAX", 
                                            ifelse(INDUSTRY == "NME", "NME", "CRGDP")))),
           GDP_BREAKDOWN = ifelse(label == "Formal", "FOR",
                                  ifelse(label == "Informal", "INF", 
                                         ifelse(label=="Subsistence", "SUB",
                                                ifelse(label =="Owner Occupied Dwellings", "OOD",
                                                       ifelse(label=="Activities of private households as employers of domestic staff", "PAS", 
                                                              ifelse(label == "Of which non-monetary economy", "NME",  "_T")))))),
           TRANSFORMATION = "N",
           UNIT_MEASURE = ifelse(TIME_PERIOD == "Bweight", "PERCENT", "FJD"),
           UNIT_MULT = 6,
           OBS_STATUS = ifelse(grepl("r", TIME_PERIOD), "R",
                               ifelse(grepl("p", TIME_PERIOD), "P", "")),
           BASE_PER = "",
           OBS_COMMENT = "",
           DECIMALS = 1,
           TIME_PERIOD = ifelse(TIME_PERIOD == "Bweight", "_T", substr(TIME_PERIOD, 1, 4)),
           INDUSTRY = ifelse(nchar(INDUSTRY) > 2, "_T", INDUSTRY)
    ) |>
    select(-label)
  
  
  gdp_intermediate_cost_DT <- rbind(gdp_intermediate_cost_DT, nextData)
  index <- index + 1
}

#Combine gdp_gross_output_DT and gdp_intermediate_cost_DT otgether

nominal_gross_intermediate <- rbind(gdp_gross_output_DT, gdp_intermediate_cost_DT)

#Reorder the columns
nominal_gross_intermediate <- nominal_gross_intermediate |>
  select(FREQ, REF_AREA, INDICATOR, INDUSTRY, GDP_BREAKDOWN, TRANSFORMATION, TIME_PERIOD, OBS_VALUE, BASE_PER, UNIT_MEASURE, UNIT_MULT, OBS_STATUS, OBS_COMMENT, DECIMALS)

#write the final datframe to the output csv file

write.csv(nominal_gross_intermediate, "../output/na/NOMINAL_GROSS_OUTPUT_INTERMEDIATE_COST.csv", row.names = FALSE)


