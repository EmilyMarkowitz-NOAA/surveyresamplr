################################################################################
# Project: Query NEFSC BTS for "good to go" zero-filled CPUE data
#
# Program Name: Query.NEFSC.Data.R
#
# Summary: Imports and cleans the NEFSC BTS data from northeast data base
#
# Created: November 14, 2024
#
################################################################################

########################## Introduction ########################################
# Remove any values from the Environment
rm(list = ls())

# Library and Source Code/Functions
require(dplyr)
require(tidyr)
require(readxl)
library(RODBC)
# source("Oracle_User_Data.R")
# source("Disconnect_Oracle.R")

################################################################################

############################# Query Database ###################################
### Query station data
# I took this code from Kiersten Curti
# This is to identify just the relevant NEFSC BTS tows from the broader database
# of survey info (svdbs.union_fscs_svsta and svdbs.svdbs_cruises)

# First section: The R/V Albatross from 1976 - 2008
# Second section: The R/V Bigelow from 2009 - present

# Season: Everything EXCEPT summer
# Purpose Code 10: "NMFS NEFSC BOTTOM TRAWL SURVEY"
# Status Code 10: "Audited"
# Type Code 1: "Standard Random Tow"
# Operation Code 1-3: "Good Performance" or "Representative Tow"
# Gear Code 1-2:  "Good Tow" or "Representative"

stn.qry <- paste(
  "select u.cruise6,",
  "u.stratum,",
  "u.tow,",
  "u.station,",
  "u.shg,",
  "u.svvessel,",
  "u.svgear,",
  "u.est_year,",
  "u.est_month,",
  "u.est_day,",
  "substr(u.est_time,1,2) || substr(u.est_time,4,2) as time,",
  "round(u.dopdistb,3) as distb,",
  "round(u.dopdistw,3) as distw,",
  "u.avgdepth,",
  "u.area,",
  "round(u.bottemp,1) as btemp,",
  "c.season,round(u.beglat,4)as xbeglan,",
  "round(u.beglon,4) as xbeglon,",
  "u.decdeg_beglat, u.decdeg_beglon, u.decdeg_endlat, u.decdeg_endlon,",
  "u.type_code,",
  "u.operation_code,",
  "u.gear_code,",
  "u.acquisition_code,",
  "u.toga,",
  "u.airtemp,",
  "u.cloud,",
  "u.baropress,",
  "u.winddir,",
  "u.windsp,",
  "u.wavehgt,",
  "u.swellhgt,",
  "u.surftemp,",
  "u.surfsalin,",
  "u.botsalin,",
  "u.botspeed,",
  "u.towdur",
  "from svdbs.union_fscs_svsta u, svdbs.svdbs_cruises c",
  "where u.cruise6 = c.cruise6 and",
  "c.Season != 'SUMMER' and",
  "c.purpose_code = 10 and",
  "c.STATUS_CODE=10 and",
  "c.YEAR BETWEEN '1976' AND '2008' and",
  "u.shg<= '123'",
  "union all",
  "select u.cruise6,",
  "u.stratum,",
  "u.tow,",
  "u.station,",
  "u.shg,",
  "u.svvessel,",
  "u.svgear,",
  "u.est_year,",
  "u.est_month,",
  "u.est_day,",
  "substr(u.est_time,1,2) || substr(u.est_time,4,2) as time,",
  "round(u.dopdistb,3) as distb,",
  "round(u.dopdistw,3) as distw,",
  "u.avgdepth,",
  "u.area,",
  "round(u.bottemp,1) as btemp,",
  "c.season,",
  "round(u.beglat,4)as xbeglan,",
  "round(u.beglon,4) as xbeglon,",
  "u.decdeg_beglat, u.decdeg_beglon, u.decdeg_endlat, u.decdeg_endlon,",
  "u.type_code,",
  "u.operation_code,",
  "u.gear_code,",
  "u.acquisition_code,",
  "u.toga,",
  "u.airtemp,",
  "u.cloud,",
  "u.baropress,",
  "u.winddir,",
  "u.windsp,",
  "u.wavehgt,",
  "u.swellhgt,",
  "u.surftemp,",
  "u.surfsalin,",
  "u.botsalin,",
  "u.botspeed,",
  "u.towdur",
  "from svdbs.union_fscs_svsta u, svdbs.svdbs_cruises c",
  "where u.cruise6 = c.cruise6 and",
  "c.Season != 'SUMMER' and",
  "c.purpose_code = 10 and",
  "c.STATUS_CODE=10 and",
  "c.YEAR BETWEEN '2009' AND '2023' and",
  "u.type_code <= '1' and",
  "u.operation_code <= '3' and",
  "u.gear_code <= '2'",
  "order by cruise6, stratum, tow, station"
)

source("Oracle_User_Data.R")
stn.data <- fetch(dbSendQuery(db1, stn.qry))
source("Disconnect_Oracle.R")

# Rename some of the column headings to make it more descriptive:
stn.data <- rename(
  stn.data,
  Cruise = CRUISE6,
  Stratum = STRATUM,
  Tow = TOW,
  Station = STATION,
  Vessel = SVVESSEL,
  Year = EST_YEAR,
  Month = EST_MONTH,
  Day = EST_DAY,
  Time = TIME,
  Depth.m = AVGDEPTH,
  StatArea = AREA,
  Temp.bottom = BTEMP,
  Season = SEASON,
  Latitude = XBEGLAN,
  Longitude = XBEGLON,
  BeginLat = DECDEG_BEGLAT,
  BeginLon = DECDEG_BEGLON,
  EndLat = DECDEG_ENDLAT,
  EndLon = DECDEG_ENDLON,
  AirTemp = AIRTEMP,
  CloudCoverPercent = CLOUD,
  BarometricPressure = BAROPRESS,
  WindDirection = WINDDIR,
  WindSpeed = WINDSP,
  WaveHeight = WAVEHGT,
  SwellHeight = SWELLHGT,
  Temp.surface = SURFTEMP,
  Salinity.surface = SURFSALIN,
  Salinity.bottom = BOTSALIN,
  TowDurationMin = TOWDUR,
  BottomSpeedKnots = BOTSPEED
) |>
  select(
    -c(
      SHG,
      SVGEAR,
      DISTB,
      DISTW,
      TYPE_CODE,
      OPERATION_CODE,
      GEAR_CODE,
      ACQUISITION_CODE,
      TOGA
    )
  )
dim(stn.data)
unique(stn.data$Vessel)

################################################################################
#svspp codes
# Red Hake - 077
# Silver Hake - 072
# Cod - 073
# Mackerel- 121
# Summer Flounder - 103
# Black Sea Bass - 141
#
# Requests and upcoming research tracks:
# Lobster - 301
# Atlantic Herring - 032
# Sea Scallop - 401
# Longfin inshore squid - 503
# Winter Flounder - 023
# Monkfish/Goosefish - 197
# Scup - 143

# Specify species names and their SVSPP Codes:
speciesnames <- c(
  "RedHake",
  "SilverHake",
  "Cod",
  "Mackerel",
  "SummerFlounder",
  "BlackSeaBass",
  "Lobster",
  "AtlanticHerring",
  "SeaScallop",
  "LongfinSquid",
  "WinterFlounder",
  "Monkfish",
  "Scup"
)
speciesnum <- c(
  '077',
  '072',
  '073',
  '121',
  '103',
  '141',
  '301',
  '032',
  '401',
  '503',
  '023',
  '197',
  '143'
)

for (i in seq_along(speciesnames)) {
  # Write script to query the survey data for each species
  catch.qry <- paste(
    "select u.cruise6, ",
    "u.stratum, ",
    "u.tow, ",
    "u.station, ",
    "u.svspp, ",
    "u.logged_species_name, ",
    "u.catchsex, ",
    "to_char(round(u.expcatchwt,3),'99990.000') as catch_wt, ",
    "u.expcatchnum ",
    "from svdbs.union_fscs_svcat u, svdbs.svdbs_cruises c ",
    "where u.cruise6 = c.cruise6 and ",
    "c.Season != 'SUMMER' and ",
    "c.purpose_code = 10 and ",
    "c.STATUS_CODE=10 and ",
    "u.svspp = '",
    speciesnum[i],
    "' ",
    "order by u.cruise6, u.stratum, u.tow, u.station, u.svspp, u.catchsex",
    sep = ""
  )

  source("Oracle_User_Data.R")
  catch.data <- fetch(dbSendQuery(db1, catch.qry))
  source("Disconnect_Oracle.R")
  # catch.data

  dim(catch.data)
  catch.data <- rename(
    catch.data,
    Cruise = CRUISE6,
    Stratum = STRATUM,
    Tow = TOW,
    Station = STATION,
    Sex = CATCHSEX,
    Tot.kg = CATCH_WT,
    Tot.N = EXPCATCHNUM
  )
  dim(catch.data)

  # Select only those catches from stations that meet SHG requirement
  NEFSC.tow <- left_join(stn.data, catch.data) |>
    # Set NAs in Tot.N and Tot.kg to 0
    mutate(
      Tot.N = ifelse(is.na(Tot.N), 0, Tot.N),
      Tot.kg = ifelse(is.na(Tot.kg), 0, Tot.kg),
    )
  NEFSC.tow.df <- data.frame(NEFSC.tow)

  write.csv(
    NEFSC.tow,
    file = paste("NEFSCBottomTrawl", speciesnames[i], ".csv", sep = "")
  )
}


load('RUN2.rdat')
