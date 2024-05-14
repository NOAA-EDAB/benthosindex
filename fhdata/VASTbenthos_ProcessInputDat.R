# Streamlined version of CreateVASTInputs.Rmd for operational updates to forage index indicators
# Modified for benthos index
# May 2024
#   This one is updating with 2022 NEFSC and NEAMAP data and OISST
#   To be used in the 2024 State of the Ecosystem report

library(tidyverse)
library(here)
library(dendextend)

# Load NEFSC stomach data received from Brian Smith

# object is called `allfh`
load(url("https://github.com/NOAA-EDAB/forageindex/raw/main/fhdat/allfh.Rdata"))

#object is called allfh21
load(url("https://github.com/NOAA-EDAB/forageindex/raw/main/fhdat/allfh21.RData"))

#object is called allfh22
load(url("https://github.com/NOAA-EDAB/forageindex/raw/main/fhdat/allfh22.RData"))

# bind all NEFSC stomach datasets
allfh <- allfh %>%
  dplyr::bind_rows(allfh21) |>
  dplyr::bind_rows(allfh22)

# can we deload the 21 and 22 datasets?
rm("allfh21", "allfh22")

###############################################################################
# read predator similarity info to generate predator list
# Input NEFSC food habits overlap matrix:

dietoverlap <- read_csv(here("data-raw/tgmat.2022-02-15.csv"))

# use dendextend functions to get list
d_dietoverlap <- dist(dietoverlap)

guilds <- hclust(d_dietoverlap)

#plot(guilds)

dend <- as.dendrogram(guilds)

dend <- rotate(dend, 1:136)

dend <- color_branches(dend, k=6) # Brian uses 6 categories

labels(dend) <- paste(as.character(names(dietoverlap[-1]))[order.dendrogram(dend)],
                      "(",labels(dend),")", 
                      sep = "")

dend <- hang.dendrogram(dend,hang_height=0.1)

pisc <- partition_leaves(dend)[[
  which_node(dend, c("Bluefish..S(37)", "Bluefish..M(36)", "Bluefish..L(35)"))
]]

piscdf <- data.frame("COMNAME" = toupper(str_remove(pisc, "\\..*")),
                     "SizeCat" = str_remove(str_extract(pisc, "\\..*[:upper:]+"), "\\.."))


plank <- partition_leaves(dend)[[
  which_node(dend, c("Atlantic herring..S(20)", "Atlantic mackerel..S(23)", "Blueback herring..XS(34)"))
]]

plankdf <- data.frame("COMNAME" = toupper(str_remove(plank, "\\..*")),
                      "SizeCat" = str_remove(str_extract(plank, "\\..*[:upper:]+"), "\\.."))

all <- partition_leaves(dend)[[
  which_node(dend, c("Barndoor skate..S(26)", "Bluefish..L(35)"))
]]

alldf <- data.frame("COMNAME" = toupper(str_remove(all, "\\..*")),
                    "SizeCat" = str_remove(str_extract(all, "\\..*[:upper:]+"), "\\.."))

sizedef <- readxl::read_excel(here::here("data-raw/Table3.xlsx"),
                              range = "B3:H55") |>  # table of NEFSC FH size categories from Brian
  dplyr::rename(XS = `Extra-Small`,
                S = "Small",
                M = "Medium",
                L = "Large",
                XL = `Extra-Large`,
                CommonName = `Common Name`,
                SpeciesName = `Species Name`) |>
  tidyr::pivot_longer(-c(1:2), names_to = "sizecat", values_to = "range") |>
  dplyr::filter(!range=="-") |>
  dplyr::mutate(COMNAME = toupper(CommonName)) 


# add missing by hand from word Table 1.docx 
# (pollock was misspelled, fixed in spreadsheet)
# (removed "flounder" from Windowpane in spreadsheet)
# asked Brian for these; response:
# For these predators and most likely any others not listed in TM216, 
# the default is S (<=20 cm), M (>20 and <=50 cm), and L (>50 cm).
# Northern kingfish M
# Buckler dory S
# Fourbeard rockling S
# Fourbeard rockling M
# Cunner M

extra <- data.frame(CommonName = c("Northern kingfish",
                                   "Buckler dory",
                                   "Fourbeard rockling",
                                   "Fourbeard rockling",
                                   "Cunner"),  
                    sizecat = c("M",
                                "S",
                                "S",
                                "M",
                                "M"), 
                    range = c("21-50",
                              "≤20",
                              "≤20",
                              "21-50",
                              "21-50"))  |>
  dplyr::mutate(COMNAME = toupper(CommonName)) 

sizedef <- bind_rows(sizedef, extra)

# dplyr::mutate(COMNAME = toupper(`Common Name`),
#               XS = readr::parse_number(`Extra-Small`),
#               S = readr::parse_number(Small),
#               M = readr::parse_number(Medium),
#               L = readr::parse_number(Large),
#               XL = readr::parse_number(`Extra-Large`))


# we'll call benthivores everything but piscivores and planktivores
benthivores <- alldf |>
  dplyr::anti_join(dplyr::bind_rows(piscdf, plankdf)) |>
  dplyr::left_join(sizedef, join_by(COMNAME == COMNAME, SizeCat == sizecat)) |>
  dplyr::left_join(ecodata::species_groupings, by = "COMNAME") |>
  dplyr::select(COMNAME, ITISSPP, SCINAME, SizeCat = SizeCat.x, SizeRange_cm = range) |>
  dplyr::distinct()

fh.nefsc.benthivore.complete <- allfh %>%
  #filter(pynam != "EMPTY") %>%
  left_join(benthivores, by = c("pdcomnam" = "COMNAME",
                                   "sizecat" = "SizeCat")) 


##############################################################################
# Get prey list from Rpath megabenthos and macrobenthos categories

# Rpath prey list, object is called prey
load(here("data/prey.RData")) #original mappings from June 2023

# make adjustments to list based on modeling group discussions
addtomacro <- prey |>
  dplyr::filter(RPATH == "Megabenthos") |> 
  dplyr::filter(PYNAM %in% c("ALBUNEA PARETII", "EMERITA TALPOIDA", "RANILIA MURICATA", "HIPPIDAE"))

megaben <- prey |>
  dplyr::filter(RPATH == "Megabenthos") |>
  dplyr::filter(!PYNAM %in% c("ALBUNEA PARETII", "EMERITA TALPOIDA", "RANILIA MURICATA", "HIPPIDAE"))

macroben <- prey |>
  dplyr::filter(RPATH == "Macrobenthos") |> 
  dplyr::filter(!PYNAM %in% c("PECTINIDAE", "PECTINIDAE SHELL", "PECTINIDAE VISCERA")) |>
  dplyr::bind_rows(addtomacro)

macrobenfh <- allfh %>%
  dplyr::left_join(macroben %>% 
                     dplyr::select(PYNAM, RPATH) %>%
                     setNames(., tolower(names(.)))) %>%
  dplyr::filter(!is.na(rpath))

megabenfh <- allfh %>%
  dplyr::left_join(megaben %>% 
                     dplyr::select(PYNAM, RPATH) %>%
                     setNames(., tolower(names(.)))) %>%
  dplyr::filter(!is.na(rpath))

macrobenITIS <- macroben |>
  dplyr::select(PYNAM, PYCOMNAM, PYSPP) |>
  dplyr::left_join(ecodata::species_groupings, join_by(PYNAM == SCINAME)) |>
  dplyr::select(PYNAM, PYCOMNAM, COMNAME, PYSPP, ITISSPP)

megabenITIS <- megaben |>
  dplyr::select(PYNAM, PYCOMNAM, PYSPP) |>
  dplyr::left_join(ecodata::species_groupings, join_by(PYNAM == SCINAME)) |>
  dplyr::select(PYNAM, PYCOMNAM, COMNAME, PYSPP, ITISSPP)


fh.nefsc.benthivore.complete.macrobenthos <- fh.nefsc.benthivore.complete %>%
  mutate(macrobenthos = case_when(pynam %in% macrobenITIS$pynam ~ "macroben",
                              TRUE ~ "othprey"))

fh.nefsc.benthivore.complete.megabenthos <- fh.nefsc.benthivore.complete %>%
  mutate(megabenthos = case_when(pynam %in% megabenITIS$pynam ~ "megaben",
                                  TRUE ~ "othprey"))


###############################################################################
# Make the NEFSC dataset aggregating prey based on prey list

macrobenall_stn <- fh.nefsc.benthivore.complete.macrobenthos %>%
  #create id linking cruise6_station
  #create season_ng spring and fall Spring=Jan-May, Fall=June-Dec
  mutate(id = paste0(cruise6, "_", station),
         year = as.numeric(year),
         month = as.numeric(month),
         season_ng = case_when(month <= 6 ~ "SPRING",
                               month >= 7 ~ "FALL",
                               TRUE ~ as.character(NA))
  ) %>%
  dplyr::select(year, season_ng, id, stratum,
                pynam, pyamtw, pywgti, pyvoli, macrobenthos, 
                pdcomnam, pdid, pdlen, pdsvol, pdswgt, 
                beglat, beglon, declat, declon, 
                bottemp, surftemp, setdepth) %>%
  group_by(id) %>%
  #mean macrobenthos g per stomach per tow: sum all macrobenthos g/n stomachs in tow
  mutate(macrobenpywt = case_when(macrobenthos == "macroben" ~ pyamtw,
                              TRUE ~ 0.0),
         macrobenpynam = case_when(macrobenthos == "macroben" ~ pynam,
                               TRUE ~ NA_character_)) 

# Optional: save at prey disaggregated stage for paper
#saveRDS(macrobenall_stn, here("fhdata/macrobenall_stn.rds"))

# Now get station data in one line
stndat <- macrobenall_stn %>%
  dplyr::select(year, season_ng, id, 
                beglat, beglon, declat, declon, 
                bottemp, surftemp, setdepth) %>%
  distinct()

#benthivore stomachs in tow count pdid for each pred and sum
benthivorestom <- macrobenall_stn %>%
  group_by(id, pdcomnam) %>%
  summarise(nstompd = n_distinct(pdid)) %>%
  group_by(id) %>%
  summarise(nstomtot = sum(nstompd))

#mean and var pred length per tow
benthivorelen <-  macrobenall_stn %>%
  summarise(meanpisclen = mean(pdlen),
            varpisclen = var(pdlen))

# Aggregated prey at station level with predator covariates
macrobenagg_stn <- macrobenall_stn %>%
  summarise(summacrobenpywt = sum(macrobenpywt),
            nmacrobenpysp = n_distinct(macrobenpynam, na.rm = T),
            npreysp = n_distinct(pynam),
            npiscsp = n_distinct(pdcomnam)) %>%
  left_join(benthivorestom) %>%
  mutate(meanmacrobenpywt = summacrobenpywt/nstomtot) %>%
  left_join(benthivorelen) %>%
  left_join(stndat)

# save at same stage as before, writing over old file
#saveRDS(bluepyagg_stn, here("fhdat/bluepyagg_stn.rds"))

# current dataset, fix declon, add vessel, rename NEFSC
#nefsc_bluepyagg_stn <- readRDS(here("fhdat/bluepyagg_stn.rds")) %>%
nefsc_macrobenagg_stn <- macrobenagg_stn %>%
  mutate(declon = -declon,
         vessel = case_when(year<2009 ~ "AL",
                            year>=2009 ~ "HB", 
                            TRUE ~ as.character(NA)))

##############################################################################
# Add NEAMAP to make full aggregated stomach dataset

# Read in NEAMAP updated input from Jim Gartland, reformat with same names
neamap_macrobenthos_stn <- read_csv(here("fhdata/NEAMAP_Mean Stomach Weights_Macrobenthos prey_wWQ.csv")) %>%  
  mutate(vessel = "NEAMAP") %>%
  rename(id = station,
         sumbluepywt = sumbluepreywt,
         nbluepysp = nbfpreyspp,
         #npreysp = ,
         npiscsp = npiscspp,
         nstomtot = nstomtot, 
         meanbluepywt = meanbluepreywt,
         meanpisclen = meanpisclen.simple, 
         #varpisclen = ,
         season_ng = season,
         declat  = lat,
         declon = lon,
         bottemp = bWT,
         surftemp = SST, 
         setdepth = depthm) 


# combine NEAMAP and NEFSC
bluepyagg_stn_all <-  nefsc_bluepyagg_stn %>%
  bind_rows(neamap_bluepreyagg_stn) 

# Save before SST integration step
#saveRDS(bluepyagg_stn_all, here("fhdat/bluepyagg_stn_all.rds"))

###############################################################################
# Add SST into NEAMAP and reintegrate into full dataset

# Read back in if needed for SST
#bluepyagg_stn_all <- readRDS(here("fhdat/bluepyagg_stn_all.rds"))

NEFSCstations <- allfh %>%
  dplyr::mutate(id = paste0(cruise6, "_", station),
                year = as.numeric(year),
                month = as.numeric(month),
                day = as.numeric(day),
                declon = -declon) %>%
  dplyr::select(id, year, month, day, declat, declon) %>%
  dplyr::distinct()

# Need NEAMAP SST update! This is the old file
NEAMAPstationSST <- read.csv(here("fhdat/NEAMAP SST_2007_2022.csv"))

NEAMAPstations <- NEAMAPstationSST %>%
  dplyr::mutate(id = station,
                year = as.numeric(year),
                month = as.numeric(month),
                day = as.numeric(day)) %>%
  dplyr::select(id, year, month, day) %>%
  dplyr::distinct()

# remake diethauls
diethauls <- bluepyagg_stn_all %>%
  dplyr::select(id, declat, declon)

NEFSCstations <- dplyr::select(NEFSCstations, c(-declat, -declon))

Allstations <- bind_rows(NEFSCstations, NEAMAPstations)

#station id, lat lon, year month day
diethauls <- left_join(diethauls, Allstations)

#add year month day to diet data
bluepyagg_stn_all <- left_join(bluepyagg_stn_all, diethauls)

# add NEAMAP SST to surftemp field
NEAMAPidSST <- NEAMAPstationSST %>%
  mutate(id = station) %>%
  dplyr::select(id, SST)

bluepyagg_stn_all <- left_join(bluepyagg_stn_all, NEAMAPidSST, by="id") %>%
  mutate(surftemp = coalesce(surftemp, SST)) %>%
  dplyr::select(-SST)

# save merged dataset with day, month, and NEAMAP surftemp, same name
#saveRDS(bluepyagg_stn_all, here("fhdat/bluepyagg_stn_all.rds"))

###############################################################################
#Now match stations to OISST

#make an SST dataframe for 2022! Add to saved sst_data in data-raw/gridded

library(sf)
library(raster)
library(terra)
library(nngeo)

# Bastille function from https://github.com/kimberly-bastille/ecopull/blob/main/R/utils.R

nc_to_raster <- function(nc,
                         varname,
                         extent = c(0, 360, -90, 90),
                         crop = raster::extent(280, 300, 30, 50),
                         show_images = FALSE) {
  
  message("Reading .nc as brick...")
  
  r <- raster::brick(nc, varname = varname)
  
  message("Setting CRS...")
  raster::crs(r) <- "+proj=longlat +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=-77 +x_0=0 +y_0=0 +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0"
  
  # not sure if this is necessary?
  raster::extent(r) <- raster::extent(extent)
  
  if(show_images){
    par(mfrow = c(1,2))
    raster::plot(r, 1, sub = "Full dataset")
  }
  
  message("Cropping data...")
  ne_data <- raster::crop(r, crop)
  #ne_data <- raster::rotate(ne_data) add here for future pulls
  
  if(show_images){
    raster::plot(ne_data, 1, sub = "Cropped dataset")
    par(mfrow = c(1,1))
  }
  
  message("Done!")
  
  return(ne_data)
}

# function to convert to dataframe based on
# https://towardsdatascience.com/transforming-spatial-data-to-tabular-data-in-r-4dab139f311f

raster_to_sstdf <- function(brick,
                            rotate=TRUE){
  
  if(rotate) brick_r <- raster::rotate(brick)
  brick_r <- raster::crop(brick_r, raster::extent(-77,-65,35,45))
  sstdf <- as.data.frame(raster::rasterToPoints(brick_r, spatial = TRUE))
  sstdf <- sstdf %>%
    dplyr::rename(Lon = x,
                  Lat = y) %>%
    tidyr::pivot_longer(cols = starts_with("X"),
                        names_to = c("year", "month", "day"),
                        names_prefix = "X",
                        names_sep = "\\.",
                        values_to = "sst",
    )
  return(sstdf)
}

# pull the OISST data as raster brick, modified from 
# https://github.com/kimberly-bastille/ecopull/blob/main/.github/workflows/pull_satellite_data.yml

varname <- "sst"

# 1985-2021 previously pulled, processed and stored. add 2022.
# add 1981-1984 to extend back in time. No OISST before 1981.
# 1981 is only Sept-Dec so don't use

years <- #1982:1984 # 2022
for(i in years) {
  name <- paste0(i, ".nc")
  dir.create(here::here("data-raw","gridded", "sst_data"), recursive = TRUE)
  filename <- here::here("data-raw","gridded", "sst_data", paste0("test_", i, ".grd"))
  url <- paste0("https://downloads.psl.noaa.gov/Datasets/noaa.oisst.v2.highres/sst.day.mean.", i, ".nc")
  download.file(url, destfile = name)
  
  text <- knitr::knit_expand(text = "test_{{year}} <- nc_to_raster(nc = name, varname = varname)
                                     raster::writeRaster(test_{{year}}, filename = filename, overwrite=TRUE)",
                             year = i)
  print(text)
  try(eval(parse(text = text)))
  unlink(name) # remove nc file to save space
  print(paste("finished",i))
}


# convert raster to dataframe
#years <- 2022
for(i in years) {
  name <- get(paste0("test_",i))
  filename <- here::here("data-raw","gridded", "sst_data", paste0("sst", i, ".rds"))
  text <- knitr::knit_expand(text = "sst{{year}} <- raster_to_sstdf(brick = name)
                                     saveRDS(sst{{year}}, filename)",
                             year = i)
  print(text)
  try(eval(parse(text = text)))
}
#read in diet data with month day fields

#bluepyagg_stn_all <- readRDS(here("fhdat/bluepyagg_stn_all.rds"))

stations <- bluepyagg_stn_all %>%
  dplyr::mutate(day = str_pad(day, 2, pad='0'),
                month = str_pad(month, 2, pad='0'),
                yrmody = as.numeric(paste0(year, month, day))) %>%
  dplyr::select(id, declon, declat, year, yrmody) %>%
  na.omit() %>%
  sf::st_as_sf(coords=c("declon","declat"), crs=4326, remove=FALSE) 



#list of SST dataframes
SSTdfs <- list.files(here("data-raw/gridded/sst_data/"), pattern = "*.rds")

dietstn_OISST <- tibble()


for(df in SSTdfs){
  sstdf <- readRDS(paste0(here("data-raw/gridded/sst_data/", df)))
  
  # keep only bluefish dates in SST year
  stationsyr <- stations %>%
    filter(year == unique(sstdf$year))
  
  # keep only sst days in bluefish dataset
  sstdf_survdays <- sstdf %>%
    dplyr::mutate(yrmody = as.numeric(paste0(year, month, day)) )%>%
    dplyr::filter(yrmody %in% unique(stationsyr$yrmody)) %>%
    dplyr::mutate(year = as.numeric(year),
                  month = as.numeric(month),
                  day = as.numeric(day),
                  declon = Lon,
                  declat = Lat) %>%
    dplyr::select(-Lon, -Lat) %>%
    sf::st_as_sf(coords=c("declon","declat"), crs=4326, remove=FALSE)  
  
  # now join by nearest neighbor and date
  
  #https://stackoverflow.com/questions/71959927/spatial-join-two-data-frames-by-nearest-feature-and-date-in-r      
  
  yrdietOISST <- do.call('rbind', lapply(split(stationsyr, 1:nrow(stationsyr)), function(x) {
    sf::st_join(x, sstdf_survdays[sstdf_survdays$yrmody == unique(x$yrmody),],
                #join = st_nearest_feature
                join = st_nn, k = 1, progress = FALSE
    )
  }))
  
  #   #datatable solution--works but doesnt seem faster?
  #    df1 <- data.table(stationsyr)
  #   
  #  .nearest_samedate <- function(x) {
  #    st_join(st_as_sf(x), sstdf_survdays[sstdf_survdays$yrmody == unique(x$yrmody),], join = st_nearest_feature)
  #  }
  # # 
  #  yrdietOISST <- df1[, .nearest_samedate(.SD), by = list(1:nrow(df1))]
  
  dietstn_OISST <- rbind(dietstn_OISST, yrdietOISST)
  
}

#saveRDS(dietstn_OISST, here("data-raw/dietstn_OISST.rds"))

# Now join with OISST dataset

#bluepyagg_stn_all <- readRDS(here("fhdat/bluepyagg_stn_all.rds"))
#dietstn_OISST <- readRDS(here("data-raw/dietstn_OISST.rds"))


dietstn_OISST_merge <- dietstn_OISST %>%
  dplyr::rename(declon = declon.x,
                declat = declat.x,
                year = year.x,
                oisst = sst) %>%
  dplyr::select(id, oisst) %>%
  sf::st_drop_geometry()

bluepyagg_stn_all_OISST <- left_join(bluepyagg_stn_all, dietstn_OISST_merge)

saveRDS(bluepyagg_stn_all_OISST, here("fhdat/bluepyagg_stn_all_OISST_1982-2022.rds"))
