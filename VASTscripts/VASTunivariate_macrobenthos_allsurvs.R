# VAST attempt 2 univariate model as a script
# modified from https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization

# Load packages
library(here)
library(dplyr)
library(VAST)

#Read in data, separate spring and fall, and rename columns for VAST:

# this dataset created in fhdata/VASTbenthos_ProcessInputDat.R

macrobenagg_stn <- readRDS(here::here("fhdata/macrobenagg_stn_all.rds"))

# make SST column that uses surftemp unless missing or 0
# there are 3 surftemp 0 values in the dataset, all with oisst > 15
#macrobenagg_stn <- macrobenagg_stn %>%
#  dplyr::mutate(sstfill = ifelse((is.na(surftemp)|surftemp==0), oisst, surftemp))

# code Vessel as AL=0, HB=1, NEAMAP=2

macrobenagg_stn_fall <- macrobenagg_stn %>%
  #ungroup() %>%
  filter(season_ng == "FALL",
         year > 1979) %>%
  mutate(AreaSwept_km2 = 1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
         ) %>% 
  dplyr::select(Catch_g = meanmacrobenpywt, #use bluepywt for individuals input in example
         Year = year,
         Vessel,
         AreaSwept_km2,
         Lat = declat,
         Lon = declon,
         meanbenthivorelen,
         nbenthivoresp #,
         #bottemp, #this leaves out many stations for NEFSC
         #surftemp, #this leaves out many stations for NEFSC
         #oisst,
         #sstfill
         ) %>%
  na.omit() %>%
  as.data.frame()

macrobenagg_stn_spring <- macrobenagg_stn %>%
  filter(season_ng == "SPRING",
         year > 1979) %>%
  mutate(AreaSwept_km2 =1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
         ) %>% 
  dplyr::select(Catch_g = meanmacrobenpywt,
         Year = year,
         Vessel,
         AreaSwept_km2,
         Lat = declat,
         Lon = declon,
         meanbenthivorelen,
         nbenthivoresp #,
         #bottemp, #this leaves out many stations for NEFSC
         #surftemp, #this leaves out many stations for NEFSC
         #oisst,
         #sstfill
         ) %>%
  na.omit() %>%
  as.data.frame()


# Make settings (turning off bias.correct to save time for example)
# NEFSC strata limits https://github.com/James-Thorson-NOAA/VAST/issues/302

# use only MAB, GB, GOM, SS EPUs 
# leave out south of Cape Hatteras at Elizabeth's suggestion
# could also leave out SS?
# CHECK if these EPUs match what we use in SOE

MAB <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
GB  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
GOM <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)
SS  <- c(1300:1352, 3840:3990)

MABGBinshore <- c(3010:3450, 3460, 3470, 3480, 3490, 3500, 3510, 3520:3550)

MABGBoffshore <- c(1010:1080, 1090, 1100:1120,1130:1210, 1230, 1250, 1600:1750)

coast3nmbuffst <- readRDS(here::here("spatialdat/coast3nmbuffst.rds"))

MAB2 <- coast3nmbuffst %>% 
  dplyr::filter(stratum_number %in% MAB) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# # MAB state waters
# MAB2state <- MAB2 %>%
#   dplyr::filter(stratum_number2 %% 10 == 1) 
# 
# # MAB federal waters
# MAB2fed <- MAB2 %>%
#   dplyr::filter(stratum_number2 %% 10 == 2) 

# Georges Bank EPU
GB2 <- coast3nmbuffst %>% 
  dplyr::filter(stratum_number %in% GB) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# # GB state waters
# GB2state <- GB2 %>%
#   dplyr::filter(stratum_number2 %% 10 == 1) 
# 
# #GB federal waters
# GB2fed <- GB2 %>%
#   dplyr::filter(stratum_number2 %% 10 == 2) 

# # whole bluefish domain MABG
# MABGB2 <- dplyr::bind_rows(MAB2, GB2)
# 
# # MABGB state waters
# MABGBstate <- dplyr::bind_rows(MAB2state, GB2state)
# 
# # MABGB federal waters
# MABGBfed <- dplyr::bind_rows(MAB2fed, GB2fed)

# gulf of maine EPU (for SOE)
GOM2 <- coast3nmbuffst %>%
  dplyr::filter(stratum_number %in% GOM) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# scotian shelf EPU (for SOE)
SS2 <- coast3nmbuffst %>%
  dplyr::filter(stratum_number %in% SS) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# needed to cover the whole northwest atlantic grid--lets try without
# allother2 <- coast3nmbuffst %>%
#   dplyr::filter(!stratum_number %in% c(MAB, GB, GOM, SS)) %>%
#   dplyr::select(stratum_number2) %>%
#   dplyr::distinct()

# all epus
allEPU2 <- coast3nmbuffst %>%
  dplyr::filter(stratum_number %in% c(MAB, GB, GOM, SS)) %>%
  dplyr::select(stratum_number2) %>%
  dplyr::distinct()

# configs
FieldConfig <- c(
  "Omega1"   = 0,   # number of spatial variation factors (0, 1, AR1)
  "Epsilon1" = 0,   # number of spatio-temporal factors
  "Omega2"   = 0, 
  "Epsilon2" = 0
) 

# Model selection options, FieldConfig default (all IID)
# Season_knots + suffix below 
# _base         No vessel overdispersion or length/number covariates  (ensure same dataset)  
# _len          Predator mean length covariate
# _no           Number of predator species covariate
# _lenno        Predator mean length and number of predator species covariates
# _eta10        Overdispersion (vessel effect) in first linear predictor (prey encounter)
# _eta11        Overdispersion (vessel effect) in both linear predictors (prey wt)

RhoConfig <- c(
  "Beta1" = 0,      # temporal structure on years (intercepts) 
  "Beta2" = 0, 
  "Epsilon1" = 0,   # temporal structure on spatio-temporal variation
  "Epsilon2" = 0
) 
# 0 off (fixed effects)
# 1 independent
# 2 random walk
# 3 constant among years (fixed effect)
# 4 AR1

OverdispersionConfig	<- c("eta1"=0, "eta2"=0)
# eta1 = vessel effects on prey encounter rate
# eta2 = vessel effects on prey weight

strata.limits <- as.list(c("AllEPU" = allEPU2, 
                           #"MABGB" = MABGB2,
                           #"MABGBstate" = MABGBstate,
                           #"MABGBfed" = MABGBfed,
                           "MAB" = MAB2,
                           "GB" = GB2,
                           "GOM" = GOM2,
                           "SS" = SS2 #,
                           #"bfall" = bfall2,
                           #"bfin" = bfinshore2,
                           #"bfoff" = bfoffshore2,
                           #"MABGBalbinshore" = albinshore2,
                           #"MABGBothoffshore" = MABGBothoffshore2,
                           #"albbfin" = albbfinshore,
                           #"albbfall" = albbfall,
                           #"allother" = allother2)
                           ))

settings = make_settings( n_x = 500, 
                          Region = "northwest_atlantic",
                          Version = "VAST_v14_0_1", #needed to prevent error from newer dev version number
                          #strata.limits = list('All_areas' = 1:1e5), full area
                          strata.limits = strata.limits,
                          purpose = "index2", 
                          #bias.correct = TRUE,
                          #use_anisotropy = FALSE,
                          #fine_scale = FALSE,
                          #FieldConfig = FieldConfig,
                          #RhoConfig = RhoConfig,
                          OverdispersionConfig = OverdispersionConfig
                          )


New_Extrapolation_List <- readRDS(here::here("spatialdat/CustomExtrapolationList.rds"))

# select dataset and set directory for output

#########################################################
# Run model fall

season <- c("fall_500_test")

working_dir <- here::here(sprintf("pyindex/macrobenthos_%s/", season))

if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}

fit <- fit_model(
  settings = settings, 
  extrapolation_list = New_Extrapolation_List,
  Lat_i = macrobenagg_stn_fall$Lat, 
  Lon_i = macrobenagg_stn_fall$Lon, 
  t_i = macrobenagg_stn_fall$Year, 
  b_i = as_units(macrobenagg_stn_fall[,'Catch_g'], 'g'),
  a_i = rep(1, nrow(macrobenagg_stn_fall)),
  v_i = macrobenagg_stn_fall$Vessel,
  #Q_ik = as.matrix(bluepyagg_stn_fall[,c("npiscsp", 
  #                                       "meanpisclen", 
  #                                       "sstfill"
  #                                       )]),
  #Use_REML = TRUE,
  working_dir = paste0(working_dir, "/"))

saveRDS(fit, file = paste0(working_dir, "/fit.rds"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/"))

######################################################
# Run model spring

season <- c("spring_500_test")

working_dir <- here::here(sprintf("pyindex/macrobenthos_%s/", season))

if(!dir.exists(working_dir)) {
  dir.create(working_dir)
}                         
  
fit <- fit_model( settings = settings,  
                 extrapolation_list = New_Extrapolation_List,
                 Lat_i = macrobenagg_stn_spring[,'Lat'], 
                 Lon_i = macrobenagg_stn_spring[,'Lon'], 
                 t_i = macrobenagg_stn_spring[,'Year'], 
                 b_i = as_units(macrobenagg_stn_spring[,'Catch_g'], 'g'), 
                 a_i = rep(1, nrow(macrobenagg_stn_spring)),
                 v_i = macrobenagg_stn_spring$Vessel,
                 #Q_ik = as.matrix(bluepyagg_stn_spring[,c("npiscsp", 
                 #                                         "meanpisclen", 
                 #                                         "sstfill"
                 #                                         )]),
                # Use_REML = TRUE,
                 working_dir = paste0(working_dir, "/"))

saveRDS(fit, file = paste0(working_dir, "/fit.rds"))

# Plot results
plot( fit,
      working_dir = paste0(working_dir, "/")) 