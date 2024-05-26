# VAST attempt 2 univariate model selection as a script
# modified from https://github.com/James-Thorson-NOAA/VAST/wiki/Index-standardization

# Load packages
library(here)
library(dplyr)
library(VAST)

#Read in data, separate spring and fall, and rename columns for VAST:

# this dataset created in SSTmethods.Rmd

megabenagg_stn <- readRDS(here::here("fhdata/megabenagg_stn_all_modBT.rds"))

macrobenagg_stn <- readRDS(here::here("fhdata/macrobenagg_stn_all_modBT.rds"))

# make SST column that uses surftemp unless missing or 0
# there are 3 surftemp 0 values in the dataset, all with oisst > 15
megabenagg_stn <- megabenagg_stn %>%
  dplyr::mutate(btfill = ifelse((is.na(bottemp)|bottemp==0), mod_bt, bottemp))

macrobenagg_stn <- macrobenagg_stn %>%
  dplyr::mutate(btfill = ifelse((is.na(bottemp)|bottemp==0), mod_bt, bottemp))

# code Vessel as AL=0, HB=1, NEAMAP=2

megabenagg_stn_fall <- megabenagg_stn %>%
  #ungroup() %>%
  filter(season_ng == "FALL", # Annual model for MRIP index
    year > 1979) %>%
  mutate(AreaSwept_km2 = 1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
  ) %>% 
  dplyr::select(Catch_g = meanmegabenpywt, #use megabenwt for individuals input in example
                Year = year,
                Vessel,
                AreaSwept_km2,
                Lat = declat,
                Lon = declon,
                meanbenthivorelen,
                nbenthivoresp,
                #bottemp, #this leaves out many stations for NEFSC
                #surftemp, #this leaves out many stations for NEFSC
                mod_bt,#leaves out 2023 NEAMAP, ok for now
                btfill
  ) %>%
  na.omit() %>%
  as.data.frame()

megabenagg_stn_spring <- megabenagg_stn %>%
  #ungroup() %>%
  filter(season_ng == "SPRING", # Annual model for MRIP index
    year > 1979) %>%
  mutate(AreaSwept_km2 = 1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
  ) %>% 
  dplyr::select(Catch_g = meanmegabenpywt, #use megabenwt for individuals input in example
                Year = year,
                Vessel,
                AreaSwept_km2,
                Lat = declat,
                Lon = declon,
                meanbenthivorelen,
                nbenthivoresp,
                #bottemp, #this leaves out many stations for NEFSC
                #surftemp, #this leaves out many stations for NEFSC
                mod_bt,#leaves out 2023 NEAMAP, ok for now
                btfill
  ) %>%
  na.omit() %>%
  as.data.frame()

macrobenagg_stn_fall <- macrobenagg_stn %>%
  #ungroup() %>%
  filter(season_ng == "FALL", # Annual model for MRIP index
         year > 1979) %>%
  mutate(AreaSwept_km2 = 1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
  ) %>% 
  dplyr::select(Catch_g = meanmacrobenpywt, #use megabenwt for individuals input in example
                Year = year,
                Vessel,
                AreaSwept_km2,
                Lat = declat,
                Lon = declon,
                meanbenthivorelen,
                nbenthivoresp,
                #bottemp, #this leaves out many stations for NEFSC
                #surftemp, #this leaves out many stations for NEFSC
                mod_bt,#leaves out 2023 NEAMAP, ok for now
                btfill
  ) %>%
  na.omit() %>%
  as.data.frame()

macrobenagg_stn_spring <- macrobenagg_stn %>%
  #ungroup() %>%
  filter(season_ng == "SPRING", # Annual model for MRIP index
         year > 1979) %>%
  mutate(AreaSwept_km2 = 1, #Elizabeth's code
         #declon = -declon already done before neamap merge
         Vessel = as.numeric(as.factor(vessel))-1
  ) %>% 
  dplyr::select(Catch_g = meanmacrobenpywt, #use megabenwt for individuals input in example
                Year = year,
                Vessel,
                AreaSwept_km2,
                Lat = declat,
                Lon = declon,
                meanbenthivorelen,
                nbenthivoresp,
                #bottemp, #this leaves out many stations for NEFSC
                #surftemp, #this leaves out many stations for NEFSC
                mod_bt,#leaves out 2023 NEAMAP, ok for now
                btfill
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

MAB2 <- FishStatsUtils::northwest_atlantic_grid %>% 
  dplyr::filter(stratum_number %in% MAB) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# Georges Bank EPU
GB2 <- FishStatsUtils::northwest_atlantic_grid %>% 
  dplyr::filter(stratum_number %in% GB) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# gulf of maine EPU (for SOE)
GOM2 <- FishStatsUtils::northwest_atlantic_grid %>% 
  dplyr::filter(stratum_number %in% GOM) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# scotian shelf EPU (for SOE)
SS2 <- FishStatsUtils::northwest_atlantic_grid %>%  
  dplyr::filter(stratum_number %in% SS) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# needed to cover the whole northwest atlantic grid--lets try without
# allother2 <- coast3nmbuffst %>%
#   dplyr::filter(!stratum_number %in% c(MAB, GB, GOM, SS)) %>%
#   dplyr::select(stratum_number2) %>%
#   dplyr::distinct()

# all epus
allEPU2 <- FishStatsUtils::northwest_atlantic_grid %>%  
  dplyr::filter(stratum_number %in% c(MAB, GB, GOM, SS)) %>%
  dplyr::select(stratum_number) %>%
  dplyr::distinct()

# Model selection 1 (spatial, spatio-temporal effects, no covariates) options and naming:
# Use_REML = TRUE in fit_model
# Season_knots + suffix below
# _alleffectson             FieldConfig default (all IID)
# _noaniso                  FieldConfig default (all IID) and use_anistropy = FALSE
# _noomeps2                 FieldConfig 0 for Omega2, Epsilon2
# _noomeps2_noaniso         FieldConfig 0 for Omega2, Epsilon2 and use_anistropy = FALSE
# _noomeps2_noeps1          FieldConfig 0 for Omega2, Epsilon2, Epsilon1
# _noomeps2_noeps1_noaniso  FieldConfig 0 for Omega2, Epsilon2, Epsilon1 and use_anistropy = FALSE
# _noomeps12                FieldConfig both Omega, Epsilon 0
# _noomeps12_noaniso        FieldConfig both Omega, Epsilon 0 and use_anistropy = FALSE

# default configs
FieldConfig = matrix( "IID", ncol=2, nrow=3, 
                      dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))

RhoConfig <- c(
  "Beta1" = 0,      # temporal structure on years (intercepts) 
  "Beta2" = 0, 
  "Epsilon1" = 0,   # temporal structure on spatio-temporal variation
  "Epsilon2" = 0
) 
# not testing alternative RhoConfigs here just noted for completeness
# 0 off (fixed effects)
# 1 independent
# 2 random walk
# 3 constant among years (fixed effect)
# 4 AR1

use_anisotropy <- TRUE


# Model selection 2 (covariates) options, FieldConfig default (all IID), no REML
# Season_knots + suffix below 
# _base         No vessel overdispersion or length/number covariates  (ensure same dataset)  
# _len          Predator mean length covariate
# _num          Number of predator species covariate
# _lennum       Predator mean length and number of predator species covariates
# _sst          Combined in situ and OISST covariate
# _lensst       Predator mean length and SST covariates
# _numsst       Number of predator species and SST covariates
# _lennumsst    Predator mean length, number of predator species, and SST covariates
# _eta10        Overdispersion (vessel effect) in first linear predictor (prey encounter)
# _eta11        Overdispersion (vessel effect) in both linear predictors (prey wt)


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
                           "SS" = SS2
                           #"bfall" = bfall2,
                           #"bfin" = bfinshore2,
                           #"bfoff" = bfoffshore2,
                           #"MABGBalbinshore" = albinshore2,
                           #"MABGBothoffshore" = MABGBothoffshore2,
                           #"allother" = allother2
                           ))


# list of data, settings, and directory for output for each option

mod.season <- c("megaben_fall_500", "megaben_spring_500",
                "macroben_fall_500", "macroben_spring_500") #includes n knots

mod.dat <- list(megabenagg_stn_fall, megabenagg_stn_spring,
                macrobenagg_stn_fall, macrobenagg_stn_spring)

names(mod.dat) <- mod.season

mod.config <- c("alleffectson", "noaniso", 
                "noomeps2", "noomeps2_noaniso", 
                "noomeps2_noeps1", "noomeps2_noeps1_noaniso",
                "noomeps12", "noomeps12_noaniso")

FieldConfig1 <- matrix( "IID", ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))
FieldConfig2 <- matrix( c("IID","IID","IID",0,0,"IID"), ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))
FieldConfig3 <- matrix( c("IID",0,"IID",0,0,"IID"), ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))
FieldConfig4 <- matrix( c(0,0,"IID",0,0,"IID"), ncol=2, nrow=3, 
                        dimnames=list(c("Omega","Epsilon","Beta"),c("Component_1","Component_2")))

mod.FieldConfig <- list(FieldConfig1, FieldConfig1,
                        FieldConfig2, FieldConfig2,
                        FieldConfig3, FieldConfig3,
                        FieldConfig4, FieldConfig4)

names(mod.FieldConfig) <- mod.config

mod.use_anistropy <- list(TRUE, FALSE, 
                          TRUE, FALSE,
                          TRUE, FALSE,
                          TRUE, FALSE)

names(mod.use_anistropy) <- mod.config

#########################################################
# Run model selection 1

for(season in mod.season){
  
  season <- season 
  
  dat <- mod.dat[[season]]
  
  for(config in mod.config) {
    
    name <- paste0(season,"_", config)
    
    working_dir <- here::here(sprintf("pyindex_modsel1/%s/", name))
    
    if(!dir.exists(working_dir)) {
      dir.create(working_dir)
    }
    
    FieldConfig <- mod.FieldConfig[[config]]
    use_anisotropy <- mod.use_anistropy[[config]]
    
    settings <- make_settings( n_x = 500, 
                               Region = "northwest_atlantic",
                               Version = "VAST_v14_0_1", #needed to prevent error from newer dev version number
                               #strata.limits = list('All_areas' = 1:1e5), full area
                               strata.limits = strata.limits,
                               purpose = "index2", 
                               bias.correct = FALSE,
                               use_anisotropy = use_anisotropy,
                               FieldConfig = FieldConfig,
                               RhoConfig = RhoConfig, #always default
                               OverdispersionConfig = OverdispersionConfig
    )
    
    
    fit <- fit_model(
      settings = settings, 
      #extrapolation_list = New_Extrapolation_List,
      Lat_i = dat$Lat, 
      Lon_i = dat$Lon, 
      t_i = dat$Year, 
      b_i = as_units(dat[,'Catch_g'], 'g'),
      a_i = rep(1, nrow(dat)),
      v_i = dat$Vessel,
      #Q_ik = as.matrix(dat[,c("npiscsp", 
      #                                       "meanpisclen", 
      #                                       "sstfill"
      #                                      )]),
      Use_REML = TRUE,
      working_dir = paste0(working_dir, "/"))
    
    #saveRDS(fit, file = paste0(working_dir, "/fit.rds"))
    
    # Plot results
    plot( fit,
          working_dir = paste0(working_dir, "/"))
    
  } # end config loop
} # end season loop

#########################################################
# Define covariate combinations

mod.covar <- c("base", "len", 
               "num", "lennum", 
               "bt", "lenbt", 
               "numbt", "lennumbt",
               "eta10", "eta11")

OverdispersionConfig	<- c("eta1"=0, "eta2"=0)
# eta1 = vessel effects on prey encounter rate
# eta2 = vessel effects on prey weight

OverdispersionConfig1 <- c("eta1"=1, "eta2"=0)
OverdispersionConfig2 <- c("eta1"=1, "eta2"=1)

mod.eta <- list(OverdispersionConfig, OverdispersionConfig, 
                OverdispersionConfig, OverdispersionConfig, 
                OverdispersionConfig, OverdispersionConfig, 
                OverdispersionConfig, OverdispersionConfig,
                OverdispersionConfig1, OverdispersionConfig2)

names(mod.eta) <- mod.covar

#########################################################
# Run model selection 2

for(season in mod.season){
  
  season <- season # c("annual_500_lennosst_ALLsplit")
  
  dat <- mod.dat[[season]]
  
  Q_ikbase  <-  NULL
  Q_iklen <- as.matrix(dat[,c("meanbenthivorelen")])
  Q_iknum <- as.matrix(dat[,c("nbenthivoresp")])
  Q_iklennum <- as.matrix(dat[,c("meanbenthivorelen", "nbenthivoresp")])
  Q_ikbt <- as.matrix(dat[,c("btfill")])
  Q_iklenbt <- as.matrix(dat[,c("meanbenthivorelen", "btfill")])
  Q_iknumbt <- as.matrix(dat[,c("nbenthivoresp", "btfill")])
  Q_iklennumbt <- as.matrix(dat[,c("meanbenthivorelen", "nbenthivoresp", "btfill")])
  
  mod.Qik <- list(Q_ikbase, Q_iklen, 
                  Q_iknum, Q_iklennum,
                  Q_ikbt, Q_iklenbt, 
                  Q_iknumbt, Q_iklennumbt,
                  Q_ikbase, Q_ikbase)
  
  names(mod.Qik) <- mod.covar
  
  for(covar in mod.covar) {
    
    name <- paste0(season,"_", covar)
    
    working_dir <- here::here(sprintf("pyindex_modsel2/%s/", name))
    
    if(!dir.exists(working_dir)) {
      dir.create(working_dir)
    }
    
    # winners of model selection 1
    use_anisotropy <- TRUE
    FieldConfig <- FieldConfig1
     
    OverdispersionConfig <- mod.eta[[covar]]
    Q_ik <- mod.Qik[[covar]]
    
    settings <- make_settings( n_x = 500, 
                               Region = "northwest_atlantic",
                               Version = "VAST_v14_0_1", #needed to prevent error from newer dev version number
                               #strata.limits = list('All_areas' = 1:1e5), full area
                               strata.limits = strata.limits,
                               purpose = "index2", 
                               bias.correct = FALSE,
                               use_anisotropy = use_anisotropy,
                               FieldConfig = FieldConfig,
                               RhoConfig = RhoConfig, #always default
                               OverdispersionConfig = OverdispersionConfig
    )
    
    
    fit <- fit_model(
      settings = settings, 
      #extrapolation_list = New_Extrapolation_List,
      Lat_i = dat$Lat, 
      Lon_i = dat$Lon, 
      t_i = dat$Year, 
      b_i = as_units(dat[,'Catch_g'], 'g'),
      a_i = rep(1, nrow(dat)),
      v_i = dat$Vessel,
      Q_ik = Q_ik,
      #Use_REML = TRUE,
      working_dir = paste0(working_dir, "/"))
    
    #saveRDS(fit, file = paste0(working_dir, "/fit.rds"))
    
    # Plot results
    plot( fit,
          working_dir = paste0(working_dir, "/"))
    
  } # end covar loop
} # end season loop
