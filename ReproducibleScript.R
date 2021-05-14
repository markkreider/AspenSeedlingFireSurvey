# Reproducible script for:  

# Title: "Low-density aspen seedling establishment is widespread following recent wildfires in the western U.S."
# Journal: Ecology
# Paper Authors: Mark R. Kreider (corresponding author), Larissa L. Yocom
# Author affiliations: Department of Wildland Resources and Ecology Center, Utah State University, Logan, UT 84321

# Script Author: Mark R. Kreider
# Script contact: mark.kreider@umontana.edu

# load packages
library(dplyr)

# Summary statistics -------
plot_values <- readRDS("Data/plot_values") # load plot data

# number of plots with seedling regeneration
plot_values %>% 
  summarise(n = sum(occ_aspenSeedling), # number of plots with seedling regeneration
            perc = mean(occ_aspenSeedling)) # percent of plots with seedling regeneration


# amount of occupied (aspen seedling) plots with no pre-fire aspen
plot_values %>% 
  filter(occ_aspenSeedling == 1) %>% # filter to only occupied plots
  
  # create variable of pre-fire aspen
  mutate(prefire_aspen = ifelse(occ_aspenSucker + aspen_overstory > 0, # if plot had either suckers or aspen in overstory...
                                 1, # pre-fire aspen was present
                                 0)) %>% # otherwise, no pre-fire aspen
  summarise(n = n() - sum(prefire_aspen), # sum of occupied plots with no pre-fire aspen
            perc = n / n()) # percent of occupied plots with no pre-fire aspen



# median distance of occupied plots to aspen seed source
plot_values %>% 
  filter(occ_aspenSeedling == 1) %>% # filter to only occupied plots
  summarise(median_distance = median(aspenDist))

# occupied plots >= 1000 meters from aspen seed source
plot_values %>% 
  filter(occ_aspenSeedling == 1) %>% # filter to only occupied plots
  filter(aspenDist >= 1000) %>% 
  nrow

# number of fires with sucker regeneration
plot_values %>% 
  group_by(fire) %>%
  summarise(n_sucker_occ = max(occ_aspenSucker)) %>% # find which fires had sucker regeneration
  summarise(n = sum(n_sucker_occ), # number of fires with sucker regeneration
            perc = n / n()) # percent of fires with sucker regeneration

# number of plots with sucker regeneration
plot_values %>% 
  summarise(n = sum(occ_aspenSucker), # number of plots with sucker regeneration
            perc = mean(occ_aspenSucker)) # percent of plots with sucker regeneration



# TABLE 1 ------- 
table1_groupedByFire <- 
  plot_values %>% 
  group_by(fire) %>% # group summary by fire footprints
  summarise(seedlingOcc = max(occ_aspenSeedling), # whether a fire had aspen seedling establishment (also exception below)
            allPlots_n = n(), # total number of plots
            occPlots_n = sum(occ_aspenSeedling), # number of plots with aspen seedling establishment
            occPlots_perc = mean(occ_aspenSeedling) %>% round(2), # percent of plots with aspen seedling establishment
            dens_median = (aspen_dens[which(aspen_dens > 0)] %>% median) * (10000 / 100),
            dens_mean = (aspen_dens[which(aspen_dens > 0)] %>% mean) * (10000 / 100),
            dens_max = (aspen_dens[which(aspen_dens > 0)] %>% max) * (10000 / 100),
            elev_min = min(as.integer(elev)),
            elev_max = max(as.integer(elev)),
            elev_range = elev_max - elev_min,
            precip_min = min(as.integer(precip)),
            precip_max = max(as.integer(precip)),
            temp_min = min(temp),
            temp_max = max(temp),
            precip_z_2019 = mean(precip_z_2019) %>% round(2),
            temp_z_2019 = mean(temp_z_2019) %>% round(2)) %>%
  as.data.frame()

# In two fires, we found seedlings within the fire area, however not in any plots
table1_groupedByFire$seedlingOcc[table1_groupedByFire$fire == "West Valley"] <- 1  # seedlings were found in West Valley outside of plots
table1_groupedByFire$seedlingOcc[table1_groupedByFire$fire == "Bald Mountain"] <- 1 # seedlings were found in Bald Mountain outside of plots

table1_overall_summary <- 
  table1_groupedByFire %>%
  summarise(fire = "OVERALL SUMMARY",
            seedlingOcc = mean(seedlingOcc),
            allPlots_n = sum(allPlots_n),
            occPlots_n = sum(occPlots_n),
            occPlots_perc = (occPlots_n / allPlots_n) %>% round(2),
            elev_min = min(elev_min),
            elev_max = max(elev_max),
            elev_range = elev_max - elev_min,
            precip_min = min(precip_min),
            precip_max = max(precip_max),
            temp_min = min(temp_min),
            temp_max = max(temp_max)
  ) %>%
    cbind(., (plot_values %>% 
            summarise(dens_mean = (aspen_dens[which(aspen_dens > 0)] %>% mean) * (10000 / 100),
                      dens_median = (aspen_dens[which(aspen_dens > 0)] %>% median) * (10000 / 100),
                      dens_max = (aspen_dens[which(aspen_dens > 0)] %>% max) * (10000 / 100),
                      precip_z_2019 = mean(precip_z_2019) %>% round(2),
                      temp_z_2019 = mean(temp_z_2019) %>% round(2))
            )) %>% 
  dplyr::select(names(table1_groupedByFire)) # get in same order as table1_groupedByFire


mapOrder <- data.frame(fire = c("Lyle Springs",
                                "Roosevelt",
                                "Marten Creek",
                                "Slate",
                                "Murdock",
                                "Willow Creek",
                                "Dollar Ridge",
                                "Coal Hollow",
                                "Pole Creek",
                                "Bald Mountain",
                                "Trail Mountain",
                                "Pole Canyon",
                                "West Valley",
                                "Stina",
                                "Cat",
                                "OVERALL SUMMARY"))

table1 <- rbind(table1_groupedByFire, table1_overall_summary) %>%
  merge(mapOrder, ., by = "fire", sort = F)


# TABLE 2 ------

historicalClimate <- readRDS("Data/historicalClimate")

# climate z-score mean/standard deviation of aspen seedling establishment years in each historical site
table2 <- historicalClimate %>%
  mutate(ppt_z = (ppt_annual - mean_ppt) / sd_ppt,
         temp_z = (temp_annual - mean_temp) / sd_temp,
         def_z = (def_annual - mean_def) / sd_def) %>%
  group_by(site) %>%
  summarise(year_min = min(year),
            year_max = max(year),
            nYear = unique(year) %>% length,
            mean_ppt_z = mean(ppt_z) %>% round(2),
            sd_ppt_z = sd(ppt_z) %>% round(2),
            mean_temp_z = mean(temp_z) %>% round(2),
            sd_temp_z = sd(temp_z) %>% round(2),
            mean_def_z = mean(def_z) %>% round(2),
            sd_def_z = sd(def_z) %>% round(2))

# overall climate z-score mean/standard deviation of aspen seedling establishment years across historical sites
 table2_overall <- historicalClimate %>%
  mutate(ppt_z = (ppt_annual - mean_ppt) / sd_ppt,
         temp_z = (temp_annual - mean_temp) / sd_temp,
         def_z = (def_annual - mean_def) / sd_def) %>%
  summarise(mean_ppt_z = mean(ppt_z) %>% round(2),
            sd_ppt_z = sd(ppt_z) %>% round(2),
            mean_temp_z = mean(temp_z) %>% round(2),
            sd_temp_z = sd(temp_z) %>% round(2),
            mean_def_z = mean(def_z, na.rm = T) %>% round(2),
            sd_def_z = sd(def_z, na.rm = T) %>% round(2))



# FIGURE 1 -------
library(rgdal)
library(raster)
library(sp)

#load in state perimeters
statePerims <- readOGR("Data/statePerimeters/statePerims.shp")

# load in Area of Interest
coords_AOI <- matrix(c(-109, 36,
                       -109, 45,
                       -114, 45,
                       -114, 36,
                       -109, 36), 
                     ncol = 2, byrow = TRUE) # define rectangle by long/lat
AOI_poly <- Polygon(coords_AOI)
AOI_sp <- SpatialPolygons(list(Polygons(list(AOI_poly), ID = "a")), proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")) 
AOI_df <- matrix(data = c(0))
rownames(AOI_df) = "a"  
AOI_spdf <- SpatialPolygonsDataFrame(AOI_sp, data= as.data.frame(AOI_df)) %>%
  spTransform(crs(statePerims))

# load in locations of searched fires
fireLocations <- readOGR("Data/fireLocations/fire_locations.shp")

# load in estimated locations of historical aspen seedling establishment
historicalSites_estimatedLocations <- readOGR("Data/historicalSites_estimatedLocation/historicalSites_estimated.shp")

# load in raster of aspen cover/distribution (from Ellenwood et al. 2015; via Google Earth Engine)
aspenDist <- raster("Data/aspenDist_raster.tif")

# Figure1a
plot(statePerims, border = NA)
plot(aspenDist, col = "grey30", border = NA, add = T)
plot(statePerims, border = "grey50", add = T)
points(historicalSites_estimatedLocations, col = "cyan", pch = 16, cex = 1.5)
points(historicalSites_estimatedLocations, cex = 1.5)
points(fireLocations, col = "red", pch = 16, cex = 1.5)
points(fireLocations, cex = 1.5)
plot(AOI_spdf , add = TRUE, lty = 2, lwd = 2, border = "red")
# location labels assigned manually in Adobe Illustrator


# load in basal area (in meters per hectare) and 1981-2010 climate averages (mean temp, precip) of 
# 10,000 random points within aspen's distribution in the western U.S. (Ellenwood et al. 2015).
# Data acquired from PRISM datasets in Google Earth Engine

aspenDist <- readRDS("Data/aspenDist_ba_climate") # load in data from 10,000 random points

library(ggplot2)
figure1b <- ggplot() + 
  # aspen distribution across western U.S.
  geom_point(data = aspenDist, 
             shape = 16, col = "grey65", 
             aes(x = temp_annual, y = ppt_annual, 
                 size = ba, # pixels with more basal area get bigger dots
                 alpha = ba)) + # pixels with more basal area are less transparent
  
  # climate of plots without aspen seedling establishment
  # jittered to see all of them
  geom_jitter(width = 0.2, height = 20, 
              data = plot_values %>% filter(occ_aspenSeedling == 0),
              aes(x = temp, y = precip),
              col = "red", fill = "white", size = 3, shape = 21, alpha = .8) +
  
  # climate of plots with aspen seedling establishment
  # jittered to see all of them
  geom_jitter(width = .2, height = 20, 
              data = plot_values %>% filter(occ_aspenSeedling == 1),
              aes(x = temp, y = precip),
              col = "red", fill = "red", size = 3, shape = 21, alpha = .8) +
  labs(x = "Mean Annual Temperature (°C) ", y = "Annual Precipitation (mm)") + 
  xlim(-1, 12) + 
  scale_y_continuous(breaks = seq(300, 1200, by = 100),limits = c(300, 1200)) + 
  theme_classic() + 
  theme(legend.position = "none") 


# OCCUPANCY MODELING --------


######## latitude adjusted elevation ########.
plot_values$lat %>% range # range of latitude across plots

# because plots span 8 degrees of latitude, elevation is not strictly comparable across study area
# calculate latitude-adjusted elevation

# load elevation/latitude of random points containing aspen cover (Ellenwood et al. 2015) within study area
# data accessed via Google Earth Engine
aspenElevLat <- readRDS("Data/AspenElevLat") # elevation in meters, latitude in degrees
plot(elevation ~ lat, data = aspenElevLat) # visualize relationship
elev.mod <- lm(elevation ~ lat, data = aspenElevLat) # model elevation as a function of latitude
summary(elev.mod) # model summary

# apply latitude adjustment to elevation (anomaly in meters from latitude-predicted mean elevation)
plot_values$elev.adj <- plot_values$elev - predict(elev.mod, newdata = data.frame(lat = plot_values$lat))

# precip and temp correlated with latitude-adjusted elevation
cor(plot_values$elev.adj, plot_values$precip)
cor(plot_values$elev.adj, plot_values$temp)




######## model selection ########.

# scale data for model
seedlingOccModData <- plot_values %>% 
  dplyr::select(dNBR, burn, slope, 
                canCov, suckerDens, aspenDist, 
                aspectTrans, herbComp, 
                precip, temp) %>% 
  rename(aspect = aspectTrans) %>% # rename transformed aspect to just "aspect"
  scale() %>% # scale and center predictor variables
  as.data.frame() %>% 
  mutate(occ_aspenSeedling = plot_values$occ_aspenSeedling) # add in binary aspen seedling occupancy (response variable)


# load Rethinking package
# installation instructions: https://www.rdocumentation.org/packages/rethinking/versions/2.13
require(rethinking)  


# function to iterate model selection
iterateMod.occ <- function(paramsToRemove, # which parameters are removed each iteration in a given step
                           modelStep # model step (used to name model output)
                           ){
  
  predName.occ <- # vector of predictors in full model
    c("dNBR",
      "burn",
      "slope",
      "canCov",
      "suckerDens",
      "aspenDist",
      "aspect",
      "herbComp",
      "precip",
      "temp",
      "precipTempInt")
  
  # make sure that parameters that are listed are in the list of possible parameters
  for(i in paramsToRemove){
    if(!(i %in% predName.occ)){stop(paste0('"', i, '" is not a parameter'))}
  }
  
  
  # if any removed variable is part of an interaction, have to remove the interaction as well
  if(sum(paramsToRemove %in% c("precip", "temp")) > 0){
    paramsToRemove <- c("precipTempInt", paramsToRemove) %>% unique
  }
  

  # MODEL STRUCTURE
  
  # text of model beginning
  modStart <- "ulam(alist(occ_aspenSeedling ~ dbinom( 1 , p ), logit(p) <- a +"
  
  # vector of linear predictor expressions
  predExpr <- c("bNBR * dNBR", 
                "bBurn * burn",
                "bSlope * slope",
                "bCanCov * canCov",
                "bSuckerDens * suckerDens", 
                "bAspenDist * aspenDist",
                "bAspect * aspect",
                "bHerb * herbComp",
                "bPrecip * precip",
                "bTemp * temp",
                "bPrecipTempInt * precip * temp") 
  
  # vector of prior expressions
  priors <- c("bNBR ~ dnorm(0, 3)",
              "bBurn ~ dnorm(0, 3)",
              "bSlope ~ dnorm(0, 3)",
              "bCanCov ~ dnorm(0, 3)",
              "bSuckerDens ~ dnorm(0, 3)",
              "bAspenDist ~ dnorm(0, 3)",
              "bAspect ~ dnorm(0, 3)",
              "bHerb ~ dnorm(0, 3)",
              "bPrecip~ dnorm(0, 3)",
              "bTemp  ~ dnorm(0, 3)",
              "bPrecipTempInt ~ dnorm(0, 3)") 
  
  # put predictors/priors into dataframe to link them together
  pred_df <- data.frame(predName = predName.occ,
                        predExpr = predExpr,
                        priors = priors) 
  if(!is.null(paramsToRemove)) pred_df <- pred_df[-which(pred_df$predName %in% paramsToRemove),] # remove any necessary parameters
  
  # text of model end
  modEnd <- ", a ~ normal(0, 1.5) ), data= seedlingOccModData, chains=3 , cores=3, log_lik = T )"
  
  
  # paste all model-code together
  modText <- paste(
    modStart,
    paste(pred_df$predExpr, collapse = " + "),
    ",",
    paste(pred_df$priors, collapse = ","),
    modEnd,
    collapse = ""
  ) 
  
  # run model
  model <- eval(parse(text = modText)) # turn text into code to run
  
  saveRDS(model, paste0("ModelOutput/modocc_", modelStep, "_", paramsToRemove[length(paramsToRemove)])) # save model object
  return(model)
  
}


# vector of predictors in full model (used to specify which predictors to remove)
predName.occ <- 
  c("dNBR",
    "burn",
    "slope",
    "canCov",
    "suckerDens",
    "aspenDist",
    "aspect",
    "herbComp",
    "precip",
    "temp",
    "precipTempInt")


# Full Model --------#

modocc_full <- iterateMod.occ(modelStep = "full", 
                              paramsToRemove = NULL) 

# look at model estimates
precis(modocc_full) 
plot(modocc_full)

# Backwards Elimination Step 1 ---------#

# run models, each one removing one predictor
modocc_back1_1  <- iterateMod.occ(modelStep = "back1", paramsToRemove = predName.occ[1]) 
modocc_back1_2  <- iterateMod.occ(modelStep = "back1", paramsToRemove = predName.occ[2]) 
modocc_back1_3  <- iterateMod.occ(modelStep = "back1", paramsToRemove = predName.occ[3]) 
modocc_back1_4  <- iterateMod.occ(modelStep = "back1", paramsToRemove = predName.occ[4]) 
modocc_back1_5  <- iterateMod.occ(modelStep = "back1", paramsToRemove = predName.occ[5]) 
modocc_back1_6  <- iterateMod.occ(modelStep = "back1", paramsToRemove = predName.occ[6]) 
modocc_back1_7  <- iterateMod.occ(modelStep = "back1", paramsToRemove = predName.occ[7])
modocc_back1_8  <- iterateMod.occ(modelStep = "back1", paramsToRemove = predName.occ[8]) 
modocc_back1_9  <- iterateMod.occ(modelStep = "back1", paramsToRemove = predName.occ[9]) 
modocc_back1_10  <- iterateMod.occ(modelStep = "back1", paramsToRemove = predName.occ[10]) 
modocc_back1_11  <- iterateMod.occ(modelStep = "back1", paramsToRemove = predName.occ[11]) 

# read in models (commented out unless need to use them; used if R crashes to recover step in model selection)
#modocc_full <- readRDS("ModelOutput/modocc_full_")
#modocc_back1_1 <- readRDS(paste0("ModelOutput/modocc_back1_", predName.occ[1]))
#modocc_back1_2 <- readRDS(paste0("ModelOutput/modocc_back1_", predName.occ[2]))
#modocc_back1_3 <- readRDS(paste0("ModelOutput/modocc_back1_", predName.occ[3]))
#modocc_back1_4 <- readRDS(paste0("ModelOutput/modocc_back1_", predName.occ[4]))
#modocc_back1_5 <- readRDS(paste0("ModelOutput/modocc_back1_", predName.occ[5]))
#modocc_back1_6 <- readRDS(paste0("ModelOutput/modocc_back1_", predName.occ[6]))
#modocc_back1_7 <- readRDS(paste0("ModelOutput/modocc_back1_", predName.occ[7]))
#modocc_back1_8 <- readRDS(paste0("ModelOutput/modocc_back1_", predName.occ[8]))
#modocc_back1_9 <- readRDS(paste0("ModelOutput/modocc_back1_", predName.occ[9]))
#modocc_back1_10 <- readRDS(paste0("ModelOutput/modocc_back1_", predName.occ[10]))
#modocc_back1_11 <- readRDS(paste0("ModelOutput/modocc_back1_", predName.occ[11]))

# compare backwards elimination STEP 1 with full model
bestMod.occ.1 <- compare(modocc_full,
                         modocc_back1_1,
                         modocc_back1_2,
                         modocc_back1_3,
                         modocc_back1_4,
                         modocc_back1_5,
                         modocc_back1_6,
                         modocc_back1_7,
                         modocc_back1_8,
                         modocc_back1_9,
                         modocc_back1_10,
                         modocc_back1_11) %>% # orders models by increasing WAIC (best model is at top)
  rownames %>% # take rownames (model names)
  .[1] # select first model name (best model)

selected.Mod.occ.1 <- eval(parse(text = bestMod.occ.1)) # call best model object, rename it as the selected model from this step
precis(selected.Mod.occ.1) # visualize model posteriors
saveRDS(selected.Mod.occ.1, "ModelOutput/selected.Mod.occ.1")  # save selected model

# read in selected model 
selected.Mod.occ.1 <- readRDS("ModelOutput/selected.Mod.occ.1") 

if(bestMod.occ.1 != "modocc_full"){ # if the selected model is not the same as the full model...
  exclude.occ.1 <- bestMod.occ.1 %>% strsplit("_") %>% unlist %>% .[length(.)] %>% as.numeric # permanently exclude the predictor that was removed in that model
  
  if(sum(predName.occ[exclude.occ.1] %in% c("precip", "temp")) > 0){ # if one of the parameters in the interaction is removed...
    exclude.occ.1 <- c(which(predName.occ == "precipTempInt"), exclude.occ.1) %>% unique # also remove the interaction term
  }
}

# run models, each one removing one predictor, plus parameter removed in previous step
if(!(1 %in% exclude.occ.1)){modocc_back2_1  <- iterateMod.occ(modelStep = "back2", paramsToRemove = c(predName.occ[c(exclude.occ.1)], predName.occ[1]))} 
if(!(2 %in% exclude.occ.1)){modocc_back2_2  <- iterateMod.occ(modelStep = "back2", paramsToRemove = c(predName.occ[c(exclude.occ.1)], predName.occ[2]))} 
if(!(3 %in% exclude.occ.1)){modocc_back2_3  <- iterateMod.occ(modelStep = "back2", paramsToRemove = c(predName.occ[c(exclude.occ.1)], predName.occ[3]))} 
if(!(4 %in% exclude.occ.1)){modocc_back2_4  <- iterateMod.occ(modelStep = "back2", paramsToRemove = c(predName.occ[c(exclude.occ.1)], predName.occ[4]))} 
if(!(5 %in% exclude.occ.1)){modocc_back2_5  <- iterateMod.occ(modelStep = "back2", paramsToRemove = c(predName.occ[c(exclude.occ.1)], predName.occ[5]))} 
if(!(6 %in% exclude.occ.1)){modocc_back2_6  <- iterateMod.occ(modelStep = "back2", paramsToRemove = c(predName.occ[c(exclude.occ.1)], predName.occ[6]))} 
if(!(7 %in% exclude.occ.1)){modocc_back2_7  <- iterateMod.occ(modelStep = "back2", paramsToRemove = c(predName.occ[c(exclude.occ.1)], predName.occ[7]))} 
if(!(8 %in% exclude.occ.1)){modocc_back2_8  <- iterateMod.occ(modelStep = "back2", paramsToRemove = c(predName.occ[c(exclude.occ.1)], predName.occ[8]))} 
if(!(9 %in% exclude.occ.1)){modocc_back2_9  <- iterateMod.occ(modelStep = "back2", paramsToRemove = c(predName.occ[c(exclude.occ.1)], predName.occ[9]))} 
if(!(10 %in% exclude.occ.1)){modocc_back2_10  <- iterateMod.occ(modelStep = "back2", paramsToRemove = c(predName.occ[c(exclude.occ.1)], predName.occ[10]))} 
if(!(11 %in% exclude.occ.1)){modocc_back2_11  <- iterateMod.occ(modelStep = "back2", paramsToRemove = c(predName.occ[c(exclude.occ.1)], predName.occ[11]))} 


# Backwards Elimination Step 2 -------#

selected.Mod.occ.1 <- readRDS("ModelOutput/selected.Mod.occ.1") # selected model from previous step

# find parameters that are still in contention
params.occ.1 <- list.files("ModelOutput/") %>% 
  .[which(grepl('^modocc_back2_', .))] %>%
  strsplit("_") %>%
  lapply(function(x) x[length(x)]) %>%
  unlist 
params.occ.num.1 <- which(predName.occ %in% params.occ.1)

# read in models (commented out unless need to use them; used if R crashes to recover step in model selection)
#modocc_back2_1 <- readRDS(paste0("ModelOutput/modocc_back2_", predName.occ[1]))
#modocc_back2_2 <- readRDS(paste0("ModelOutput/modocc_back2_", predName.occ[2]))
#modocc_back2_3 <- readRDS(paste0("ModelOutput/modocc_back2_", predName.occ[3]))
#modocc_back2_4 <- readRDS(paste0("ModelOutput/modocc_back2_", predName.occ[4]))
#modocc_back2_5 <- readRDS(paste0("ModelOutput/modocc_back2_", predName.occ[5]))
#modocc_back2_6 <- readRDS(paste0("ModelOutput/modocc_back2_", predName.occ[6]))
#modocc_back2_7 <- readRDS(paste0("ModelOutput/modocc_back2_", predName.occ[7]))
#modocc_back2_8 <- readRDS(paste0("ModelOutput/modocc_back2_", predName.occ[8]))
#modocc_back2_9 <- readRDS(paste0("ModelOutput/modocc_back2_", predName.occ[9]))
#modocc_back2_10 <- readRDS(paste0("ModelOutput/modocc_back2_", predName.occ[10]))
#modocc_back2_11 <- readRDS(paste0("ModelOutput/modocc_back2_", predName.occ[11]))


# compare backwards elimination STEP 2 with best model from STEP 1
bestMod.occ.2 <- eval(parse(text = 
                              paste0("compare(selected.Mod.occ.1,", 
                                     paste("modocc_back2", params.occ.num.1, sep = "_", collapse = ","),
                                     ")"))) %>%
  rownames %>%
  .[1]


selected.Mod.occ.2 <- eval(parse(text = bestMod.occ.2))
saveRDS(selected.Mod.occ.2, "ModelOutput/selected.Mod.occ.2")


if(bestMod.occ.2 != "selected.Mod.occ.1"){ # if the selected model better than the best model from the previous step...
  exclude.occ.2 <- c(exclude.occ.1, (bestMod.occ.2 %>% strsplit("_") %>% unlist %>% .[length(.)] %>% as.numeric)) # permanently exclude the predictor that was removed in that model
  
   
  if(sum(predName.occ[exclude.occ.2] %in% c("precip", "temp")) > 0){ # if one of the parameters in the interaction is removed...
    exclude.occ.2 <- c(which(predName.occ == "precipTempInt"), exclude.occ.2) %>% unique # also remove the interaction term
  }
  
  
}

# run models, each one removing one predictor, plus parameter removed in previous step
if(!(1 %in% exclude.occ.2)){modocc_back3_1  <- iterateMod.occ(modelStep = "back3", paramsToRemove = c(predName.occ[c(exclude.occ.2)], predName.occ[1]))} 
if(!(2 %in% exclude.occ.2)){modocc_back3_2  <- iterateMod.occ(modelStep = "back3", paramsToRemove = c(predName.occ[c(exclude.occ.2)], predName.occ[2]))} 
if(!(3 %in% exclude.occ.2)){modocc_back3_3  <- iterateMod.occ(modelStep = "back3", paramsToRemove = c(predName.occ[c(exclude.occ.2)], predName.occ[3]))} 
if(!(4 %in% exclude.occ.2)){modocc_back3_4  <- iterateMod.occ(modelStep = "back3", paramsToRemove = c(predName.occ[c(exclude.occ.2)], predName.occ[4]))} 
if(!(5 %in% exclude.occ.2)){modocc_back3_5  <- iterateMod.occ(modelStep = "back3", paramsToRemove = c(predName.occ[c(exclude.occ.2)], predName.occ[5]))} 
if(!(6 %in% exclude.occ.2)){modocc_back3_6  <- iterateMod.occ(modelStep = "back3", paramsToRemove = c(predName.occ[c(exclude.occ.2)], predName.occ[6]))} 
if(!(7 %in% exclude.occ.2)){modocc_back3_7  <- iterateMod.occ(modelStep = "back3", paramsToRemove = c(predName.occ[c(exclude.occ.2)], predName.occ[7]))} 
if(!(8 %in% exclude.occ.2)){modocc_back3_8  <- iterateMod.occ(modelStep = "back3", paramsToRemove = c(predName.occ[c(exclude.occ.2)], predName.occ[8]))} 
if(!(9 %in% exclude.occ.2)){modocc_back3_9  <- iterateMod.occ(modelStep = "back3", paramsToRemove = c(predName.occ[c(exclude.occ.2)], predName.occ[9]))} 
if(!(10 %in% exclude.occ.2)){modocc_back3_10  <- iterateMod.occ(modelStep = "back3", paramsToRemove = c(predName.occ[c(exclude.occ.2)], predName.occ[10]))} 
if(!(11 %in% exclude.occ.2)){modocc_back3_11  <- iterateMod.occ(modelStep = "back3", paramsToRemove = c(predName.occ[c(exclude.occ.2)], predName.occ[11]))} 

# Backwards Elimination Step 3 -------#

selected.Mod.occ.2 <- readRDS("ModelOutput/selected.Mod.occ.2") # selected model from previous step

# find parameters that are still in contention
params.occ.2 <- list.files("ModelOutput/") %>% 
  .[which(grepl('^modocc_back3_', .))] %>%
  strsplit("_") %>%
  lapply(function(x) x[length(x)]) %>%
  unlist 
params.occ.num.2 <- which(predName.occ %in% params.occ.2)

# read in models (commented out unless need to use them; used if R crashes to recover step in model selection)
#modocc_back3_1 <- readRDS(paste0("ModelOutput/modocc_back3_", predName.occ[1]))
#modocc_back3_2 <- readRDS(paste0("ModelOutput/modocc_back3_", predName.occ[2]))
#modocc_back3_3 <- readRDS(paste0("ModelOutput/modocc_back3_", predName.occ[3]))
#modocc_back3_4 <- readRDS(paste0("ModelOutput/modocc_back3_", predName.occ[4]))
#modocc_back3_5 <- readRDS(paste0("ModelOutput/modocc_back3_", predName.occ[5]))
#modocc_back3_6 <- readRDS(paste0("ModelOutput/modocc_back3_", predName.occ[6]))
#modocc_back3_7 <- readRDS(paste0("ModelOutput/modocc_back3_", predName.occ[7]))
#modocc_back3_8 <- readRDS(paste0("ModelOutput/modocc_back3_", predName.occ[8]))
#modocc_back3_9 <- readRDS(paste0("ModelOutput/modocc_back3_", predName.occ[9]))
#modocc_back3_10 <- readRDS(paste0("ModelOutput/modocc_back3_", predName.occ[10]))
#modocc_back3_11 <- readRDS(paste0("ModelOutput/modocc_back3_", predName.occ[11]))

# compare backwards elimination STEP 3 with best model from STEP 2
bestMod.occ.3 <- eval(parse(text = 
                              paste0("compare(selected.Mod.occ.2,", 
                                     paste("modocc_back3", params.occ.num.2, sep = "_", collapse = ","),
                                     ")"))) %>%
  rownames %>%
  .[1]


selected.Mod.occ.3 <- eval(parse(text = bestMod.occ.3))
saveRDS(selected.Mod.occ.3, "ModelOutput/selected.Mod.occ.3")


if(bestMod.occ.3 != "selected.Mod.occ.2"){ # if the selected model better than the best model from the previous step...
  exclude.occ.3 <- c(exclude.occ.2, (bestMod.occ.3 %>% strsplit("_") %>% unlist %>% .[length(.)] %>% as.numeric)) # permanently exclude the predictor that was removed in that model
  
  
  if(sum(predName.occ[exclude.occ.3] %in% c("precip", "temp")) > 0){ # if one of the parameters in the interaction is removed...
    exclude.occ.3 <- c(which(predName.occ == "precipTempInt"), exclude.occ.3) %>% unique # also remove the interaction term
  }
  
}

# run models, each one removing one predictor, plus parameter removed in previous step
if(!(1 %in% exclude.occ.3)){modocc_back4_1  <- iterateMod.occ(modelStep = "back4", paramsToRemove = c(predName.occ[c(exclude.occ.3)], predName.occ[1]))} 
if(!(2 %in% exclude.occ.3)){modocc_back4_2  <- iterateMod.occ(modelStep = "back4", paramsToRemove = c(predName.occ[c(exclude.occ.3)], predName.occ[2]))} 
if(!(3 %in% exclude.occ.3)){modocc_back4_3  <- iterateMod.occ(modelStep = "back4", paramsToRemove = c(predName.occ[c(exclude.occ.3)], predName.occ[3]))} 
if(!(4 %in% exclude.occ.3)){modocc_back4_4  <- iterateMod.occ(modelStep = "back4", paramsToRemove = c(predName.occ[c(exclude.occ.3)], predName.occ[4]))} 
if(!(5 %in% exclude.occ.3)){modocc_back4_5  <- iterateMod.occ(modelStep = "back4", paramsToRemove = c(predName.occ[c(exclude.occ.3)], predName.occ[5]))} 
if(!(6 %in% exclude.occ.3)){modocc_back4_6  <- iterateMod.occ(modelStep = "back4", paramsToRemove = c(predName.occ[c(exclude.occ.3)], predName.occ[6]))} 
if(!(7 %in% exclude.occ.3)){modocc_back4_7  <- iterateMod.occ(modelStep = "back4", paramsToRemove = c(predName.occ[c(exclude.occ.3)], predName.occ[7]))} 
if(!(8 %in% exclude.occ.3)){modocc_back4_8  <- iterateMod.occ(modelStep = "back4", paramsToRemove = c(predName.occ[c(exclude.occ.3)], predName.occ[8]))} 
if(!(9 %in% exclude.occ.3)){modocc_back4_9  <- iterateMod.occ(modelStep = "back4", paramsToRemove = c(predName.occ[c(exclude.occ.3)], predName.occ[9]))} 
if(!(10 %in% exclude.occ.3)){modocc_back4_10  <- iterateMod.occ(modelStep = "back4", paramsToRemove = c(predName.occ[c(exclude.occ.3)], predName.occ[10]))} 
if(!(11 %in% exclude.occ.3)){modocc_back4_11  <- iterateMod.occ(modelStep = "back4", paramsToRemove = c(predName.occ[c(exclude.occ.3)], predName.occ[11]))} 


# Backwards Elimination Step 4 -------#

selected.Mod.occ.3 <- readRDS("ModelOutput/selected.Mod.occ.3") # selected model from previous step

# find parameters that are still in contention
params.occ.3 <- list.files("ModelOutput/") %>% 
  .[which(grepl('^modocc_back4_', .))] %>%
  strsplit("_") %>%
  lapply(function(x) x[length(x)]) %>%
  unlist 
params.occ.num.3 <- which(predName.occ %in% params.occ.3)


# read in models (commented out unless need to use them; used if R crashes to recover step in model selection)
#modocc_back4_1 <- readRDS(paste0("ModelOutput/modocc_back4_", predName.occ[1]))
#modocc_back4_2 <- readRDS(paste0("ModelOutput/modocc_back4_", predName.occ[2]))
#modocc_back4_3 <- readRDS(paste0("ModelOutput/modocc_back4_", predName.occ[3]))
#modocc_back4_4 <- readRDS(paste0("ModelOutput/modocc_back4_", predName.occ[4]))
#modocc_back4_5 <- readRDS(paste0("ModelOutput/modocc_back4_", predName.occ[5]))
#modocc_back4_6 <- readRDS(paste0("ModelOutput/modocc_back4_", predName.occ[6]))
#modocc_back4_7 <- readRDS(paste0("ModelOutput/modocc_back4_", predName.occ[7]))
#modocc_back4_8 <- readRDS(paste0("ModelOutput/modocc_back4_", predName.occ[8]))
#modocc_back4_9 <- readRDS(paste0("ModelOutput/modocc_back4_", predName.occ[9]))
#modocc_back4_10 <- readRDS(paste0("ModelOutput/modocc_back4_", predName.occ[10]))
#modocc_back4_11 <- readRDS(paste0("ModelOutput/modocc_back4_", predName.occ[11]))

# compare backwards elimination STEP 4 with best model from STEP 3
bestMod.occ.4 <- eval(parse(text = 
                              paste0("compare(selected.Mod.occ.3,", 
                                     paste("modocc_back4", params.occ.num.3, sep = "_", collapse = ","),
                                     ")"))) %>%
  rownames %>%
  .[1]


selected.Mod.occ.4 <- eval(parse(text = bestMod.occ.4))
saveRDS(selected.Mod.occ.4, "ModelOutput/selected.Mod.occ.4")

if(bestMod.occ.4 == "selected.Mod.occ.3"){ # if best model is the same as the parent model...
  saveRDS(selected.Mod.occ.4, "ModelOutput/modocc_final") # this becomes the final model
  
}



# Final Model --------#

modocc_final <- readRDS("ModelOutput/modocc_final")
precis(modocc_final)

# re-run final model with increased iterations for inference

# function to take final model and re-run it
finalModelRun <- function(model, iter, warmup, d){
  call <- model@call
  model_long <- ulam(flist = eval(parse(text = unlist(call)[2] %>% as.character)),
                     data  =  d,
                     chains = call$chains %>% as.integer,
                     cores = call$cores %>% as.integer,
                     log_lik = T,
                     iter = iter,
                     warmup = warmup)
  return(model_long)
}

# run final model with increased chains 
modocc_final_inference <- finalModelRun(model = modocc_final,
                                        d = seedlingOccModData,
                                        iter= 4000,
                                        warmup = 1000)
saveRDS(modocc_final_inference, "ModelOutput/modocc_final_inference")
precis(modocc_final_inference)


# Sucker occupancy --------#

suckerOccModData <- plot_values %>% 
  dplyr::select(burn, slope, 
                canCov, aspenDist, 
                aspectTrans, 
                precip) %>% # include all predictors from final aspen seedling occupancy model (except sucker density)
  rename(aspect = aspectTrans) %>% # rename aspect 
  scale() %>% # scale data as with seedling occupancy model
  as.data.frame() %>% 
  mutate(sucker_occ = ifelse(plot_values$suckerDens > 0, 1, 0)) # add in sucker occupancy as response variable
  

require(rethinking)
modSuckerocc_final_inference <- ulam(
  alist(
    sucker_occ ~ dbinom(1, p), 
    logit(p) <- 
      a + 
      bBurn * burn + 
      bSlope * slope + 
      bCanCov * canCov + 
      bAspenDist * aspenDist + 
      bAspect * aspect + 
      bPrecip * precip, 
    
    # priors
    bBurn ~ dnorm(0, 3), 
    bSlope ~ dnorm(0, 3), 
    bCanCov ~ dnorm(0, 3), 
    bAspenDist ~ dnorm(0, 3), 
    bAspect ~ dnorm(0, 3), 
    bPrecip ~ dnorm(0, 3), 
    a ~ normal(0, 1.5)),
  data=suckerOccModData, iter =4000, chains = 4, log_lik = T
)

saveRDS(modSuckerocc_final_inference, "ModelOutput/modSuckerocc_final_inference")
precis(modSuckerocc_final_inference)

# SUPPLEMENTAL 1 --------
modocc_final_inference <- readRDS("ModelOutput/modocc_final_inference") # final seedling occupancy model

finalModPredictors <- modocc_final_inference@coef %>% # get coefficients from final model
  names %>% # get just names of coefficients
  .[-which(. == "a")] %>% # remove "a" (intercept) --> now is just subsetted to predictors
  substring(first = 2) %>% # remove "b" from name (e.g., "Burn" instead of "bBurn")
  tolower %>% # make lowercase to match
  as.data.frame %>%
  mutate(inclusion = 1)
names(finalModPredictors)[1] <- "predictor"


predictors <- supp1_mean <- plot_values %>%
  dplyr::select(dNBR, burn, slope, aspectTrans, canCov, aspenDist,
         suckerDens, herbComp, precip, temp) %>% # select original candidate predictors
  rename(aspect = aspectTrans,
         dnbr = dNBR,
         cancov = canCov,
         aspendist = aspenDist,
         suckerdens = suckerDens,
         herbcomp = herbComp)

supplemental1 <- data.frame(mean = predictors %>% apply(2, mean) %>% round(2),
                            min = predictors %>% apply(2, min) %>% round(2),
                            max = predictors %>% apply(2, max) %>% round(2)) %>%
  mutate(predictor = rownames(.)) %>%
  merge(., finalModPredictors, by = "predictor", all = T) %>% # add in whether predictor was in final model
  mutate(inclusion = ifelse(is.na(inclusion), 0, inclusion))
  
  


finalModPredictors 


# SUPPLEMENTAL 2 -------- 
modocc_final_inference <- readRDS("ModelOutput/modocc_final_inference") # final seedling occupancy model
modSuckerocc_final_inference <- readRDS("ModelOutput/modSuckerocc_final_inference") # final sucker occupancy model

# function to make posterior calculations
postCalc <- function(x, round = 2){
  mean <- x %>% mean
  pi <- x %>% PI(prob = 0.89)
  pi.50 <- x %>% PI(prob = 0.50)
  aboveBelow0 <- ifelse(mean(x) < 0, 
                        (x < 0) %>% mean,
                        (x > 0) %>% mean)
  data.frame(mean = mean,
             q5.5 = pi[1] %>% as.vector,
             q94.5 = pi[2] %>% as.vector,
             q25 = pi.50[1] %>% as.vector,
             q75 = pi.50[2] %>% as.vector,
             f = aboveBelow0) %>%
    round(round)
}

# extract samples from posterior
post.modocc_final_inference <- extract.samples(modocc_final_inference)
post.modSuckerocc_final_inference  <- extract.samples(modSuckerocc_final_inference)

# calculate posterior probability intervals
# values may differ slightly (i.e., by 0.01) from values in paper due to sampling variation
occ.seedling.params <- 
  post.modocc_final_inference %>% 
  lapply(postCalc) %>% 
  do.call(rbind, .) %>% 
  mutate(param = rownames(.)) %>%
  .[order(abs(precis(modocc_final_inference)$mean), decreasing = T),]

occ.suckers.params <- 
  post.modSuckerocc_final_inference %>% 
  lapply(postCalc) %>% 
  do.call(rbind, .) %>% 
  mutate(param = rownames(.)) %>%
  .[order(abs(precis(modSuckerocc_final_inference)$mean), decreasing = T),]


# FIGURE 2 -------
modocc_final_inference <- readRDS("ModelOutput/modocc_final_inference") # final seedling occupancy model
modSuckerocc_final_inference <- readRDS("ModelOutput/modSuckerocc_final_inference") # final sucker occupancy model

# create sequence of each variable (possible prediction space; in scaled data)
burnSeq <- seq(min(seedlingOccModData$burn), 
               max(seedlingOccModData$burn), length.out = 100)
slopeSeq <- seq(min(seedlingOccModData$slope), 
                max(seedlingOccModData$slope), length.out = 100)
canCovSeq <- seq(min(seedlingOccModData$canCov), 
                 max(seedlingOccModData$canCov), length.out = 100)
aspenDistSeq <- seq(min(seedlingOccModData$aspenDist), 
                    max(seedlingOccModData$aspenDist), length.out = 100)
aspectSeq <- seq(min(seedlingOccModData$aspect), 
                 max(seedlingOccModData$aspect), length.out = 100)
precipSeq <- seq(min(seedlingOccModData$precip), 
                   max(seedlingOccModData$precip), length.out = 100)
suckerDensSeq <- seq(min(seedlingOccModData$suckerDens), 
                  max(seedlingOccModData$suckerDens), length.out = 100)

# back-transform scaled sequences to actual values for visualization 
burnSeq_backTran <- (burnSeq * sd(plot_values$burn) + mean(plot_values$burn)) * 100 # convert from proportion to percentage
slopeSeq_backTran <- slopeSeq * sd(plot_values$slope) + mean(plot_values$slope)
canCovSeq_backTran <- (canCovSeq * sd(plot_values$canCov) + mean(plot_values$canCov)) * 100  # convert from proportion to percentage
aspenDistSeq_backTran <- aspenDistSeq * sd(plot_values$aspenDist) + mean(plot_values$aspenDist)
aspectSeq_backTran <- aspectSeq * sd(plot_values$aspectTrans) + mean(plot_values$aspectTrans)
suckerDensSeq_backTran <- (suckerDensSeq * sd(plot_values$suckerDens) + mean(plot_values$suckerDens)) * 100  # convert from proportion to percentage
precipSeq_backTran <- precipSeq * sd(plot_values$precip) + mean(plot_values$precip)


# function to generate predictions
occPredFun <- function(var, # which predictor's sequence is being used
                       mod, # model to predict with
                       data = data.frame(burn = rep(0, length = 100),
                                         slope = 0,
                                         canCov = 0,
                                         aspenDist = 0,
                                         aspect = 0,
                                         precip = 0,
                                         suckerDens = 0)) # unless declared otherwise, all predictors are set to 0 (their mean)
{
  data[,which(names(data) == paste(var))] <- eval(parse(text = paste0(var, "Seq"))) # put predictor sequence into predictive input data
  post <- link(mod, data = data) # extract draws from posterior
  mean <- post %>% apply(2, mean) # calculate mean of draws
  probInt <- post %>% apply(2, PI, prob = 0.89) # calculate 89% confidence interval around mean
  
  return(list(mean = mean, probInt = probInt))
  
}

# generate predictions
seedlingOcc_burn <- occPredFun(mod = modocc_final_inference, var = "burn")
seedlingOcc_slope <- occPredFun(mod = modocc_final_inference, var = "slope")
seedlingOcc_canCov <- occPredFun(mod = modocc_final_inference, var = "canCov")
seedlingOcc_aspenDist <- occPredFun(mod = modocc_final_inference, var = "aspenDist")
seedlingOcc_aspect <- occPredFun(mod = modocc_final_inference, var = "aspect")
seedlingOcc_precip <- occPredFun(mod = modocc_final_inference, var = "precip")
seedlingOcc_suckerDens <- occPredFun(mod = modocc_final_inference, var = "suckerDens")

suckerOcc_burn <- occPredFun(mod = modSuckerocc_final_inference, var = "burn")
suckerOcc_slope <- occPredFun(mod = modSuckerocc_final_inference, var = "slope")
suckerOcc_canCov <- occPredFun(mod = modSuckerocc_final_inference, var = "canCov")
suckerOcc_aspenDist <- occPredFun(mod = modSuckerocc_final_inference, var = "aspenDist")
suckerOcc_aspect <- occPredFun(mod = modSuckerocc_final_inference, var = "aspect")
suckerOcc_precip <- occPredFun(mod = modSuckerocc_final_inference, var = "precip")

# Figure 2
predData_lowSuckDens <- data.frame(burn = 0,
                                   slope = 0,
                                   canCov = 0,
                                   aspenDist = 0,
                                   aspect = 0,
                                   precip = precipSeq, # predict across range of precipitation
                                   suckerDens = quantile(seedlingOccModData$suckerDens, 0.1) %>% as.vector) # and at low sucker density (10%)
predData_highSuckDens <- data.frame(burn = 0,
                                    slope = 0,
                                    canCov = 0,
                                    aspenDist = 0,
                                    aspect = 0,
                                    precip = precipSeq, # predict across range of precipitation
                                    suckerDens = quantile(seedlingOccModData$suckerDens, 0.9) %>% as.vector) # and at high sucker density (90%)


post_low <- link(modocc_final_inference, data = predData_lowSuckDens) # predict using new data
mean_low <- post_low %>% apply(2, mean) # calculate posterior mean
probInt_low <- post_low %>% apply(2, PI, prob = 0.89) # calculate 89% posterior probability intervals

post_high <- link(modocc_final_inference, data = predData_highSuckDens) # predict using new data
mean_high <- post_high %>% apply(2, mean) # calculate posterior mean
probInt_high <- post_high %>% apply(2, PI, prob = 0.89) # calculate 89% posterior probability intervals


ggplot() +
  # sucker prediction
  geom_line(aes(y = suckerOcc_precip$mean, x = precipSeq_backTran)) + 
  geom_ribbon(aes(ymin = suckerOcc_precip$probInt[1,],
                  ymax = suckerOcc_precip$probInt[2,],
                  x = precipSeq_backTran), alpha = 0.5, fill = "grey80") +
  
  # seedling prediction at low sucker density
  geom_line(aes(y = mean_low, x = precipSeq_backTran)) + 
  geom_ribbon(aes(ymin = probInt_low[1,],
                  ymax = probInt_low[2,],
                  x = precipSeq_backTran), alpha = 0.5, fill = "red") +
  
  # seedling prediction at high sucker density
  geom_line(aes(y = mean_high, x = precipSeq_backTran)) + 
  geom_ribbon(aes(ymin = probInt_high[1,],
                  ymax = probInt_high[2,],
                  x = precipSeq_backTran), alpha = 0.25, fill = "red") + 
  ylim(0, 1) + 
  labs(x = "1981–2010 mean annual precipitation (mm)",
       y = "Occupancy probability") +
  theme_classic()







# SUPPLEMENTAL 3 ------
burnPlot <- ggplot() + 
  geom_line(aes(y = seedlingOcc_burn$mean, x = burnSeq_backTran)) + 
  geom_ribbon(aes(ymin = seedlingOcc_burn$probInt[1,],
                  ymax = seedlingOcc_burn$probInt[2,],
                  x = burnSeq_backTran), alpha = 0.5, fill = "red") + 
  labs(y = "Occupancy probability", x = "% burned") + 
  ylim(c(0, max(seedlingOcc_burn$probInt[2,]))) + 
  theme_classic()


slopePlot <- ggplot() + 
  geom_line(aes(y = seedlingOcc_slope$mean, x = slopeSeq_backTran)) + 
  geom_ribbon(aes(ymin = seedlingOcc_slope$probInt[1,],
                  ymax = seedlingOcc_slope$probInt[2,],
                  x = slopeSeq_backTran), alpha = 0.5, fill = "red") + 
  labs(y = "Occupancy probability", x = "Slope (degrees)") + 
  ylim(c(0, max(seedlingOcc_slope$probInt[2,]))) + 
  theme_classic()

canCovPlot <- ggplot() + 
  geom_line(aes(y = seedlingOcc_canCov$mean, x = canCovSeq_backTran)) + 
  geom_ribbon(aes(ymin = seedlingOcc_canCov$probInt[1,],
                  ymax = seedlingOcc_canCov$probInt[2,],
                  x = canCovSeq_backTran), alpha = 0.5, fill = "red") + 
  labs(y = "Occupancy probability", x = "Canopy Cover %") + 
  ylim(c(0, max(seedlingOcc_canCov$probInt[2,]))) + 
  theme_classic()

aspenDistPlot <- ggplot() + 
  geom_line(aes(y = seedlingOcc_aspenDist$mean, x = aspenDistSeq_backTran)) + 
  geom_ribbon(aes(ymin = seedlingOcc_aspenDist$probInt[1,],
                  ymax = seedlingOcc_aspenDist$probInt[2,],
                  x = aspenDistSeq_backTran), alpha = 0.5, fill = "red") + 
  labs(y = "Occupancy probability", x = "Distance to live aspen (m)") + 
  ylim(c(0, max(seedlingOcc_aspenDist$probInt[2,]))) + 
  theme_classic()

aspectPlot <- ggplot() + 
  geom_line(aes(y = seedlingOcc_aspect$mean, x = aspectSeq_backTran)) + 
  geom_ribbon(aes(ymin = seedlingOcc_aspect$probInt[1,],
                  ymax = seedlingOcc_aspect$probInt[2,],
                  x = aspectSeq_backTran), alpha = 0.5, fill = "red") + 
  labs(y = "Occupancy probability", x = "Aspect (sine-transformed)") + 
  ylim(c(0, max(seedlingOcc_aspect$probInt[2,]))) + 
  theme_classic()

suckerDensPlot <- ggplot() + 
  geom_line(aes(y = seedlingOcc_suckerDens$mean, x = suckerDensSeq_backTran)) + 
  geom_ribbon(aes(ymin = seedlingOcc_suckerDens$probInt[1,],
                  ymax = seedlingOcc_suckerDens$probInt[2,],
                  x = suckerDensSeq_backTran), alpha = 0.5, fill = "red") + 
  labs(x = "Sucker density %",
       y = "Occupancy probability") +
  ylim(c(0, max(seedlingOcc_suckerDens$probInt[2,]))) + 
  theme_classic()

library(cowplot)

plot_grid(suckerDensPlot,
          burnPlot,
          aspectPlot,
          aspenDistPlot,
          slopePlot, 
          canCovPlot,
          ncol = 2)







 