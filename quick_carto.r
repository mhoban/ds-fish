rm(list=ls())
library(cartogram)
library(maptools)
library(tidyverse)
library(sf)
library(parallel)
library(Rcartogram)
library(getcartr)
library(rgeos)
library(rmapshaper)


setwd("~/projects/deep-sea fisheries/")
suppressWarnings(fao.data <- read_csv("200-400/data/fao-data-new.csv.gz"))

data("wrld_simpl")
world_map <- st_as_sf(wrld_simpl) %>%
  st_transform(crs="+proj=eqc +ellps=WGS84 +datum=WGS84 +no_defs") %>%
  filter(UN != 10 & UN != 260 & REGION != 10)# %>%
  #st_as_sf(ms_simplify(as(.,'Spatial'),keep=0.3,keep_shapes=T))
world_map <- st_as_sf(ms_simplify(as(world_map,'Spatial'),keep=0.3,keep_shapes=T))

if (!exists("framesfile")) {
  framesfile <- "cartogram_frames.rdata"
}
cat(paste("Saving to",framesfile))

#filter_year <- 1972

frames <- mclapply(seq(min(fao.data$year),max(fao.data$year),by=5),function(filter_year) {
  cat(paste("this year:",filter_year,"\n"))
  fish.year <- fao.data %>% 
    group_by(country.code,year) %>%
    summarise(total.catch = sum(catch)) %>%
    filter(year == filter_year) %>%
    filter(country.code != '')  
  
  world_combined <- 
    left_join(world_map,fish.year, by=c("ISO3"="country.code")) %>% 
    mutate_if(is.numeric,funs(replace(.,is.na(.),0))) #%>%
    #filter(UN != 10 & UN != 260 & REGION != 10) %>%
    #st_transform(crs="+init=epsg:3857")

  if (exists('filter_region')) {
    world_combined <- world_combined %>% filter(REGION==filter_region)
  }
  if (exists('filter_subregion')) {
    world_combined <- world_combined %>% filter(SUBREGION==filter_subregion)
  }
  fish.carto <- st_as_sf(quick.carto(as(world_combined,'Spatial'),world_combined$total.catch+1))
  #ptm <- proc.time()
  #fish.cartogram <- st_as_sf(cartogram::cartogram(as(world_combined,'Spatial'), "total.catch",itermax=iterations)) %>% mutate(year=year)
  #proc.time() - ptm
  (fish.carto)
},mc.preschedule=T, mc.cores=4)

save(frames,file=framesfile)
