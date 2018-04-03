rm(list=ls())
library(sf)
library(tidyverse)
library(viridis)
library(rvest)
library(ggplot2)
library(cartogram)
library(lwgeom)

library(rgdal)
library(sp)
library(rgeos)

library(Rcartogram)
library(getcartr)


setwd("~/projects/deep-sea fisheries/")
pop <- readr::read_csv2("200-400/data/shapefiles/EWR201512E_Matrix.csv")
berlin <- st_read("200-400/data/shapefiles/RBS_OD_LOR_2015_12/RBS_OD_LOR_2015_12.shp") %>%
  #st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  mutate(area=as.numeric(st_area(geometry))/(1e6)) %>%
  left_join(pop,by=c("PLR"="RAUMID")) %>%
  group_by(PGRNAME) %>%
  summarise(area=sum(area),population=sum(E_E)) %>%
  mutate(centroid=st_centroid(geometry)) %>%
  mutate(density=population/area) %>%
  st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
  mutate(centroid=st_transform(centroid,crs = "+proj=longlat +datum=WGS84")) %>% 
  mutate(label_x = sapply(centroid, "[[", 1), label_y = sapply(centroid, "[[", 2))

carto.1 <- st_as_sf(cartogram::cartogram(as(berlin,'Spatial'),'density',itermax = 25))
carto.2 <- st_as_sf(getcartr::quick.carto(as(berlin,'Spatial'),v=berlin$density,res=256))

ggplot(carto.1) +
  geom_sf(aes(fill=density)) + 
    scale_fill_viridis(name="Density",labels=scales::comma,breaks=pretty(carto.1$density,n=3)) +
  #geom_label(aes(x=label_x,y=label_y,label=PGRNAME)) +
  theme_minimal()

ggplot(carto.2) +
  geom_sf(aes(fill=density)) + 
  scale_fill_viridis(name="Density",labels=scales::comma,breaks=pretty(carto.2$density,n=3)) +
  #geom_label(aes(x=label_x,y=label_y,label=PGRNAME)) +
  theme_minimal()

