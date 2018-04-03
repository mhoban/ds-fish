rm(list=ls())
library(tweenr)
library(tidyverse)
library(sf)
library(broom)
library(gganimate)
library(viridis)
library(maptools)


data('wrld_simpl')

world_map <- st_as_sf(wrld_simpl) %>% 
  filter(UN != 10 & UN != 260 & REGION != 10) %>%
  st_transform(crs="+init=epsg:3857") %>%
  st_simplify()

setwd("~/projects/deep-sea fisheries/")
load("200-400/data/quick_cartos.rdata")

frames <- modify(frames,function(frame) {
  st_crs(frame) <- "+init=epsg:3857"
  ( frame )
})

#st_crs(frames[[1]]) <- "+init=epsg:3857"
#st_crs(frames[[10]]) <- "+init=epsg:3857"

f0 <- world_map %>%
  st_transform(crs="+proj=longlat +datum=WGS84") %>%
  mutate(year=0) %>%
  mutate(total.catch=0)

f1 <- frames[[1]] %>% 
  mutate_if(is.character,as.factor) %>% 
  filter(year != 0) %>%
  st_transform(crs="+proj=longlat +datum=WGS84")
f2 <- frames[[10]] %>% 
  mutate_if(is.character,as.factor) %>% 
  filter(year != 0) %>%
  st_transform(crs="+proj=longlat +datum=WGS84")


rownames(f0) <- f0$ISO3
rownames(f1) <- f1$ISO3 
rownames(f2) <- f2$ISO3

f0_tidy <- tidy(as(f0,'Spatial')) %>%
  left_join(f0,by=c("id"="ISO3")) %>%
  rename(ISO3=id) %>%
  mutate(obs=1:nrow(.)) %>% 
  mutate(year=0) %>%
  select(-geometry)
f1_tidy <- tidy(as(f1,'Spatial')) %>% 
  left_join(f1,by=c("id"="ISO3")) %>% 
  rename(ISO3=id) %>%
  mutate(obs=1:nrow(.)) %>%
  select(-geometry)
f2_tidy <- tidy(as(f2,'Spatial')) %>%
  left_join(f2,by=c("id"="ISO3")) %>% 
  rename(ISO3=id) %>%
  mutate(obs=1:nrow(.)) %>%
  select(-geometry)

# tidies <- list(f1_tidy %>% select(-ISO3),f2_tidy %>% select(-ISO3))
# 
# tween.states <- tween_states(tidies,tweenlength = 1, statelength = 1,ease='cubic-in-out',nframes=30)
# 
# p <- ggplot(tween.states, aes(frame=.frame)) +
#   geom_polygon(aes(fill=total.catch+1,x=long,y=lat,group=group),size=0,alpha=0.9) +
#   scale_fill_viridis("catcher",breaks=pretty(tween.states$total.catch)) +
#   coord_map()
# gganimate(p)

toodies <- bind_rows(f1_tidy,f2_tidy)
#toodies$id <- seq(1,nrow(f1_tidy))
toodies$ease <- "cubic-in-out"
#toodies$time <- c(1:nrow(toodies))

tween.elements <- tween_elements(toodies %>% select(-ISO3),time="year",group="obs",ease="ease",nframes=30) %>%
  mutate(year=round(year))

#p <- ggplot(tween.elements %>% arrange(order), aes(frame=.frame)) +
p <- ggplot(tween.elements, aes(frame=.frame)) +
  geom_polygon(aes(fill=total.catch+1,x=long,y=lat,group=group),size=0,alpha=0.9) +
  scale_fill_viridis("catcher",breaks=pretty(tween.elements$total.catch)) + 
  coord_map()
gganimate(p, interval=1/9)
