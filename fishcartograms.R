rm(list=ls())
library(cartogram)
library(maptools)
library(tidyverse)
library(sf)
library(gganimate)
#library(rgdal)

source("~/code/r.scripts/library/mygganimate.r")
setwd("~/projects/deep-sea fisheries/")

dontplot <- T
loaded <- T
#source("200-400/fishgraphs.R")
#fao.data2 <- read.csv("200-400/data/fao-data-new.csv")
fao.data <- readr::read_csv("200-400/data/fao-data-new.csv")

world_map <- st_read("200-400/data/shapefiles/TM_WORLD_BORDERS_SIMPL-0.3/TM_WORLD_BORDERS_SIMPL-0.3.shp")

timeseq <- 1
frames <- do.call('rbind',lapply(seq(min(fao.data$year),max(fao.data$year),by=15),function(year) {
  cat("year\n")
  fish.year <- fao.data %>% 
    filter(year==year) %>%
    group_by(country.code) %>%
    summarise(total.catch = sum(catch)) %>%
    ungroup() %>%
    filter(country.code != '')  
  
  #combined.levels <- sort(union(levels(world_map$ISO3),levels(fish.year$country.code)))
  world_combined <- 
    left_join(world_map,fish.year, by=c("ISO3"="country.code")) %>% 
    mutate_if(is.numeric,funs(replace(.,is.na(.),0))) #%>%
    #filter(REGION==2)
  fish.cartogram <- st_as_sf(cartogram::cartogram(as(world_combined,'Spatial'), "total.catch",itermax=100)) %>% mutate(year=year)
  
  rownames(world_combined) <- world_combined$ISO3
  wc <- as(world_combined,'Spatial')
  wc_df <- tidy(wc) %>% left_join(wc@data,by=c('id'='ISO3'))
  wc_df$id <- seq(1,nrow(wc_df))
  wc_df$year <- year-1
  
  rownames(fish.cartogram) <- fish.cartogram$ISO3
  fc <- as(fish.cartogram,'Spatial')
  fc_df <- tidy(fc) %>% left_join(fc@data,by=c('id'='ISO3'))
  fc_df$id <- seq(1,nrow(fc_df))
  #fc_df$time <- timeseq
  #timeseq=timeseq+1
  (rbind(wc_df,fc_df))
}))

library(tweenr)
frames$ease <- 'cubic-in-out'
tw <- tween_elements(frames,time='year',group='id',ease='ease',nframes=100) %>%
  mutate(year=round(year))

p <- ggplot(tw %>% arrange(order) ,aes(frame=.frame, ttl=year)) +
  geom_polygon(aes(fill = total.catch, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name="Total Catch (t)", breaks=pretty(tw$total.catch)) +
  coord_map()

#animation::ani.options(interval = 1/9)
mygg_animate(p, title_frame =T, interval=1/9)







rownames(world_combined) <- world_combined$ISO3
wc <- as(world_combined,'Spatial')
wc_df <- tidy(wc) %>% left_join(wc@data,by=c('id'='ISO3'))

rownames(fish.cartogram) <- fish.cartogram$ISO3
fc <- as(fish.cartogram,'Spatial')
fc_df <- tidy(fc) %>% left_join(fc@data,by=c('id'='ISO3'))

wc_df$id <- seq(1,nrow(wc_df))
fc_df$id <- seq(1,nrow(fc_df))

both <- rbind(wc_df,fc_df,wc_df)
both$ease <- "cubic-in-out"
both$time <- rep(c(1:3),each=nrow(wc_df))

#both <- both %>% 
#  dplyr::select(long,lat,group,ease,time,total.catch,LON,LAT)

tw <- tween_elements(both,time='time',group='id',ease='ease',nframes=30)

# rownames(europe@data) <- as.character(europe$ISO3)
# rownames(w@data) <- as.character(w$ISO3)

#europe <- spTransform(europe,CRS('+init=epsg:3395'))

fish.cartogram <- st_as_sf(cartogram::cartogram(as(world_combined,'Spatial'), "total.catch",itermax=50))

# ggplot(world_combined) +
#   geom_sf(aes(fill=total.catch)) + 
#   scale_fill_viridis(name="Total Catch",labels=scales::comma,breaks=pretty(world_combined$total.catch)) +
#   #geom_label(aes(x=label_x,y=label_y,label=PGRNAME)) +
#   theme_minimal()
# 
# ggplot(fish.cartogram) +
#   geom_sf(aes(fill=total.catch)) + 
#   scale_fill_viridis(name="Total Catch",labels=scales::comma,breaks=pretty(fish.cartogram$total.catch)) +
#   #geom_label(aes(x=label_x,y=label_y,label=PGRNAME)) +
#   theme_minimal()



library(gganimate)

p <- ggplot(tw %>% arrange(order) ,aes(frame=.frame)) +
  geom_polygon(aes(fill = total.catch, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name="Total Catch (t)", breaks=pretty(tw$total.catch)) +
  ggtitle("Asia catch:") +
  #labs( title = "Asia", subtitle=paste("Catch per country in",year) ) +
  #ylim(-35,35) +
  # theme(
  #   text = element_text(color = "#22211d"), 
  #   plot.background = element_rect(fill = "#f5f5f4", color = NA), 
  #   panel.background = element_rect(fill = "#f5f5f4", color = NA), 
  #   legend.background = element_rect(fill = "#f5f5f4", color = NA),
  #   plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  #   plot.subtitle = element_text(size= 13, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
  #   legend.position = c(0.2, 0.26)
  # ) +
coord_map()

animation::ani.options(interval = 1/9)
gganimate(p, "200-400/data/shapefiles/asia_animate.gif",  title_frame = F)

# plot various stages in the animation
# ggplot() + geom_polygon(data = tw %>% filter(.frame==0) %>% arrange(order), aes(fill = total.catch, x = long, y = lat, group = group) , size=0, alpha=0.9)
# ggplot() + geom_polygon(data = tw %>% filter(.frame==5) %>% arrange(order), aes(fill = total.catch, x = long, y = lat, group = group) , size=0, alpha=0.9)
# ggplot() + geom_polygon(data = tw %>% filter(.frame==10) %>% arrange(order), aes(fill = total.catch, x = long, y = lat, group = group) , size=0, alpha=0.9)
# ggplot() + geom_polygon(data = tw %>% filter(.frame==15) %>% arrange(order), aes(fill = total.catch, x = long, y = lat, group = group) , size=0, alpha=0.9)
# ggplot() + geom_polygon(data = tw %>% filter(.frame==20) %>% arrange(order), aes(fill = total.catch, x = long, y = lat, group = group) , size=0, alpha=0.9)
# ggplot() + geom_polygon(data = tw %>% filter(.frame==25) %>% arrange(order), aes(fill = total.catch, x = long, y = lat, group = group) , size=0, alpha=0.9)
# ggplot() + geom_polygon(data = tw %>% filter(.frame==30) %>% arrange(order), aes(fill = total.catch, x = long, y = lat, group = group) , size=0, alpha=0.9)





# 
# ggplot() +
#   geom_polygon(data = spdf_fortified, aes(fill = total.catch/1000, x = long, y = lat, group = group) , size=0, alpha=0.9) +
#   theme_void() +
#   scale_fill_viridis(name="Total catch (1k tonnes)", breaks=pretty(afr_cartogram$total.catch/1000), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1)) +
#   labs( title = "Euro-catch" ) +
#   #ylim(-35,35) +
#   theme(
#     text = element_text(color = "#22211d"), 
#     plot.background = element_rect(fill = "#f5f5f4", color = NA), 
#     panel.background = element_rect(fill = "#f5f5f4", color = NA), 
#     legend.background = element_rect(fill = "#f5f5f4", color = NA),
#     plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
#     #legend.position = c(3.57,38.87)
#     legend.justification = c('center'),
#     legend.position = c(0.26,0.95)
#   ) +
#   coord_map()
# 
# afr_cartogram <- w
# spdf_fortified <- tidy(afr_cartogram)
# spdf_fortified = spdf_fortified %>% left_join(. , afr_cartogram@data, by=c("id"="ISO3")) 
# 
# ggplot() +
#   geom_polygon(data = spdf_fortified, aes(fill = total.catch/1000, x = long, y = lat, group = group) , size=0, alpha=0.9) +
#   theme_void() +
#   scale_fill_viridis(name="Total catch (1k tonnes)", breaks=pretty(afr_cartogram$total.catch/1000), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1)) +
#   labs( title = "Euro-catch" ) +
#   #ylim(-35,35) +
#   theme(
#     text = element_text(color = "#22211d"), 
#     plot.background = element_rect(fill = "#f5f5f4", color = NA), 
#     panel.background = element_rect(fill = "#f5f5f4", color = NA), 
#     legend.background = element_rect(fill = "#f5f5f4", color = NA),
#     plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
#     #legend.position = c(3.57,38.87)
#     legend.justification = c('center'),
#     legend.position = c(0.26,0.95)
#   ) +
#   coord_map()

# 
# data(wrld_simpl)
# afr=wrld_simpl[wrld_simpl$REGION==2,]
# 
# # We can visualize the region's boundaries with the plot function
# plot(afr)
# 
# # construct a cartogram using the population in 2005
# afr_cartogram <- cartogram(afr, "POP2005", itermax=5)
# 
# # This is a new geospatial object, we can visualise it!
# plot(afr_cartogram)
# 
# # It is a new geospatial object: we can use all the usual techniques on it! Let's start with a basic ggplot2 chloropleth map:
# 
# spdf_fortified <- tidy(afr_cartogram)
# spdf_fortified = spdf_fortified %>% left_join(. , afr_cartogram@data, by=c("id"="ISO3")) 
# ggplot() +
#   geom_polygon(data = spdf_fortified, aes(fill = POP2005, x = long, y = lat, group = group) , size=0, alpha=0.9) +
#   coord_map() +
#   theme_void()
# 
# # As seen before, we can do better with a bit of customization
# 
# ggplot() +
#   geom_polygon(data = spdf_fortified, aes(fill = POP2005/1000000, x = long, y = lat, group = group) , size=0, alpha=0.9) +
#   theme_void() +
#   scale_fill_viridis(name="Population (M)", breaks=c(1,50,100, 140), guide = guide_legend( keyheight = unit(3, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1)) +
#   labs( title = "Africa 2005 Population" ) +
#   ylim(-35,35) +
#   theme(
#     text = element_text(color = "#22211d"), 
#     plot.background = element_rect(fill = "#f5f5f4", color = NA), 
#     panel.background = element_rect(fill = "#f5f5f4", color = NA), 
#     legend.background = element_rect(fill = "#f5f5f4", color = NA),
#     plot.title = element_text(size= 22, hjust=0.5, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
#     legend.position = c(0.2, 0.26)
#   ) +
#   coord_map()
# 
ptm <- proc.time()
fish.cartogram <- st_as_sf(cartogram::cartogram(as(world_combined,'Spatial'), "total.catch",itermax=15)) %>% mutate(year=year)
proc.time() - ptm
