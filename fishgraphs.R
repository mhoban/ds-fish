# generate figures for 200-400m species
# start by clearing the workspace and getting us where we want to be
if (!exists('loaded')) {
  rm(list=ls())  
}


# load these things
library(ggsci)
library(tidyverse)
library(ggplot2)
library(scales)

# set working directory
setwd("~/projects/deep-sea fisheries/")

# load and filter species table
my.species <- read.csv("200-400/data/species.csv") %>% filter(owner=='mykle') %>% droplevels()


####### FAO DATA #######
# read and format FAO data
fao.data <- read.csv("200-400/data/fao-data.csv") %>%
  # convert from wide to long format
  gather("year","catch",-c(1:5)) %>% 
  # R puts an X before column names that are numbers, the next three lines
  # make sure that years are treated as years in numeric format
  separate(year,into=c("nothing","year"),"(?<=X)(?=[0-9])") %>%
  select(-nothing) %>%
  mutate(year=as.numeric(year))

# subset FAO data to species list
my.fao <- fao.data %>%
  filter(common.name %in% my.species$common.name) %>%
  # this adds in the missing data from the species table
  left_join(.,my.species,by="common.name") %>%
  droplevels() %>% 
  select(-owner) %>%
  mutate(common.name=factor(common.name))
  

####### SAU DATA #######
sau.data <- read.csv("200-400/data/sau-data.csv")
my.sau <- sau.data %>% 
  filter(common.name %in% my.species$common.name) %>%
  rename(country=fishing.entity) %>%
  droplevels() 
  


# sum total catch per species by year and country 
fao <- my.fao %>% 
  group_by(year,common.name,country) %>% 
  summarise(total.catch=sum(catch)) %>% 
  ungroup() %>% 
  arrange(year,common.name) %>% 
  mutate_if(is.factor,as.character)
sau <- my.sau %>% 
  group_by(year,common.name,country) %>% 
  summarise(total.catch=sum(tonnes)) %>% 
  ungroup() %>%
  arrange(year,common.name) %>% 
  mutate_if(is.factor,as.character)

# combine both catch datasets into one distinguished by a factor denoting its
# source (i.e. fao/sau) and rebuild all the factors at the end
combined <- full_join(fao,sau,by=c('year','common.name','country'),suffix=c('.fao','.sau')) %>% 
  mutate_all(funs(replace(., which(is.na(.)), 0))) %>%    # replace NAs with zeroes
  gather("source","catch",-c(1:3)) %>%                    # make into long format
  mutate_if(is.character,as.factor) %>%                   # rebuild factors from character vectors
  arrange(year,common.name,source)                        # sort the dataset by year, species, and data source)


if (!exists('dontplot') | dontplot == F)
{
  ## plot stacked bar graphs of country by year for each species
  # create a labeller function so to display source categories better
  source.label = as_labeller(c('total.catch.fao' = 'Total Catch (FAO)', 'total.catch.sau' = 'Total Catch (SAU)'))
  # plot catch by country for each species. in RStudio you can cycle through the different plots
  # country.plotz will be a list of each plot object
  country.plotz <- map(levels(combined$common.name),function(taxon) {
    plotr <- ggplot(combined %>% filter(common.name == taxon), aes(x=year,y=catch,fill=country)) +
      geom_col(color="black") +
      theme_minimal() +
      scale_y_continuous(name="Total Catch",labels=comma) + 
      scale_x_continuous(name="Year",breaks=pretty(combined$year,n=10)) +
      scale_fill_igv(name="Country") +
      facet_wrap(~source,nrow=2,ncol=1,labeller = source.label) + 
      ggtitle(taxon)
    cat(taxon,"\n")
    print(plotr)
    return(plotr)
  })
  
  # uncomment to create a multi-page PDF with one plot on each page
  pdf("200-400/plots/country-year.pdf",paper='USr')
  walk(country.plotz,function(plotr) {
    print(plotr)
  })
  dev.off()
  
  # plot step graphs per species
  # as above, step.plotz will be a list of each plot object
  global.catch <- combined %>% group_by(year,common.name,source) %>% summarise(total.catch=sum(catch)) %>% ungroup()
  step.plotz <- map(levels(global.catch$common.name),function(taxon) {
    plotr <- ggplot(global.catch %>% filter(common.name == taxon), aes(x=year,y=total.catch,color=source)) +
      geom_step() +
      theme_classic() +
      #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_color_igv(name='Data Source',breaks=c('total.catch.fao','total.catch.sau'),labels=c('FAO','FAO+SAUP')) +
      scale_x_continuous(name="Year",breaks=pretty(combined$year,n=10)) +
      scale_y_continuous(name="Total Catch",labels=comma) +
      ggtitle(taxon)
    cat(taxon,"\n")
    print(plotr)
    return(plotr)
  })
  
  # uncomment to create a multi-page PDF with one plot on each page
  pdf("200-400/plots/species-total.pdf",paper='USr')
  walk(step.plotz,function(plotr) {
    print(plotr)
  })
  dev.off()
}
