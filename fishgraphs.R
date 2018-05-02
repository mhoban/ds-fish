# generate figures for 200-400m species
# start by clearing the workspace and getting us where we want to be
if (!exists('loaded')) {
  rm(list=ls())  
}


# load these things
library(tidyverse)
#library(ggplot2)
library(scales)
library(ggsci)

# set working directory
setwd("~/projects/deep-sea fisheries/200-400/data/")

# load and filter species table
my.species <- read.csv("species.csv") %>% filter(owner=='mykle') %>% droplevels()


##### LOAD DATA ########
all.data <- read_csv("200-400/data/fao-sau-combined.csv",guess_max = 100000) %>%
  filter(scientific_name %in% my.species$scientific.name) %>%
  droplevels()

# aggretate and summarize dataset
combined <- all.data %>%
  group_by(year,common_name,country,source) %>%
  summarise(total.catch=sum(catch)) %>%
  ungroup() %>%
  arrange(year,common_name) %>%
  mutate_if(is.character,as.factor)

if (!exists('dontplot') | dontplot == F)
{
  ## plot stacked bar graphs of country by year for each species
  # create a labeller function so to display source categories better
  source.label = as_labeller(c('fao' = 'Total Catch (FAO)', 'sau' = 'Total Catch (SAU+FAO)'))
  # plot catch by country for each species. in RStudio you can cycle through the different plots
  # country.plotz will be a list of each plot object
  country.plotz <- map(levels(combined$common_name),function(taxon) {
    plotr <- ggplot(combined %>% filter(common_name == taxon), aes(x=year,y=total.catch,fill=country)) +
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
  # pdf("200-400/plots/country-year.pdf",paper='USr')
  # walk(country.plotz,function(plotr) {
  #   print(plotr)
  # })
  # dev.off()
  
  # plot step graphs per species
  # as above, step.plotz will be a list of each plot object
  
  # summarize global catch totals per year (for all countries)
  global.catch <- combined %>% 
    group_by(year,common_name,source) %>% 
    summarise(gross.catch=sum(total.catch)) %>% 
    ungroup()
  step.plotz <- map(levels(global.catch$common_name),function(taxon) {
    plotr <- ggplot(global.catch %>% filter(common_name == taxon), aes(x=year,y=gross.catch,color=source)) +
      geom_step() +
      theme_classic() +
      #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_color_igv(name='Data Source',breaks=c('fao','sau'),labels=c('FAO','FAO+SAU')) +
      scale_x_continuous(name="Year",breaks=pretty(global.catch$year,n=10)) +
      scale_y_continuous(name="Total Catch",labels=comma) +
      ggtitle(taxon)
    cat(taxon,"\n")
    print(plotr)
    return(plotr)
  })
  
  # uncomment to create a multi-page PDF with one plot on each page
  # pdf("200-400/plots/species-total.pdf",paper='USr')
  # walk(step.plotz,function(plotr) {
  #   print(plotr)
  # })
  # dev.off()
}
