rm(list=ls())
library(tidyverse)

setwd("~/projects/deep-sea fisheries/200-400/")
#species <- readxl::read_excel("data/200-400 m data and paper/DeepSpeciesList_2.xlsx",sheet = "SpeciesList")
species <- read_csv("data/species.csv")
fao <- readr::read_csv("data/tsd_global_production_long.csv.gz",guess_max = 100000) %>% 
  mutate_if(is.character,as.factor) %>%
  mutate(source="fao") %>%
  mutate(reporting_status="Reported") %>%
  rename(common_name=species) %>%
  select(country,year,common_name,scientific_name,reporting_status,catch,source) %>%
  filter(scientific_name %in% species$scientific.name) %>%
  droplevels()

#sau.n <- readr::read_csv("data/sau/sau-data-fixed.csv", guess_max=100000)
sau <- readr::read_csv("data/sau/sau-data-fixed.csv", guess_max=100000) %>% 
  mutate_if(is.character,as.factor) %>%
  mutate(source="sau") %>%
  rename(country=fishing_entity) %>%
  rename(catch=tonnes) %>%
  select(country,year,common_name,scientific_name,reporting_status,catch,source) %>%
  filter(scientific_name %in% species$scientific.name) %>% 
  droplevels()

sumz <- sau %>% group_by(country,year,scientific_name,reporting_status) %>% summarise(total_catch=sum(catch))
sumzer <- sumz %>% group_by(scientific_name,year) %>% summarise(total_catch=sum(total_catch))

all_data <- rbind(fao,sau)

readr::write_excel_csv(all_data,"data/fao-sau-combined.csv",col_names = T)
f <- readr::read_csv("data/fao-sau-combined.csv")

fao.countries <- levels(fao$country)
sau.countries <- levels(sau$country)
sau.only.countries <- sau.countries[!(sau.countries %in% fao.countries)]

fao.species <- levels(fao$scientific_name)
sau.species <- levels(sau$scientific_name)
sau.only.species <- sau.species[!(sau.species %in% fao.species)]

fao.common <- levels(fao$common_name)
sau.common <- levels(sau$common_name)
sau.only.common <- sau.common[!(sau.common %in% fao.common)]
# 
# View(sau.only.species)
# View(fao.species)
# View(sau.species)

  
