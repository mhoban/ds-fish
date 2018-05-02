rm(list=ls())
library(tidyverse)
library(stringr)

setwd("~/projects/deep-sea fisheries/200-400/")
#species <- readxl::read_excel("data/200-400 m data and paper/DeepSpeciesList_2.xlsx",sheet = "SpeciesList")
species <- read_csv("data/species.csv")
countries <- read_csv("data/ref_country.csv")


fao <- readr::read_csv("data/merge/tsd_global_production_long.csv.gz",guess_max = 100000) %>% 
  mutate(source="fao") %>%
  #mutate(reporting_status="Reported") %>%
  #rename(common_name=species) %>%
  select(country,country_iso,country_un,fishing_area,year,common_name,scientific_name,reporting_status,catch,source) %>%
  #filter(scientific_name %in% species$scientific.name) %>% keep the whole business
  mutate_if(is.character,as.factor) %>%
  arrange(year,country,common_name) %>%
  droplevels()


## make sure to sum "unreported" by each sub type of unreported

sau <- read_csv("data/merge/Mykle_eez_v47.csv", guess_max=100000) %>%
  mutate(source="sau") %>%
  rename(country=fishing_entity) %>%
  rename(catch=tonnes) %>%
  #filter(scientific_name %in% species$scientific.name) %>% no filter species
  left_join(countries,by=c('country'='name_en')) %>%
  rename(country_un=un_code) %>%
  rename(country_iso=iso_3_code) %>%
  select(country,country_iso,country_un,fao_area,year,common_name,scientific_name,reporting_status,catch,source) %>%
  rename(fishing_area=fao_area) %>%
  mutate_if(is.character,as.factor) %>%
  arrange(year,country,common_name) %>%
  droplevels()

highseas <- read_csv("data/merge/Les_eez_hs_04-10-2018_v47.csv",guess_max=100000) %>%
  mutate(source="sau") %>%
  #mutate(common_name="Argentines") %>%
  rename(country=fishing_entity) %>%
  rename(catch=tonnes) %>%
  #filter(scientific_name %in% species$scientific.name) %>%
  left_join(countries,by=c('country'='name_en')) %>%
  left_join(species,by=c('scientific_name'='scientific.name')) %>%
  rename(common_name=common.name) %>%
  rename(country_un=un_code) %>%
  rename(country_iso=iso_3_code) %>%
  select(country,country_iso,country_un,fao_area,year,common_name,scientific_name,reporting_status,catch,source) %>%
  rename(fishing_area=fao_area) %>%
  mutate_if(is.character,as.factor) %>%
  arrange(year,country,common_name) %>%
  droplevels()

argentines <- read_csv("data/merge/Les_GenusArgentina_v47.csv",guess_max=100000) %>%
  mutate(source="sau") %>%
  mutate(common_name="Argentine") %>%
  rename(country=fishing_entity) %>%
  rename(catch=tonnes) %>%
  #filter(scientific_name %in% species$scientific.name) %>%
  left_join(countries,by=c('country'='name_en')) %>%
  #left_join(species,by=c('scientific_name'='scientific.name')) %>%
  #rename(common_name=common.name) %>%
  rename(country_un=un_code) %>%
  rename(country_iso=iso_3_code) %>%
  select(country,country_iso,country_un,fao_area,year,common_name,scientific_name,reporting_status,catch,source) %>%
  rename(fishing_area=fao_area) %>%
  mutate_if(is.character,as.factor) %>%
  arrange(year,country,common_name) %>%
  droplevels()


#sau.n <- readr::read_csv("data/sau/sau-data-fixed.csv", guess_max=100000)
# sau <- readr::read_csv("data/sau/sau-data-fixed.csv.gz", guess_max=100000) %>% 
#   mutate(source="sau") %>%
#   #mutate(country_code=NA) %>%
#   rename(country=fishing_entity) %>%
#   rename(catch=tonnes) %>%
#   filter(scientific_name %in% species$scientific.name) %>% 
#   left_join(countries,by=c('country'='name_en')) %>%
#   rename(country_un=un_code) %>%
#   rename(country_iso=iso_3_code) %>%
#   select(country,country_iso,country_un,area_name,year,common_name,scientific_name,reporting_status,catch,source) %>%
#   rename(fishing_area=area_name) %>%
#   mutate_if(is.character,as.factor) %>%
#   arrange(year,country,common_name) %>%
#   droplevels()

all_data <- bind_rows(fao,sau,highseas,argentines) %>%
  mutate_if(is.character,as.factor) %>%
  arrange(year,country,common_name)

#all_data_filtered <- all_data[!duplicated(all_data[,c('scientific_name','year','reporting_status','catch','fishing_area','country')]),]
all_data_filtered <- all_data %>% filter(!duplicated(all_data))

write_excel_csv(all_data,"data/fao-sau-combined.csv",col_names = T)

fao <- all_data %>% filter(source == 'fao') %>% droplevels()
sau <- all_data %>% filter(source == 'sau') %>% droplevels()



fao.areas <- levels(fao$fishing_area)
sau.areas <- levels(sau$fishing_area)
fao.only.areas <- fao.areas[!(fao.areas %in% sau.areas)]
sau.only.areas <- sau.areas[!(sau.areas %in% fao.areas)]

fao.countries <- levels(fao$country)
sau.countries <- levels(sau$country)
#sau.countries <- levels(factor(fixed_countries))
sau.only.countries <- sau.countries[!(sau.countries %in% fao.countries)]
fao.only.countries <- fao.countries[!(fao.countries %in% sau.countries)]

fao.species <- levels(fao$scientific_name)
sau.species <- levels(sau$scientific_name)
sau.only.species <- sau.species[!(sau.species %in% fao.species)]
fao.only.species <- fao.species[!(fao.species %in% sau.species)]

fao.common <- levels(fao$common_name)
sau.common <- levels(sau$common_name)
sau.only.common <- sau.common[!(sau.common %in% fao.common)]
fao.only.common <- fao.common[!(fao.common %in% sau.common)]
# 
# View(sau.only.species)
# View(fao.species)
# View(sau.species)

  
