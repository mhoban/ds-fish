rm(list=ls())
library(tidyverse)
library(stringr)


### Sync SAU and FAO data
## Steps:
## 1. run sau-to-fao.sed on SAU .csv files (this synchronizes country names)
## $ sed -i'.backup' -fsau-to-fao.sed 'Mykle_v2_v47.csv'
## 2. run this script

setwd("~/projects/deep-sea fisheries/200-400/")
species_translate <- read_csv("data/merge/realmerge/species-translate.csv")
countries <- read_csv("data/ref_country.csv")

## Read entire FAO database (every entry for production type 'capture')
fao <- read_csv("data/merge/realmerge/tsd_global_production_long.csv.gz",guess_max = 100000) %>% 
  filter(scientific_name %in% species_translate$scientific_name_fao) %>%
  mutate(source="fao") %>% # designate source as FAO
  rename(fao_area=fishing_area) %>%
  select(country,country_iso,country_un,fao_area,year,common_name,scientific_name,reporting_status,catch,source) %>%
  arrange(year,country,common_name) # sort table

## Read, calculate, and format SAU dataset

# this file contains all the latest SAU data for our class species list
sau <- read_csv("data/merge/realmerge/Mykle_v2_v47.csv") %>%
  left_join(species_translate,by=c("scientific_name"="scientific_name_sau")) %>%
  left_join(countries,by=c("fishing_entity"="name_en")) %>%
  mutate(source="sau") %>%
  rename(country=fishing_entity) %>%
  rename(country_un=un_code) %>%
  rename(country_iso=iso_3_code) %>%
  select(country,country_iso,country_un,fao_area,year,common_name_fao,scientific_name_fao,reporting_status,source,gear,tonnes) %>%
  rename(common_name=common_name_fao) %>%
  rename(scientific_name=scientific_name_fao) %>%
  group_by(country,country_iso,country_un,fao_area,year,common_name,scientific_name,reporting_status,source) %>%
  summarise(catch=sum(tonnes)) %>% # sum catch across gear types
  ungroup() %>%
  select(country,country_iso,country_un,fao_area,year,common_name,scientific_name,reporting_status,catch,source) %>%
  arrange(year,country,common_name) # sort table


all_data <- union(fao,sau) %>%
  arrange(source,year,country,common_name,reporting_status)

write_excel_csv(all_data,"data/merge/realmerge/fao-sau-combined.csv",col_names = T)
