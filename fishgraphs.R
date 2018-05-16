# generate figures for 200-400m species
# start by clearing the workspace
rm(list=ls())  

# load these things
library(tidyverse)
library(scales)
library(ggsci)

# set working directory
setwd("~/projects/deep-sea fisheries/200-400/")

##### big old color palette ##########
# base palette of 269 colors (they're fairly diverse, so we should be able to show all the countries)
pal_269 <- c( "#000000", "#FFFF00", "#1CE6FF", "#FF34FF", "#FF4A46", "#008941", "#006FA6", "#A30059",
              "#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
              "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
              "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100",
              "#DDEFFF", "#000035", "#7B4F4B", "#A1C299", "#300018", "#0AA6D8", "#013349", "#00846F",
              "#372101", "#FFB500", "#C2FFED", "#A079BF", "#CC0744", "#C0B9B2", "#C2FF99", "#001E09",
              "#00489C", "#6F0062", "#0CBD66", "#EEC3FF", "#456D75", "#B77B68", "#7A87A1", "#788D66",
              "#885578", "#FAD09F", "#FF8A9A", "#D157A0", "#BEC459", "#456648", "#0086ED", "#886F4C",
              
              "#34362D", "#B4A8BD", "#00A6AA", "#452C2C", "#636375", "#A3C8C9", "#FF913F", "#938A81",
              "#575329", "#00FECF", "#B05B6F", "#8CD0FF", "#3B9700", "#04F757", "#C8A1A1", "#1E6E00",
              "#7900D7", "#A77500", "#6367A9", "#A05837", "#6B002C", "#772600", "#D790FF", "#9B9700",
              "#549E79", "#FFF69F", "#201625", "#72418F", "#BC23FF", "#99ADC0", "#3A2465", "#922329",
              "#5B4534", "#FDE8DC", "#404E55", "#0089A3", "#CB7E98", "#A4E804", "#324E72", "#6A3A4C",
              "#83AB58", "#001C1E", "#D1F7CE", "#004B28", "#C8D0F6", "#A3A489", "#806C66", "#222800",
              "#BF5650", "#E83000", "#66796D", "#DA007C", "#FF1A59", "#8ADBB4", "#1E0200", "#5B4E51",
              "#C895C5", "#320033", "#FF6832", "#66E1D3", "#CFCDAC", "#D0AC94", "#7ED379", "#012C58",
              
              "#7A7BFF", "#D68E01", "#353339", "#78AFA1", "#FEB2C6", "#75797C", "#837393", "#943A4D",
              "#B5F4FF", "#D2DCD5", "#9556BD", "#6A714A", "#001325", "#02525F", "#0AA3F7", "#E98176",
              "#DBD5DD", "#5EBCD1", "#3D4F44", "#7E6405", "#02684E", "#962B75", "#8D8546", "#9695C5",
              "#E773CE", "#D86A78", "#3E89BE", "#CA834E", "#518A87", "#5B113C", "#55813B", "#E704C4",
              "#00005F", "#A97399", "#4B8160", "#59738A", "#FF5DA7", "#F7C9BF", "#643127", "#513A01",
              "#6B94AA", "#51A058", "#A45B02", "#1D1702", "#E20027", "#E7AB63", "#4C6001", "#9C6966",
              "#64547B", "#97979E", "#006A66", "#391406", "#F4D749", "#0045D2", "#006C31", "#DDB6D0",
              "#7C6571", "#9FB2A4", "#00D891", "#15A08A", "#BC65E9", "#FFFFFE", "#C6DC99", "#203B3C",
              
              "#671190", "#6B3A64", "#F5E1FF", "#FFA0F2", "#CCAA35", "#374527", "#8BB400", "#797868",
              "#C6005A", "#3B000A", "#C86240", "#29607C", "#402334", "#7D5A44", "#CCB87C", "#B88183",
              "#AA5199", "#B5D6C3", "#A38469", "#9F94F0", "#A74571", "#B894A6", "#71BB8C", "#00B433",
              "#789EC9", "#6D80BA", "#953F00", "#5EFF03", "#E4FFFC", "#1BE177", "#BCB1E5", "#76912F",
              "#003109", "#0060CD", "#D20096", "#895563", "#29201D", "#5B3213", "#A76F42", "#89412E",
              "#1A3A2A", "#494B5A", "#A88C85", "#F4ABAA", "#A3F3AB", "#00C6C8", "#EA8B66", "#958A9F",
              "#BDC9D2", "#9FA064", "#BE4700", "#658188", "#83A485", "#453C23", "#47675D", "#3A3F00",
              "#061203", "#DFFB71", "#868E7E", "#98D058", "#6C8F7D", "#D7BFC2", "#3C3E6E", "#D83D66",
              
              "#2F5D9B", "#6C5E46", "#D25B88", "#5B656C", "#00B57F", "#545C46", "#866097", "#365D25",
              "#252F99", "#00CCFF", "#674E60", "#FC009C", "#92896B")
##### end of the color palette #####
  
# load and filter species table
my.species <- read.csv("data/species.csv") %>% filter(owner=='mykle') %>% droplevels()

# alternately, do this:
# my.species <- tibble(
#   scientific_name = c(
#     'Lophius gastrophysus',
#     'Lophius piscatorius',
#     'Lophius vaillanti',
#     'Lophius vomerinus',
#     'Genypterus blacodes',
#     'Genypterus capensis',
#     'Genypterus chilensis',
#     'Genypterus maculatus',
#     'Austroglossus microlepis',
#     'Lepidorhombus whiffiagonis',
#     'Capromimus abbreviatus'
#   ),
#   common_name = c(
#     'Blackfin goosefish',
#     'Angler',
#     'Shortspine African angler',
#     'Devil anglerfish',
#     'Pink cusk-eel',
#     'Kingklip',
#     'Red cusk-eel',
#     'Black cusk-eel',
#     'West coast sole',
#     'Megrim'
#   )
# )


##### LOAD DATA ########
# initial dataset load and filtering:
# subset on requested species list and remove SAU 'unreported' data points
all_data <- read_csv("code/datasync/fao-sau-combined-filtered-catchtype.csv",guess_max = 100000) %>%
  filter(scientific_name %in% my.species$scientific_name) %>% 
  filter(source == 'fao' | reporting_status == 'Unreported') %>% 
  rename(total_catch = catch) %>%
  mutate_if(is.character,as.factor) %>% 
  arrange(year,common_name,country,fao_area,source)
# notably, we don't drop unused factor levels, so levels(all_data$scientific_name) will 
# list all available species (before filtering). similarly, for countries and common names

# recalculate catch data by data source
# for source='fao', we retain the original fao data
# for source='sau', we sum fao + sau (unreported) data
catch_data <- bind_rows(
  sau=all_data %>%
    group_by(year,common_name,scientific_name,catch_type,country,fao_area) %>%
    summarise(total_catch=sum(total_catch)) %>% 
    ungroup(), #%>%
    #mutate(source="sau"),
  fao=all_data %>%  
    filter(source=='fao') %>%
    group_by(year,common_name,scientific_name,catch_type,country,fao_area) %>%
    summarise(total_catch=sum(total_catch)) %>%
    ungroup(),
  .id="source"
) %>%
  # push 'source' column to the last position:
  select(-source,source) %>% 
  # sort by year, common name, country, and data source:
  arrange(year,common_name,country,source) %>% 
  # make character fields into factor fields:
  mutate_if(is.character,as.factor) %>% 
  # reorder catch type factor levels so 'landings' appears before 'discards':
  mutate(catch_type=factor(catch_type,levels=rev(levels(catch_type)))) 

# Create named color palettes for color consistency in factor levels across figures
# countries
pal_countries <- pal_269[1:nlevels(all_data$country)]
names(pal_countries) <- levels(all_data$country)
# species
pal_spp <- pal_igv('default')(nlevels(all_data$scientific_name)) # by scientific name
names(pal_spp) <- levels(all_data$scientific_name)
pal_comm <- pal_igv('default')(nlevels(all_data$common_name)) # by common name
names(pal_comm) <- levels(all_data$common_name)


# create a labeller function to display source categories better
source_labeller = as_labeller(c('fao' = 'Total Catch (FAO)', 'sau' = 'Total Catch (SAU+FAO)'))
# this function chooses the line size based on the data source
size_chooser <- function(x) { ifelse(x=='fao',1.5,1) }


##### Step plots ######

# plot step graphs for each species. I plot the FAO line a bit thicker than the
# SAU line. This is because they overlap across a fair number of years and I
# want them both to be visible as above, step.plotz will be a list of each plot object

# summarize global catch totals per year (for all countries) for step plots
global.catch.step <- catch_data %>% 
  group_by(year,common_name,scientific_name,source) %>% 
  summarise(gross_catch=sum(total_catch)) %>% 
  ungroup()

step.plotz <- pmap(list(unique(as.character(global.catch.step$common_name)),unique(as.character(global.catch.step$scientific_name))),function(taxon,sciname) {
  plotdata <- global.catch.step %>% filter(common_name == taxon)
  plotr <- ggplot(plotdata, aes(x=year,y=gross_catch,color=source)) +
    geom_step(size=size_chooser(plotdata$source)) +
    theme_classic() +
    #theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    scale_color_igv(name='Data Source',breaks=c('fao','sau'),labels=c('FAO','FAO+SAU')) +
    scale_x_continuous(name="Year",breaks=pretty(plotdata$year,n=10)) +
    scale_y_continuous(name="Total Catch",labels=comma) +
    ggtitle(substitute(a~" ("~italic(b)~")",list(a=taxon,b=sciname))) + 
    theme(legend.position = "bottom")
  cat(taxon,"\n")
  print(plotr)
  return(plotr)
})

# # uncomment to create a multi-page PDF with one plot on each (landscape-oriented) page
# pdf("plots/species-total.pdf",paper='USr')
# walk(step.plotz,function(plotr) {
#   print(plotr)
# })
# dev.off()


###### Stacked bar plots by country ######
## plot stacked bar graphs of global per-species catch color coded by country

# plot catch by country for each species. in RStudio you can cycle through the different plots
# country.plotz will be a list of each plot object
country.plotz <- pmap(list(unique(as.character(catch_data$common_name)),unique(as.character(catch_data$scientific_name))),function(taxon,sciname) {
  plotr <- ggplot(catch_data %>% filter(common_name == taxon), aes(x=year,y=total_catch,fill=country)) +
    geom_col(color="black") +
    theme_minimal() +
    theme(panel.border = element_rect(colour = "black",fill = NA)) +
    scale_y_continuous(name="Total Catch",labels=comma) + 
    scale_x_continuous(name="Year",breaks=pretty(catch_data$year,n=10)) +
    scale_fill_manual(name="Country",values=pal_countries) + 
    facet_wrap(~source,nrow=2,ncol=1,labeller = source_labeller) + 
    ## uncomment the next line (and comment the previous one) to further break panels by catch type (landings/discards)
    # facet_grid(catch_type~source,labeller=labeller(source=source_labeller)) +
    ggtitle(substitute(a~" ("~italic(b)~")",list(a=taxon,b=sciname))) +
    theme(legend.position = "bottom")
  cat(taxon,"\n")
  print(plotr) # uncomment to plot in RStudio
  return(plotr)
})

# ### uncomment to create a multi-page PDF with one plot on each page
# pdf("plots/country-year.pdf",paper='USr')
# walk(country.plotz,function(plotr) {
#   print(plotr)
# })
# dev.off()


# aggregate global catch by species and fao fishing area
global.catch.area <- catch_data %>%
  group_by(year,common_name,scientific_name,fao_area,source) %>%
  summarise(gross_catch=sum(total_catch)) %>%
  ungroup()

# plot catch by species for each fishing area. in RStudio you can cycle through the different plots
# area.plotz will be a list of each plot object
area.plotz <- map(unique(as.character(global.catch.area$fao_area)),function(fishing_area) {
  plotr <- ggplot(global.catch.area %>% filter(fao_area == fishing_area), aes(x=year,y=gross_catch,fill=common_name)) +
    geom_col(color="black") +
    theme_minimal() +
    theme(panel.border = element_rect(colour = "black",fill = NA)) +
    scale_y_continuous(name="Total Catch",labels=comma) + 
    scale_x_continuous(name="Year",breaks=pretty(catch_data$year,n=10)) +
    scale_fill_manual(name="Species",values=pal_comm) + 
    facet_wrap(~source,nrow=2,ncol=1,labeller = source_labeller) + 
    ## uncomment the next line (and comment the previous one) to further break panels by catch type (landings/discards)
    # facet_grid(catch_type~source,labeller=labeller(source=source_labeller)) +
    ggtitle(fishing_area) +
    theme(legend.position = "bottom")
  cat(fishing_area,"\n")
  print(plotr) # uncomment to plot in RStudio
  return(plotr)
})

# ### uncomment to create a multi-page PDF with one plot on each page
# pdf("plots/area-species.pdf",paper='USr')
# walk(area.plotz,function(plotr) {
#   print(plotr)
# })
# dev.off()

###### Stacked bar plots by taxa #####
## plot stacked bar graph of total global catch color coded by taxon

# aggregate global catch, retaining catch_type
global.catch.bar <- catch_data %>% 
  group_by(year,common_name,scientific_name,catch_type,source) %>% 
  summarise(gross_catch=sum(total_catch)) %>% 
  ungroup()

(global.spp.plotz <- 
  ggplot(global.catch.bar, aes(x=year,y=gross_catch,fill=common_name)) +
    geom_col(color="black") +
    theme_minimal() +
    scale_y_continuous(name="Total Catch",labels=comma) + 
    scale_x_continuous(name="Year",breaks=pretty(catch_data$year,n=10)) +
    scale_fill_manual(name="Species",values=pal_comm) + 
    facet_wrap(~source,nrow=2,ncol=1,labeller = source_labeller) + 
    ## uncomment the next line (and comment the previous one) to further break panels by catch type (landings/discards)
    #facet_grid(catch_type~source,labeller=labeller(source=source_labeller)) +
    ggtitle("Global catch by species") +
    theme(legend.position = "bottom"))
