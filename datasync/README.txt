This folder contains the complete FAO data for all species, the SAU data dump
that was given to me for our species list, and the code and scripts necessary to
generate a combined dataset.

Files:

tsd_global_production_long.csv.gz: The complete FAO dataset for all species
                                   (gzipped)
Mykle_v2_v47.csv.gz: SAU dataset for our species list (gzipped, unmodified)
ref_country.csv.gz: Dump of the FAO table of countries (gzipped)
species-translate.csv.gz: Our species list as understood by both FAO and SAU
                          (gzipped)
sau-to-fao.sed: SED script to fix country names between SAU and FAO
sync-fao-sau.R: R script to generate the final combined dataset


How to generate the combined dataset:

1. You need to run a sed script on the command line to fix wonky country names
that SAU uses. To do this, open a terminal and run:

$ gunzip Mykle_v2_v47.csv.gz
$ sed -i'.original' -f sau-to-fao.sed Mykle_v2_v47.csv
$ gzip Mykle_v2_v47.csv

What you have done is unzipped the file, run several search and replace
operations (first copying the original file with an extension of .original), and
then re-zipped it. The file is now ready for processing in R with the correct
country names and everything.


2. Go into R and load the sync-fao-sau.R script. You may need to adjust the
pathnames as they appear in the code, but otherwise it should just run. You'll
need the tidyverse package to be installed for it to work properly. The end
result is that you'll have two files:

fao-sau-combined-filtered.csv: combined dataset filtered for our species list
fao-sau-combined-all.csv: combined dataset with FAO data unfiltered (but SAU
                          data still filtered on our species list)

