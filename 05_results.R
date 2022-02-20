# MRP LR ----
# Phil Swatton
# University of Essex
# File 05: Create Results


## Packages
library(tidyverse)
library(haven)
library(labelled)
library(sf)
library(leaflet)
library(parlitools)
library(britpol) #https://github.com/jackobailey/britpol


## Data
const <- read_dta("data/BES-2019-General-Election-results-file-v1.0.dta")
post <- readRDS("results/post.rds")



# Datasets ----
df <- const %>%
  select(ConstituencyName, Region, Con19, Lab19, LD19, Green19, UKIP19, SNP19, PC19, Other19) %>%
  rename(constituency = ConstituencyName) %>%
  mutate(constituency = case_when(constituency == "Carmarthen West and Pembrokeshire South" ~ "Carmarthen West and South Pembrokeshire",
                                  constituency == "Ynys M\xf4n" ~ "Ynys Mon",
                                  T ~ constituency),
         across(matches("19"), ~ case_when(is.na(.x) ~ 0,
                                           T ~ .x))) %>%
  left_join(post)

# parlitools hex map
hex <- west_hex_map %>%
  rename(constituency = constituency_name) %>%
  mutate(constituency = gsub("\\.", "", constituency))
  mutate(constituency = case_when(constituency == "Bury St. Edmunds" ~ "",
                                  constituency == "Holdborn and St. Pancras" ~ "",
                                  constituency == "St. Albans" ~ "",
                                  constituency == "St. Helens North" ~ "",
                                  constituency == "St. Helens South and Whiston" ~ "",
                                  constituency == "Ynys Mon" ~ "",
                                  constituency == "St. Austell and Newquay" ~ "St. Austell and Newquay",
                                  constituency == "St. Ives" ~ "St Ives"))

# britpol red wall
rw <- red_wall



# Correlations
cor(df[df$Region %in% 1:9, 3:11])
cor(df[df$Region %in% 10, 3:11]) #wales
cor(df[df$Region %in% 11, 3:11]) #scotland


# Map ----

hex
class(hex)

hex$constituency[!(hex$constituency[hex$region_name != "Northern Ireland"] %in% post$constituency)]
unique(post$constituency) %>% sort()


post$constituency
