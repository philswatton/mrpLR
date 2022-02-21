# MRP LR ----
# Phil Swatton
# University of Essex
# File 05: Create Results


## Packages
library(tidyverse)
library(haven)
library(scales)
library(labelled)
# library(sf)
library(leaflet)
library(parlitools)
library(htmlwidgets)



## Data
bes <- readRDS("data/bes2.rds")
const <- read_dta("data/BES-2019-General-Election-results-file-v1.0.dta")
post <- readRDS("results/post.rds")


## Colours
red <- "#e4003b"
blu <- "#00aeef"
yt <- "#ffffff"
clist <- c(red, yt, blu)



# Datasets ----
df <- const %>%
  select(ConstituencyName, Region, Winner19, Con19, Lab19, LD19, Green19, Brexit19, UKIP19, SNP19, PC19, Other19) %>%
  rename(constituency = ConstituencyName,
         winner = Winner19) %>%
  mutate(winner = to_character(winner),
         Region = to_character(Region),
         constituency = case_when(constituency == "Carmarthen West and Pembrokeshire South" ~ "Carmarthen West and South Pembrokeshire",
                                  !is.na(str_match(constituency, "Ynys")) ~ "Ynys Mon",
                                  T ~ constituency),
         across(matches("19"), ~ case_when(is.na(.x) ~ 0,
                                           T ~ .x))) %>%
  left_join(post)

# parlitools hex map
hex <- west_hex_map %>%
  rename(constituency = constituency_name) %>%
  mutate(constituency = gsub("\\.", "", constituency)) %>%
  filter(region_name != "Northern Ireland") %>%
  select(-region_name) %>%
  left_join(df %>% select(constituency, Region, lrscale, winner) %>% mutate(lrscale = round(lrscale, 3))) %>%
  mutate(colour = col_numeric(clist, range(hex$lrscale))(hex$lrscale))




# Table ----

df %>%
  select(constituency, winner, lrscale) %>%
  arrange(lrscale) %>% View()




# Correlations ----
cor(df[, 4:13]) # All
cor(df[df$Region %in% 1:9, 4:13]) #England
cor(df[df$Region %in% 10, 4:13]) #wales
cor(df[df$Region %in% 11, 4:13]) #scotland


# Map ----

# Creating map labels
labels <- paste0(
  "Constituency: ", hex$constituency, "</br>",
  "Region: ", hex$Region, "</br>",
  "Left-Right Scale: ", hex$lrscale, "</br>",
  "2019 Winner: ", hex$winner
) %>% lapply(htmltools::HTML)

# Creating the map itself
m <- leaflet(options=leafletOptions(
  dragging = FALSE, zoomControl = FALSE, tap = FALSE,
  minZoom = 6, maxZoom = 6, maxBounds = list(list(2.5,-7.75),list(58.25,50.0)),
  attributionControl = FALSE),
  hex) %>%
  addPolygons(
    color = "grey",
    weight=0.75,
    opacity = 0.5,
    fillOpacity = 1,
    fillColor = ~colour,
    label=labels) %>%
  onRender(
    "function(x, y) {
        var myMap = this;
        myMap._container.style['background'] = '#fff';
    }")%>%
  mapOptions(zoomToLimits = "first")


# Print map
m

# Save map
saveWidget(m, file="results/map.html")

