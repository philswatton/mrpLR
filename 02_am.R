# MRP LR ----
# Phil Swatton
# University of Essex
# File 02: Run AM model


## Packages
library(tidyverse)
library(psmisc) #alternative AM scaling function is available in basicspace


## BES
bes <- readRDS("data/bes.rds")




# AM scaling ----

mat <- bes %>% select(starts_with("lr"))
am <- amscale(mat[2:6], mat[[1]], polarity=2)

summary(am)



# Output ----

bes <- bes %>%
  select(-starts_with("lr")) %>%
  mutate(lrscale = am$respondent$idealpt)
saveRDS(bes, "data/bes2.rds")


