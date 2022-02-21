# MRP LR ----
# Phil Swatton
# University of Essex
# File 04: Run MRP model


## Packages
library(tidyverse)
library(lme4)


## Data
bes <- readRDS("data/bes2.rds")
frame <- readRDS("data/frame.rds")
marginals <- readRDS("data/marginals.rds")
extras <- readRDS("data/extras.rds") %>%
  mutate(across(matches("19"), ~ case_when(is.na(.x) ~ 0,
                                           T ~ .x)))




# Regression ----


## Formula ----
model <- lrscale ~ (1|gender) + (1|age) + (1|nssec) + (1|qualifications) + (1|ethnicity) + (1|owns) +
  (1|constituency) +
  perRemain + Con19 + SNP19 + Green19 + LD19 + PC19 + Other19 +
  density + perUnemployed + perManufacturing + perWhite


## Run regression ----
result <- lmer(model, bes)

# Save the output
saveRDS(result, "results/model.rds")



# Poststratification ----

## Build frame ----
frame <- left_join(frame, extras)


## Generate predictions ----
pred <- predict(result, frame, allow.new.levels = T)
saveRDS(pred, "results/pred.rds")
frame$pred <- pred


## Poststratify
post <- frame %>%
  mutate(wpred = pred * n) %>%
  group_by(constituency) %>%
  summarise(lrscale = sum(wpred)/sum(n))
saveRDS(post, "results/post.rds")









