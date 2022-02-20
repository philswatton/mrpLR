#!/usr/bin/Rscript


# MRP LR ----
# Phil Swatton
# University of Essex
# File 03: Build population frame


## Packages
library(tidyverse)
library(anesrake)
library(survey)
library(foreach)
library(doParallel)


## Data
marginals <- readRDS("data/marginals.rds") %>% as.data.frame()
census <- readRDS("data/census.rds") %>%
  filter(complete.cases(.)) %>%
  mutate(across(.fns=as.factor),
         id = row_number()) %>%
  as.data.frame()




# Raking ----

constRake <- function(constNumber) {

  # Subset constituency row
  const <- marginals[constNumber, c(3:35)] %>% t() %>% as.vector()
  names(const) <- names(marginals[constNumber, c(3:35)])

  # Generate list of marginal distributions
  marginalDist <- list(gender = const[1:2],
                       age = const[3:12],
                       nssec = const[13:20],
                       qualifications = const[21:26],
                       ethnicity = const[27:31],
                       owns = const[32:33])

  # Rake using anesrake() function from anesrake package
  constOut <- anesrake(inputter = marginalDist,
                       dataframe = census,
                       caseid = census$id)

  # Get census, attach weights
  constCensus <- census
  constCensus$weights <- constOut$weightvec

  # Produce weighted table
  cdesign <- svydesign(ids = ~1, weights = ~weights, data = constCensus)

  constJoint <- svytable(formula=~gender+age+nssec+qualifications+ethnicity+owns, design=cdesign) %>%
    data.frame() %>%
    mutate(constituency = marginals[constNumber,1],
           n = marginals[constNumber,2],
           per = Freq / sum(Freq),
           n = n * per) %>%
    select(!per & !Freq)


  return(constJoint)
}


# Register parallel computing
registerDoParallel(cores=(parallel::detectCores()-1))

# Run raking algorithm
frame <- foreach(i=1:nrow(marginals), .combine=rbind, .packages=c("tidyverse","anesrake","survey")) %dopar% {
  constRake(i)
}

# Convert frame factors back to character
frame <- frame %>%
  mutate(across(where(is.factor), as.character))

# End cluster
stopImplicitCluster()

# Save
saveRDS(frame, "data/frame.rds")

