# MRP LR ----
# Phil Swatton
# University of Essex
# File 01: Set up datasets for MRP


## Packages
library(tidyverse)
library(haven)
library(labelled)


## Datasets
bes <- read_dta("data/BES2019_W21_v21.0.dta")
census1 <- read_dta("data/isg_regionv2.dta")
census2 <- read_dta("data/safeguarded_regional.dta")
const <- read_dta("data/BES-2019-General-Election-results-file-v1.0.dta")




# Subset Data ----

## BES Subset ----

df <- bes %>%
  select(id, age, ns_sec_analytic, gender, p_education, p_housing, p_ethnicity, pcon,
         leftRight, matches("^lr[[:alpha:]]+")) %>%
  rename(lrSelf = leftRight,
         lrGreen = lrgreens) %>%
  select(-lrSNP, -lrPC)


## Const Subset ----
marginals <- const %>%
  select(ConstituencyName, Electorate19, Region,

         # Gender
         c11Male, c11Female,

         # Age
         c11Age18to19,c11Age20to24,c11Age25to29,c11Age30to44,c11Age45to59,
         c11Age60to64,c11Age65to74,c11Age75to84,c11Age85to89,c11Age90plus,

         # NS-SEC
         starts_with("c11NSSEC"),

         # Educational Qualifications
         starts_with("c11Qual"),

         # Ethnicity
         # starts_with("c11Ethnicity"),
         c11EthnicityWhite, c11EthnicityMixed, c11EthnicityAsian, c11EthnicityBlack, c11EthnicityOther,

         #Home Ownership
         c11HouseOwned,

         # Vote Shares
         Con19, Lab19, LD19, Green19, Brexit19, UKIP19, SNP19, PC19, Other19,

         # Turnout
         Turnout19,

         # Variables to use in the model
         remainHanretty, c11Unemployed, c11IndustryManufacturing, c11PopulationDensity)
names(marginals) <- names(marginals) %>% gsub("c11", "", .)


## Census Subset ----

# ethnicityew is person, ethuk11 is household

cen <- census1 %>%
  select(sex, age, nssec, hlqupuk11, ethnicityew, tenduk11)

cen2 <- census2 %>%
  select(SEX, AGE, NSSEC, HLQPS11, ETHNIC, TENHUK11) %>% #not ETHHUK11 - because I used ethnicityew not ethuk11
  rename(sex = SEX,
         age = AGE,
         nssec = NSSEC,
         qualifications = HLQPS11,
         ethnicity = ETHNIC,
         owns = TENHUK11)




# Recoding ----

# bes = eng
# constituencies = sub
# census = cen


## Gender ----

### BES
df <- df %>%
  mutate(gender = case_when(gender == 1 ~ "Male",
                            gender == 2 ~ "Female",
                            TRUE ~ NA_character_))

### Census
cen <- cen %>%
  rename(gender = sex) %>%
  mutate(gender = case_when(gender == 1 ~ "Male",
                            gender == 2 ~ "Female",
                            TRUE ~ NA_character_))

cen2 <- cen2 %>%
  rename(gender = sex) %>%
  mutate(gender = case_when(gender == 1 ~ "Male",
                            gender == 2 ~ "Female",
                            TRUE ~ NA_character_))

### Marginals
marginals <- marginals %>%
  mutate(Male = Male/100,
         Female = Female/100)


## Age ----

### BES
df <- df %>%
  mutate(age = case_when(age <= 19 ~ "Age18to19",
                         age <= 24 ~ "Age20to24",
                         age <= 29 ~ "Age25to29",
                         age <= 44 ~ "Age30to44",
                         age <= 59 ~ "Age45to59",
                         age <= 64 ~ "Age60to64",
                         age <= 74 ~ "Age65to74",
                         age <= 84 ~ "Age75to84",
                         age <= 89 ~ "Age85to89",
                         TRUE ~ "Age90plus")) #no missing data in age var so this is fine

### Census
cen <- cen %>%
  mutate(age = case_when(age < 18 ~ NA_character_,
                         age <= 19 ~ "Age18to19",
                         age <= 24 ~ "Age20to24",
                         age <= 29 ~ "Age25to29",
                         age <= 44 ~ "Age30to44",
                         age <= 59 ~ "Age45to59",
                         age <= 64 ~ "Age60to64",
                         age <= 74 ~ "Age65to74",
                         age <= 84 ~ "Age75to84",
                         age <= 89 ~ "Age85to89",
                         TRUE ~ "Age90plus"))

cen2 <- cen2 %>%
  mutate(age = case_when(age < 18 ~ NA_character_,
                         age <= 19 ~ "Age18to19",
                         age <= 24 ~ "Age20to24",
                         age <= 29 ~ "Age25to29",
                         age <= 44 ~ "Age30to44",
                         age <= 59 ~ "Age45to59",
                         age <= 64 ~ "Age60to64",
                         age <= 74 ~ "Age65to74",
                         age <= 84 ~ "Age75to84",
                         age <= 89 ~ "Age85to89",
                         TRUE ~ "Age90plus"))


### Marginals
marginals <- marginals %>%
  mutate(sumage = Age18to19 + Age20to24 + Age25to29 + Age30to44 + Age45to59 + Age60to64 + Age65to74 + Age75to84 + Age85to89 + Age90plus,
         across(matches("Age\\d"), ~.x / sumage)) %>%
  select(-sumage)

# sanity check
# marginals %>%
#   mutate(sumage = Age18to19 + Age20to24 + Age25to29 + Age30to44 + Age45to59 + Age60to64 + Age65to74 + Age75to84 + Age85to89 + Age90plus,
#          sumage = sumage == 1) %>%
#   select(sumage) %>%
#   all()


## NS-SEC ----

### BES
df <- df %>%
  rename(nssec = ns_sec_analytic) %>%
  mutate(nssec = case_when(nssec == 11 ~ "HigherManager",
                           nssec == 12 ~ "HigherProfessional",
                           nssec == 20 ~ "LowerManager",
                           nssec == 30 ~ "Intermediate",
                           nssec == 40 ~ "SmallEmployer",
                           nssec == 50 ~ "LowerSupervisor",
                           nssec == 60 ~ "SemiRoutine",
                           nssec == 70 ~ "Routine"))

### Census
cen <- cen %>%
  mutate(nssec = case_when(nssec %in% 1:2 ~ "HigherManager",
                           nssec %in% 3:6 ~ "HigherProfessional",
                           nssec %in% 7:12 ~ "LowerManager",
                           nssec %in% 13:16 ~ "Intermediate",
                           nssec %in% 17:20 ~ "SmallEmployer",
                           nssec %in% 21:23 ~ "LowerSupervisor",
                           nssec %in% 24:30 ~ "SemiRoutine",
                           nssec %in% 31:35 ~ "Routine",
                           TRUE ~ NA_character_))

cen2 <- cen2 %>%
  mutate(nssec = case_when(nssec %in% 1:2 ~ "HigherManager",
                           nssec %in% seq(3.1,3.4,0.1) ~ "HigherProfessional",
                           nssec %in% c(seq(4.1,4.4,0.1),5:6) ~ "LowerManager",
                           nssec %in% seq(7.1,7.4,0.1) ~ "Intermediate",
                           nssec %in% c(8.1,8.2,9.1,9.2) ~ "SmallEmployer",
                           nssec %in% c(10,11.1,11.2) ~ "LowerSupervisor",
                           nssec %in% seq(12.1,12.7,0.1) ~ "SemiRoutine",
                           nssec %in% seq(13.1,13.5,0.1) ~ "Routine",
                           TRUE ~ NA_character_))


### Marginals
marginals <- marginals %>%
  select(-NSSECNeverWorked, -NSSECLongtermUnemployed) %>%
  mutate(nssecsum = NSSECHigherManager + NSSECHigherProfessional + NSSECLowerManager + NSSECIntermediate + NSSECSmallEmployer + NSSECLowerSupervisor + NSSECSemiRoutine + NSSECRoutine,
         across(starts_with("NSSEC", ignore.case=F), ~.x/nssecsum)) %>%
  select(-nssecsum)
names(marginals) <- names(marginals) %>% gsub("NSSEC", "", .)


# Sanity check
# marginals %>%
#   mutate(nssecsum = NSSECHigherManager + NSSECHigherProfessional + NSSECLowerManager + NSSECIntermediate + NSSECSmallEmployer + NSSECLowerSupervisor + NSSECSemiRoutine + NSSECRoutine,
#          nssecsum = nssecsum == 1) %>%
#   select(nssecsum) %>%
#   # all() %>%
#   View()


## Education ----

### BES
df <- df %>%
  rename(qualifications = p_education) %>%
  mutate(qualifications = case_when(qualifications %in% c(1) ~ "LevelNone",
                                    qualifications %in% c(4:5,8) ~ "Level1",
                                    qualifications %in% c(2:3,6:7,9:10) ~ "Level2",
                                    qualifications %in% c(11:13) ~ "Level3",
                                    qualifications %in% c(14:17) ~ "Level4plus",
                                    qualifications %in% c(8) ~ "LevelOther",
                                    TRUE ~ NA_character_))

### Census
cen <- cen %>%
  rename(qualifications = hlqupuk11) %>%
  mutate(qualifications = case_when(qualifications == 10 ~ "LevelNone",
                                    qualifications == 11 ~ "Level1",
                                    qualifications %in% 12:13 ~ "Level2", #merging apprenticeships into lvl2
                                    qualifications == 14 ~ "Level3",
                                    qualifications == 15 ~ "Level4plus",
                                    qualifications == 16 ~ "LevelOther",
                                    TRUE ~ NA_character_))

cen2 <- cen2 %>%
  mutate(qualifications = case_when(qualifications == 20 ~ "LevelNone",
                                    qualifications == 21 ~ "Level1",
                                    qualifications == 22 ~ "Level2",
                                    qualifications == 23 ~ "Level3",
                                    qualifications == 24 ~ "Level4plus"))

### Marginals
marginals <- marginals %>%
  mutate(QualLevel2 = case_when(!is.na(QualApprentice) ~ QualLevel2 + QualApprentice,
                                TRUE ~ QualLevel2)) %>%
  mutate(across(matches("Qual"), ~.x/100)) %>%
  rename(Level4plus = QualLevel4,
         LevelNone = QualNone,
         LevelOther = QualOther) %>%
  select(-QualApprentice)
names(marginals) <- names(marginals) %>% gsub("Qual", "", .)

marginals %>% select(matches("Level")) %>% rowSums()




## Ethnicity ----

### BES
df <- df %>%
  rename(ethnicity = p_ethnicity) %>%
  mutate(ethnicity = case_when(ethnicity %in% 1:2 ~ "White",
                               ethnicity %in% 3:6 ~ "Mixed",
                               ethnicity %in% c(7:10,14) ~ "Asian",
                               ethnicity %in%  11:13 ~ "Black",
                               ethnicity %in% 15 ~ "OtherEthnic",
                               TRUE ~ NA_character_))

### Census
cen <- cen %>%
  rename(ethnicity = ethnicityew) %>%
  mutate(ethnicity = case_when(ethnicity %in% 1:4 ~ "White",
                               ethnicity %in% 5:8 ~ "Mixed",
                               ethnicity %in% 9:13 ~ "Asian",
                               ethnicity %in% 14:16 ~ "Black",
                               ethnicity %in% 17:18 ~ "OtherEthnic",
                               TRUE ~ NA_character_))
cen2 <- cen2 %>%
  mutate(ethnicity = case_when(ethnicity %in% 1:6 ~ "White",
                               ethnicity == 7 ~ "Mixed",
                               ethnicity %in% 8:12 ~ "Asian",
                               ethnicity %in% 13:14 ~ "Black",
                               ethnicity %in% 15:16 ~ "OtherEthnic"))


### Marginals
marginals <- marginals %>%
  rename(EthnicityOtherEthnic = EthnicityOther) %>%
  mutate(across(matches("Ethnicity"), ~.x/100))
names(marginals) <- names(marginals) %>% gsub("Ethnicity", "", .)




## Home ownership ----

### BES
df <- df %>%
  rename(owns = p_housing) %>%
  mutate(owns = case_when(owns %in% 1:2 ~ "Owns",
                          owns %in% 3:9 ~ "Rents",
                          TRUE ~ NA_character_))

### Census
cen <- cen %>%
  rename(owns = tenduk11) %>%
  mutate(owns = case_when(owns %in% 0:1 ~ "Owns",
                          owns %in% 2:9 ~ "Rents")) #as in BES, decomposing "lives rent free" into "rents" as they don't *own* a home

cen2 <- cen2 %>%
  mutate(owns = case_when(owns %in% 0:1 ~ "Owns",
                          owns %in% 2:9 ~ "Rents"))


### Marginals
marginals <- marginals %>%
  rename(Owns = HouseOwned) %>%
  mutate(Owns = Owns/100,
         Rents = 1 - Owns)



## Left-Right ----

### BES
df <- df %>%
  mutate(across(starts_with("lr"), ~case_when(.x == 9999 ~ NA_real_,
                                              T ~ as.numeric(.x))))


## Constituency ----

### BES
df <- df %>%
  mutate(constituency = to_character(pcon)) %>%
  filter(constituency != "NOT in a 2010 Parliamentary Constituency" & constituency != "Lagan Valley") %>%
  select(-pcon)


### Marginals
marginals <- marginals %>%
  rename(constituency = ConstituencyName) %>%
  mutate(constituency = case_when(constituency == "Carmarthen West and Pembrokeshire South" ~ "Carmarthen West and South Pembrokeshire",
                                  !is.na(str_match(constituency, "Ynys")) ~ "Ynys Mon",
                                  T ~ constituency))




## Extra constituency variables ----

marginals <- marginals %>%
  rename(region = Region,
         perRemain = remainHanretty,
         perUnemployed = Unemployed,
         perManufacturing = IndustryManufacturing,
         density = PopulationDensity) %>%
  mutate(across(starts_with("per"), ~.x/100),
         across(matches("^(?!Age).*19$", perl=T), ~.x/100),
         density = density/100,
         region = to_character(region))


## Adding constituency-level variables to the BES ----

extras <- marginals %>%
  select(constituency, region, matches("^(?!Age).*19$", perl=T), starts_with("per"), density, White) %>%
  rename(perWhite = White)

df <- left_join(df, extras) %>%
  mutate(across(matches("^(?!Age).*19$", perl=T), ~ case_when(is.na(.x) ~ 0,
                                                              T ~ .x)))

marginals <- marginals %>% select(-starts_with("per"), -density, -region,
                                  -Con19, -Lab19, -LD19, -Green19, -Brexit19,
                                  -UKIP19, -SNP19, -PC19, -Other19, -Turnout19) %>%
  mutate(Electorate19 = Electorate19 * 100) #fixing a formatting error



# Saving ----

saveRDS(df, "data/bes.rds")
saveRDS(rbind(cen, cen2), "data/census.rds")
saveRDS(marginals, "data/marginals.rds")
saveRDS(extras, "data/extras.rds") #will put these into the frame after the raking is complete






