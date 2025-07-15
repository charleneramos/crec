# The purpose of this script is to analyze the Hybrid Electronics Skills Base in the Bay Area 
rm(list = ls())
setwd("C:/Users/yangh/Downloads/workforce_development_project")
library(pacman)
p_load(tidyverse)
p_load(tidycensus)
p_load(forcats)
p_load(stringr)
p_load(openxlsx)
p_load(ipumsr)
p_load(naniar)
p_load(clipr)
p_load(gdata)
p_load(tidyr)


################### CLEANING AND WRANGLING ##############################################################

# Load ACS 2019-2023 microdata, downloaded from IPUMS USA
ddi <- read_ipums_ddi("data/usa_00008.xml")
extract <- read_ipums_micro(ddi)
colnames(extract) <- tolower(colnames(extract))
summary(extract)
sum(extract$perwt)

names(extract)





# Delete the technical and redundant variables
us_data_me <- extract %>% select(-year, -cbserial, -cluster, -year, -multyear, -serial, -cbserial,
                               -cluster, -gq, -strata, -ownershpd, -multgend, -pernum, -languaged,
                               -migrate1, -vetstat, -educ)


# Recategorize the educational attainment variable
us_data_me <- us_data_me %>% 
  mutate(edu=case_when(
    educd %in% 0:61 ~"Less than High School",
    educd %in% 62:64 ~"High School",
    educd %in% 81:83 ~"Associate's Degree",
    educd %in% c(65:80, 90:100, 110:113) ~ "Some College, No Degree",
    educd %in% c(101, 114:116) ~ "Bachelor's Degree or Higher",
    educd %in% 999 ~ "Missing")) %>% 
  replace_with_na(replace=list(edu="Missing")) %>% 
  rename(Educational_Attainment = edu) %>% 
  select(-educd)

# Assign occupation names to the OCCSOC variable

us_data_me %>% count(occsoc)

occsoc_crosswalk <- read.csv("data/occsoc_crosswalk_2000_onward_without_code_descriptions.csv") %>% 
  select(X2018.Onward.ACS.PRCS, Occupation.title) %>% 
  filter(!(X2018.Onward.ACS.PRCS==""))

us_data_me <- us_data_me %>% left_join(occsoc_crosswalk, by = c("occsoc"="X2018.Onward.ACS.PRCS")) %>% 
  rename(Occupation = Occupation.title)

# Assign NAICS industry names to the INDNAICS variable

indnaics_crosswalk <- read.csv("data/indnaics_crosswalk_2023.csv") %>% 
  select(X2023.2027.ACS.PRCS.INDNAICS.CODE, Industry.Title) %>% 
  filter(!(X2023.2027.ACS.PRCS.INDNAICS.CODE==""))

us_data_me <- us_data_me %>% left_join(indnaics_crosswalk, by = c("indnaics"="X2023.2027.ACS.PRCS.INDNAICS.CODE")) %>% 
  rename(Industry = Industry.Title)

# Crosswalk migration

migrate1d_crosswalk <- ipums_val_labels(us_data_me$migrate1d)

us_data_me <- us_data_me %>% 
  left_join(migrate1d_crosswalk, by = c("migrate1d"="val")) %>% 
  rename(Migration_Status = lbl) %>% 
  select(-migrate1d)


migplac1_crosswalk <- ipums_val_labels(extract$migplac1)

us_data_me <- us_data_me %>% 
  left_join(migplac1_crosswalk, by = c("migplac1"="val")) %>% 
  rename(Migration_Place = lbl) %>% 
  select(-migplac1)

#Crosswalk degfield

degfield_crosswalk <- ipums_val_labels(extract$degfield)

us_data_me <- us_data_me %>% 
  left_join(degfield_crosswalk, by = c("degfield"="val")) %>% 
  rename(Degree_Field = lbl) %>% 
  select(-degfield)

degfieldd_crosswalk <- ipums_val_labels(extract$degfieldd)

us_data_me <- us_data_me %>% 
  left_join(degfieldd_crosswalk, by = c("degfieldd"="val")) %>% 
  rename(Degree_Field_Detailed = lbl) %>% 
  select(-degfieldd)


#Crosswalk veterans status

vetstatd_crosswalk <- ipums_val_labels(us_data_me$vetstatd)

us_data_me <- us_data_me %>% 
  left_join(vetstatd_crosswalk, by = c("vetstatd"="val")) %>% 
  rename(Veteran_Status = lbl) %>% 
  select(-vetstatd)

#Crosswalk transportation

tranwork_crosswalk <- ipums_val_labels(extract$tranwork)

us_data_me <- us_data_me %>% 
  left_join(tranwork_crosswalk, by = c("tranwork"="val")) %>% 
  rename(Commute_Mode = lbl) %>% 
  mutate(Commute_Time = round(trantime/60, 1)) %>% 
  select(-tranwork, -trantime)

#Wages

us_data_me <- us_data_me %>% rename(Wage_Income = incwage,
                                Total_Income = inctot)

#Crosswalk Employment Status

empstatd_crosswalk <- ipums_val_labels(us_data_me$empstatd)

us_data_me <- us_data_me %>% 
  left_join(empstatd_crosswalk, by = c("empstatd"="val")) %>% 
  rename(Employment_Status = lbl) %>% 
  select(-empstatd)

#Crosswalk Poverty

ipums_val_labels(extract$poverty)

us_data_me <- us_data_me %>% rename(Poverty_Threshold_Perc = poverty)


#Crosswalk City
city_crosswalk <- ipums_val_labels(extract$city)

us_data_me <- us_data_me %>% 
  left_join(city_crosswalk, by = c("city"="val")) %>% 
  rename(City = lbl) %>% 
  select(-city)


#Crosswalk Sex
sex_crosswalk <- ipums_val_labels(extract$sex)

us_data_me <- us_data_me %>% 
  left_join(sex_crosswalk, by = c("sex"="val")) %>% 
  rename(Sex = lbl) %>% 
  select(-sex)

#Recode race
us_data_me <- us_data_me %>% 
  mutate(Race=case_when(
    race == 1 ~ "White",
    race == 2 ~ "Black",
    race == 3 ~ "AIAN",
    race %in% 4:6 ~ "Asian",
    race %in% 7:9 ~ "Other Race")) %>% select(-race)

#Recode Hispanic

us_data_me <- us_data_me %>% 
  mutate(Hispanic=case_when(
    hispan == 0 ~ "Not Hispanic",
    hispan %in% 1:8 ~ "Hispanic",
    hispan == 9 ~ "Not Reported")) %>% select(-hispan)

us_data_me <- us_data_me %>% 
  mutate(Race_Ethnicity = ifelse(Race == "White" & Hispanic != "Hispanic", "White",
                                 ifelse(Race == "Black" & Hispanic != "Hispanic", "Black",
                                        ifelse(Race == "AIAN" & Hispanic != "Hispanic", "AIAN",
                                               ifelse(Race == "Asian" & Hispanic != "Hispanic", "Asian",
                                                      ifelse(Race == "Other Race" & Hispanic != "Hispanic", "Other Race", "Hispanic")))))) %>% 
  select(-Race, -Hispanic)


#Recode Health Insurance

us_data_me <- us_data_me %>% 
  mutate(Health_Insurance=case_when(
    hcovany == 1 ~ "No health insurance coverage",
    hcovany == 2 ~ "With health insurance coverage")) %>% 
  select(-hcovany)

#Crosswalk Language
language_crosswalk <- ipums_val_labels(us_data_me$language)

us_data_me <- us_data_me %>% 
  left_join(language_crosswalk, by = c("language"="val")) %>% 
  rename(Language = lbl) %>% 
  select(-language)

#Crosswalk Years in US

us_data_me <- us_data_me %>% rename(Years_in_US = yrsusa1)

us_data_me %>% names()

#Crosswalk Renter vs. Owner
rent_own_crosswalk <- ipums_val_labels(us_data_me$ownershp)

us_data_me <- us_data_me %>% 
  left_join(rent_own_crosswalk, by = c("ownershp"="val")) %>% 
  rename(Rent_or_Own = lbl) %>% 
  select(-ownershp)

#Rename the continuous/quantitative variables to be more intuitive

us_data_me <- us_data_me %>% rename(Residence_PUMA = puma,
                                Work_PUMA = pwpuma00,
                                Monthly_Rent = rentgrs)



### Remove all unnecessary data frames

rm(list=(ls()[ls()!="us_data_me"]))

#### Filter only for individuals in the labor force

us_data_me_inlf <- us_data_me %>% filter(labforce==2)




################## END OF DATA CLEANING ###################################################################












#################### Begin process to create PowerBI compatible CSV file #################################



### NOTE for Charlene --> Change the definition to other technology areas, based on the SOC definition for AI and biomanufacturing




# 192030: Chemists and Materials Scientists
# 172070: Electrical and Electronics Engineers
# 172041: Chemical Engineers
# 271021: Commercial And Industrial Designers
# 172110: Industrial Engineers, including Health and Safety


### Microelectronics-Related Technician Occupations ###

# 512020: Electrical, Electronics, and Electromechanical Assemblers
# 173023: Electrical and Electronic Engineering Technologists And Technicians
# 17302X: Other Engineering Technologists And Technicians, Except Drafters
# 519160: Computer Numerically Controlled Tool Operators And Programmers
# 194031: Chemical Technicians
# 492091: Avionics Technicians


################## Input the Relevant Technology Occupations Here ##########################################


engineer_occupations <- c("172061","17301X","172131","192030","172070","172041","271021","172110")
technician_occupations <- c("512020","173023","17302X","519160","194031","492091")


#############################################################################################################

engineers_data <- us_data_me_inlf %>% 
  select(occsoc, Occupation) %>% 
  count(occsoc, Occupation) %>% select(-n) %>%
  filter(occsoc %in% engineer_occupations) %>%
  mutate(occ_type = "Engineer")  

technicians_data <- us_data_me_inlf %>% 
  select(occsoc, Occupation) %>% 
  count(occsoc, Occupation) %>% select(-n) %>%
  filter(occsoc %in% technician_occupations) %>% 
  mutate(occ_type = "Technician")  


occupations <- bind_rows(engineers_data, technicians_data)


workers <- us_data_me_inlf %>%
  left_join(occupations %>% select(occsoc, occ_type), by=c("occsoc"))


sum(workers$perwt) # 1,887,842 workers who work in these occupations across the US


###### Get the MSA Names  #######

region_names <- ipums_val_labels(workers$met2013)

workers <- workers %>% left_join(region_names, by = c("met2013"="val")) %>% rename(MSA_Name = lbl)

workers <- workers %>% group_by(MSA_Name) %>% mutate(pb_lq_regtotal = sum(perwt))


# Filter out workers that are not in an MSA
workers <- workers %>% filter(MSA_Name!="Not in identifiable area")

#### Rank educational attainment ######

workers <- workers %>% 
  mutate(Education_Attainment_Rank = ifelse(Educational_Attainment == "Bachelor's Degree or Higher", 5,
                                            ifelse(Educational_Attainment == "Associate's Degree", 4,
                                                   ifelse(Educational_Attainment == "Some College, No Degree", 3,
                                                          ifelse(Educational_Attainment == "High School", 2,
                                                                 ifelse(Educational_Attainment == "Less than High School", 1, NA))))))




####### Add PUMA Names ###############

puma_names <- read.xlsx("data/PUMA Names.xlsx") %>% 
  mutate(PUMA = as.character(PUMA)) %>% 
  mutate(PUMA_Code = paste0(as.character(str_pad(State, 2, side = c("left"), pad = "0")), 
                            as.character(str_pad(PUMA, 5, side = c("left"), pad = "0")))) %>% 
  select(-PUMA, -State)

workers <- workers %>% 
  mutate(PUMA_Code = paste0(as.character(str_pad(statefip, 2, side = c("left"), pad = "0")), 
                            as.character(str_pad(Residence_PUMA, 5, side = c("left"), pad = "0"))))

workers <- workers %>% left_join(puma_names, by = c("PUMA_Code")) %>% rename(PUMA_Name = PUMA.Name)

# Keep only the variables that will be used for PowerBI

subset_data <- workers[1:10000, ]
write.csv(subset_data, "subset_Workforce_ACS Data.csv")

workers_export <- workers %>% 
  select(perwt, MSA_Name, occ_type, occsoc, Occupation, Perc_Growth_10Yr, Avg_Annual_Earnings,
         indnaics, Industry, Employment_Status, bpl,
         Residence_PUMA, Work_PUMA, PUMA_Code, PUMA_Name, pwcounty, statefip, countyfip, City, 
         Educational_Attainment, Education_Attainment_Rank, Degree_Field_Detailed, age, Sex, Race_Ethnicity, Veteran_Status,
         Wage_Income, Commute_Time, Rent_or_Own, Health_Insurance) %>% 
  uncount(perwt)


workers_export <- workers %>% 
  select(perwt, MSA_FIPS, MSA_Name, occ_type, occsoc, Occupation, Jobs_2025, LQ_2025, Perc_Growth_5Yr, Perc_Growth_10Yr, Avg_Annual_Earnings,
         indnaics, Industry, Employment_Status, bpl,
         Residence_PUMA, Work_PUMA, PUMA_Code, PUMA_Name, pwcounty, statefip, countyfip, City, 
         Educational_Attainment, Education_Attainment_Rank, Degree_Field_Detailed, age, Sex, Race_Ethnicity, Veteran_Status,
         Wage_Income, Commute_Time, Rent_or_Own, Health_Insurance) %>% 
  uncount(perwt)


write.csv(workers_export, "National ME Workforce_ACS Data.csv")





