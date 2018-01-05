# geospatial-clustering
# This script uses k-means to cluster voting precincts based off there centers and then 
# displays information in a leaflet app to show descriptive statistics

###########################################
## COPY THE CODE FROM map1016_jrb_202517 ##
## and re-do it with k=500 clusters      ##
## re-map it and see if it alleviates    ##
## the over division in LA City          ##
###########################################

## Goal is to make something similar to what Judy made in map1016.R for GIS
## they want iterations with 800, 900 clusters for the suggested voting locations for the new system
## I start with Judy's code and make modifications below
####################################################################################################
#Goal: to create a map with demographics info to determine community meeting

setwd("//sfile2/ExecOffice/JordanBerninger/GIS_clustering")
## CHANGE THE WORKING DIRECTORY SO I DONT MESS UP HER WORK
library(raster) ###LOAD RASTER LIBRARY FIRST###
library(readr)
library(dplyr)
library(data.table)
library(ggmap)
library(ggplot2)
library(leaflet)
library(rgdal)
library(maps)
library(foreign)
library(tigris)
library(acs)
library(stringr)
library(rgeos)
library(Hmisc)
library(scales)
library(readxl)
library(mapview)
library(sp)
library(RODBC)
library(readxl)
library(maptools)
library(flexclust)
library(knitr)
library(tidyr)
library(devtools)
library(geometry)
library(TSP)
library(maptools)
library(PBSmapping)

channel <- odbcConnect("slavote", 
                       uid="STAT_USER_E629521",pwd="sidePeaceL@weach", 
                       believeNRows=FALSE)

##########################################################################################  
#select interested fields and perform modifications
#variables used for popup 

#read in voterfile
system.time(vf_db <- sqlQuery(channel,"SELECT * FROM VoterFile")) #15 min
#vf1 <- vf

vf <- vf_db %>%
  select(1,5,7,23,25,26,31,33) %>% #select voter_id, first name, prefix, gender, 
  #birthdate, pid, perm_catg, language preference
  mutate(age = 2017 - as.numeric(substring(birth_date,1,4))) #calculate age


#fill in as much GENDER using prefix and frequency of first name         
vf$gender = as.character(vf$gender)
vf$gender = ifelse(vf$gender %in% c("F", "M"), vf$gender, NA) #change blank space in GENDER to NA

#change blank space in PREFIX to NA
vf$name_prefix = as.character(vf$name_prefix)
vf$name_prefix = ifelse(vf$name_prefix %in% c("MR", "MISS","MRS","MS"), vf$name_prefix, NA)

#if PREFIX is MR, assign M
vf$gender = as.character(vf$gender)
vf$gender = ifelse(is.na(vf$gender)==F, vf$gender, ifelse(vf$name_prefix  == "MR", "M", "F"))


gender.key <- as.data.frame.matrix(table(vf$name_first, vf$gender)) #get frequency of M & F by first name
gender.key$first.name <- row.names(gender.key)
gender.key <- gender.key %>%
  filter(`F`>0| `M`>0) #keep first name appears at least 1

vf <- vf %>%
  left_join(gender.key, by = c("name_first"="first.name")) #join with voter data

vf$gender1 <- ifelse(is.na(vf$gender)==F, vf$gender,ifelse(vf$`M`>vf$`F`, "M", "F"))
#if M freq > F freq, assign M; likewise for reverse

vf$language.count <- 1 #mark 1 count for each language in order to have a value for spread() parameter
vf.language <- spread(vf[,c("voter_id", "language", "language.count")], language, language.count, fill = 0) 
#for blank, fill 0

vf <- left_join(vf, vf.language, "voter_id" )
vf <- select(vf, 1,6:7,9,12,15:35)

#find total number of voter and median age of each precinct ID
vf.total.age <- vf %>%
  group_by(precinct_id) %>%
  dplyr::summarise(n(), median(age, na.rm = T)) %>%
  setnames(2:3, c("total", "voter.age")) %>%
  mutate(precinct_id = as.character(precinct_id))


#find female % (use for gender breakdown later)
gender.table <- as.data.frame.matrix(table(vf$precinct_id, vf$gender1))
vf.gender <- gender.table %>%
  setnames(1:2, c("Female", "Male")) %>%
  mutate(precinct_id = row.names(gender.table)) %>%
  mutate(precinct_id = as.character(precinct_id))

#number of PVBM voter
vf.perm <- vf %>%
  group_by(precinct_id) %>% 
  dplyr:: summarise(sum(is.na(perm_category)==F)) %>% 
  setnames(2, "perm")%>%
  mutate(precinct_id = as.character(precinct_id))

#% of langugage
vf.language <- vf[,c(2,6:26)] %>% group_by(precinct_id) %>%
  dplyr:: summarise_all(funs(sum))  
vf.language <- bind_cols(vf.language[1],round(vf.language[,-1]/rowSums(vf.language[,-1])*100,1)) %>%
  mutate(precinct_id = as.character(precinct_id))
## I CHANGED THIS FROM SUMMARISE_EACH() TO SUMMARISE_ALL() AND ALSO ADDED "1" INTO THE SELECT STATEMENT

#join fields
vf <- left_join(vf.total.age, vf.gender, "precinct_id")
vf <- left_join(vf, vf.perm, "precinct_id") 
vf <- left_join(vf, vf.language, "precinct_id") %>%
  mutate(perm = round(perm/total * 100,1),
         female.pct = round(Female/total * 100, 1)) 

########################################################################################## 

#get pid and CENSUS TRACT info from PrecinctDistrict_GroupType 
#(specifically, election 980 &3 496)
system.time(pdist_db <- sqlQuery(channel,"SELECT * FROM PrecinctDistrict_GroupType 
                                 WHERE election_id IN ('980','3496')")) #1 min


pdist <- pdist_db %>%
  select(3,1,54) %>% 
  mutate(index = paste(election_id, precinct_id, sep = "_"),
         CENSUS.TRACT = paste(substring(`2010 CENSUS TRACTS`, 17,20),
                              substring(`2010 CENSUS TRACTS`, 22,23), sep = "")) %>%
  #extract census tract info (2010)
  select(-3) %>%
  unique()%>%
  mutate(precinct_id = as.character(precinct_id))%>%
  filter(CENSUS.TRACT != "NANA") #remove empty census tract

pdist.censustract<- pdist %>% select(2,4) %>% unique()

precinct.supervisorial <- pdist_db %>% select(1, 3, 8)


#get 2015 ACS data (grab from Voter Turnout Project, might not used all the demographics)
## GRAB THE FILE "\\\\sfile2/ExecOffice/JudyLy/community_outreach_map/demographic data (census bureau)" 
## AND SAVE IT TO MY FOLDER ON THE E-DRIVE
{
  setwd("\\\\sfile2/ExecOffice/JordanBerninger/GIS_clustering/demographic data (census bureau)")
  combined_2015 <- read_csv("2015 ACS.csv", skip=1) %>%
    mutate(`CENSUS TRACT` = substr(Id2, 6,11),
           MALE = `Total; Estimate; SEX - Male`,
           FEMALE = `Total; Estimate; SEX - Female`,
           AGE = `Total; Estimate; Median age (years)`,
           OVER65 = `Total; Estimate; AGE - 65 to 74 years`+
             `Total; Estimate; AGE - 75 years and over`,
           ENGLISH.LEVEL =`Total; Estimate; LANGUAGE SPOKEN AT HOME AND ABILITY TO SPEAK ENGLISH - Population 5 years and over - Speak language other than English - Speak English less than \"very well\"`,
           HISPANIC = `Total; Estimate; Hispanic or Latino origin (of any race)`,
           INCOME =`Total; Estimate; Median income (dollars)`,
           WHITE= `Total; Estimate; RACE AND HISPANIC OR LATINO ORIGIN - One race - White`,
           AFR.AM =`Total; Estimate; RACE AND HISPANIC OR LATINO ORIGIN - One race - Black or African American`,
           ASIAN = `Total; Estimate; RACE AND HISPANIC OR LATINO ORIGIN - One race - Asian`) 
  combined_2015 <- combined_2015[,(ncol(combined_2015)-10):ncol(combined_2015)]
  
  education_2015 <- read_csv("2015 education (18+).csv", skip=1)%>%
    mutate(`CENSUS TRACT` = substr(Id2, 6,11),
           `HS.AND.LESS` = round((`Estimate; Male: - 18 to 24 years: - Less than 9th grade`+
                                    `Estimate; Male: - 18 to 24 years: - 9th to 12th grade, no diploma`+
                                    `Estimate; Male: - 18 to 24 years: - High school graduate (includes equivalency)`+
                                    `Estimate; Male: - 25 to 34 years: - Less than 9th grade`+ 
                                    `Estimate; Male: - 25 to 34 years: - 9th to 12th grade, no diploma`+
                                    `Estimate; Male: - 25 to 34 years: - High school graduate (includes equivalency)`+
                                    `Estimate; Male: - 35 to 44 years: - Less than 9th grade`+
                                    `Estimate; Male: - 35 to 44 years: - 9th to 12th grade, no diploma`+
                                    `Estimate; Male: - 35 to 44 years: - High school graduate (includes equivalency)`+
                                    `Estimate; Male: - 45 to 64 years: - Less than 9th grade`+
                                    `Estimate; Male: - 45 to 64 years: - 9th to 12th grade, no diploma`+
                                    `Estimate; Male: - 45 to 64 years: - High school graduate (includes equivalency)`+
                                    `Estimate; Male: - 65 years and over: - Less than 9th grade`+
                                    `Estimate; Male: - 65 years and over: - 9th to 12th grade, no diploma`+
                                    `Estimate; Male: - 65 years and over: - High school graduate (includes equivalency)`+
                                    `Estimate; Female: - 18 to 24 years: - Less than 9th grade`+
                                    `Estimate; Female: - 18 to 24 years: - 9th to 12th grade, no diploma`+
                                    `Estimate; Female: - 18 to 24 years: - High school graduate (includes equivalency)`+
                                    `Estimate; Female: - 25 to 34 years: - Less than 9th grade`+ 
                                    `Estimate; Female: - 25 to 34 years: - 9th to 12th grade, no diploma`+
                                    `Estimate; Female: - 25 to 34 years: - High school graduate (includes equivalency)`+
                                    `Estimate; Female: - 35 to 44 years: - Less than 9th grade`+
                                    `Estimate; Female: - 35 to 44 years: - 9th to 12th grade, no diploma`+
                                    `Estimate; Female: - 35 to 44 years: - High school graduate (includes equivalency)`+
                                    `Estimate; Female: - 45 to 64 years: - Less than 9th grade`+
                                    `Estimate; Female: - 45 to 64 years: - 9th to 12th grade, no diploma`+
                                    `Estimate; Female: - 45 to 64 years: - High school graduate (includes equivalency)`+
                                    `Estimate; Female: - 65 years and over: - Less than 9th grade`+
                                    `Estimate; Female: - 65 years and over: - 9th to 12th grade, no diploma`+
                                    `Estimate; Female: - 65 years and over: - High school graduate (includes equivalency)`)/`Estimate; Total:`*100, 2),
           SOME.COLLEGE =round((`Estimate; Male: - 18 to 24 years: - Some college, no degree`+
                                  `Estimate; Male: - 18 to 24 years: - Associate's degree`+
                                  `Estimate; Male: - 25 to 34 years: - Some college, no degree`+
                                  `Estimate; Male: - 25 to 34 years: - Associate's degree`+
                                  `Estimate; Male: - 35 to 44 years: - Some college, no degree`+
                                  `Estimate; Male: - 35 to 44 years: - Associate's degree`+
                                  `Estimate; Male: - 45 to 64 years: - Some college, no degree`+
                                  `Estimate; Male: - 45 to 64 years: - Associate's degree`+
                                  `Estimate; Male: - 65 years and over: - Some college, no degree`+
                                  `Estimate; Male: - 65 years and over: - Associate's degree`+
                                  `Estimate; Female: - 18 to 24 years: - Some college, no degree`+
                                  `Estimate; Female: - 18 to 24 years: - Associate's degree`+
                                  `Estimate; Female: - 25 to 34 years: - Some college, no degree`+
                                  `Estimate; Female: - 25 to 34 years: - Associate's degree`+
                                  `Estimate; Female: - 35 to 44 years: - Some college, no degree`+
                                  `Estimate; Female: - 35 to 44 years: - Associate's degree`+
                                  `Estimate; Female: - 45 to 64 years: - Some college, no degree`+
                                  `Estimate; Female: - 45 to 64 years: - Associate's degree`+
                                  `Estimate; Female: - 65 years and over: - Some college, no degree`+
                                  `Estimate; Female: - 65 years and over: - Associate's degree`)/`Estimate; Total:`*100, 2),
           BACHELOR.PLUS = round((`Estimate; Male: - 18 to 24 years: - Bachelor's degree`+
                                    `Estimate; Male: - 18 to 24 years: - Graduate or professional degree`+
                                    `Estimate; Male: - 25 to 34 years: - Bachelor's degree`+
                                    `Estimate; Male: - 25 to 34 years: - Graduate or professional degree`+
                                    `Estimate; Male: - 35 to 44 years: - Bachelor's degree`+
                                    `Estimate; Male: - 35 to 44 years: - Graduate or professional degree`+
                                    `Estimate; Male: - 45 to 64 years: - Bachelor's degree`+
                                    `Estimate; Male: - 45 to 64 years: - Graduate or professional degree`+
                                    `Estimate; Male: - 65 years and over: - Bachelor's degree`+
                                    `Estimate; Male: - 65 years and over: - Graduate or professional degree`+
                                    `Estimate; Female: - 18 to 24 years: - Bachelor's degree`+
                                    `Estimate; Female: - 18 to 24 years: - Graduate or professional degree`+
                                    `Estimate; Female: - 25 to 34 years: - Bachelor's degree`+
                                    `Estimate; Female: - 25 to 34 years: - Graduate or professional degree`+
                                    `Estimate; Female: - 35 to 44 years: - Bachelor's degree`+
                                    `Estimate; Female: - 35 to 44 years: - Graduate or professional degree`+
                                    `Estimate; Female: - 45 to 64 years: - Bachelor's degree`+
                                    `Estimate; Female: - 45 to 64 years: - Graduate or professional degree`+
                                    `Estimate; Female: - 65 years and over: - Bachelor's degree`+
                                    `Estimate; Female: - 65 years and over: - Graduate or professional degree`)/`Estimate; Total:`*100, 2)
    ) %>%
    select(170:173)
  
  household_2015 <- read_csv("2015 household.csv", skip = 1) 
  household_2015$`CENSUS TRACT` <- substr(household_2015$Id2, 6,11)
  household_2015$HOUSEHOLD <- household_2015$`Total; Estimate; Average household size`
  household_2015 <- household_2015[,c(204:205)]
  
  poverty_2015 <- read_csv("2015 poverty.csv", skip = 1) %>%
    mutate(`CENSUS TRACT` = substr(Id2, 6,11),
           POVERTY = `Estimate; Income in the past 12 months below poverty level:`/`Estimate; Total:`*100) %>%
    select(122:123)
  
  veteran_2015 <- read_csv("2015 veteran.csv", skip = 1) %>%
    mutate(`CENSUS TRACT` = substr(Id2, 6,11),
           VETERAN = ((`Estimate; Total: - Male: - 18 to 34 years: - Veteran`)+(`Estimate; Total: - Male: - 35 to 54 years: - Veteran`)+
                        (`Estimate; Total: - Male: - 55 to 64 years: - Veteran`)+(`Estimate; Total: - Male: - 65 to 74 years: - Veteran`)+
                        (`Estimate; Total: - Male: - 75 years and over: - Veteran`)+(`Estimate; Total: - Female: - 18 to 34 years: - Veteran`)+
                        (`Estimate; Total: - Female: - 35 to 54 years: - Veteran`)+
                        (`Estimate; Total: - Female: - 55 to 64 years: - Veteran`)+(`Estimate; Total: - Female: - 65 to 74 years: - Veteran`)+
                        (`Estimate; Total: - Female: - 75 years and over: - Veteran`))/`Estimate; Total:`*100)%>%
    select(82:83)
  
  employment2015 <- read_csv("2015 employment.csv", skip = 1) %>%
    mutate(`CENSUS TRACT` = substr(Id2, 6,11)) %>%
    mutate(EMPLOYMENT = `Employment/Population Ratio; Estimate; Population 16 years and over`) %>%
    select(284:285)
  
  disability2015 <- read_csv("disability data.csv") %>%
    mutate(`CENSUS TRACT`= as.character(`CENSUS TRACT`))
  
  language2015 <- read_csv("2015 language.csv",skip =1) %>% janitor::clean_names() %>%
    mutate(`CENSUS TRACT` = substr(id2, 6,11)) %>%
    mutate(`CENSUS TRACT`= as.character(`CENSUS TRACT`)) %>%
    select(contains("CENSUS TRACT"), contains("estimate")) %>%
    dplyr::select(c(contains("CENSUS TRACT"), ends_with("total"),contains("chinese"),
                    contains("spanish"),contains( "cambodian"),contains( "korean"),contains( "tagalog"),
                    contains("vietnamese"),contains( "japanese"),contains("hindi"),contains( "thai"),
                    contains( 'persian'),contains( "armenian"),contains( "russian"))) %>%
    select(-contains("english_very"))
  
  language2015 <- bind_cols(language2015[,1],round(language2015[,-c(1:2)]/rowSums(language2015[,2])*100,1))
  
  over182015 <- read_csv("2015 over18.csv", skip = 1) %>% janitor::clean_names() %>%
    mutate(`CENSUS TRACT` = substr(id2, 6,11)) %>%
    select(contains("CENSUS TRACT"), contains("total_estimate")) %>%
    mutate(over18 = 100-(total_estimate_selected_age_categories_5_to_14_years+
                           total_estimate_selected_age_categories_15_to_17_years)) %>%
    select(contains("CENSUS TRACT"), contains("over18"))
  
  citizenship2015 <- read_csv("2015 citizenship.csv", skip = 1) %>% janitor::clean_names() %>%
    mutate(`CENSUS TRACT` = substr(id2, 6,11),
           uscitizen = estimate_total-estimate_total_not_a_u_s_citizen) %>%
    select(contains("CENSUS TRACT"), contains("uscitizen"), ends_with("estimate_total"))
  
  over18.citizen <- left_join(over182015, citizenship2015, "CENSUS TRACT") %>%
    mutate(over18.citizen = uscitizen*(over18/100)/estimate_total) %>%
    select(contains("CENSUS TRACT"), contains("over18.citizen"))
  #leave over18.citizen as a factor to multiple by the language %
  
  acs_2015 <- left_join(combined_2015, education_2015, "CENSUS TRACT")
  acs_2015 <- left_join(acs_2015, poverty_2015, "CENSUS TRACT")
  acs_2015 <- left_join(acs_2015, household_2015, "CENSUS TRACT")
  acs_2015 <- left_join(acs_2015, veteran_2015, "CENSUS TRACT")
  acs_2015 <- left_join(acs_2015, employment2015, "CENSUS TRACT") 
  acs_2015 <- left_join(acs_2015, disability2015, "CENSUS TRACT")
  acs_2015 <- left_join(acs_2015, language2015, "CENSUS TRACT")
  acs_2015 <- left_join(acs_2015, over18.citizen, "CENSUS TRACT")
  
  acs_2015 <- unique(acs_2015)
  
  acs_2015 <- acs_2015[,c("CENSUS TRACT", "AGE", "FEMALE", "EMPLOYMENT", names(acs_2015[,c(19:49)]))]
  
}

#summarise stat per CENSUS TRACT
vf <- left_join(vf, pdist.censustract, "precinct_id") %>% 
  group_by(CENSUS.TRACT) %>%
  dplyr::summarise(total = sum(total),
                   voter.age = mean(voter.age,na.rm=T),
                   perm = mean(perm,na.rm=T),
                   female.pct=mean(female.pct,na.rm=T),
                   AR = mean(AR, na.rm=T),
                   CHN = mean(CHN, na.rm=T),
                   FAR = mean(FAR, na.rm=T),
                   HIN = mean(HIN, na.rm=T),
                   JPN = mean(JPN, na.rm=T),
                   KHM = mean(KHM, na.rm=T),
                   KOR = mean(KOR, na.rm=T),
                   RUS = mean(RUS, na.rm=T),
                   SPA = mean(SPA, na.rm=T),
                   TAG = mean(TAG, na.rm=T),
                   THA = mean(THA, na.rm=T),
                   VTN = mean(VTN, na.rm=T))

vf <- left_join(vf, acs_2015, by = c("CENSUS.TRACT"="CENSUS TRACT")) #join with ACS 2015 data



########################################################################################## 

#SVC primary and general 2016 (lavote.net)
setwd("\\\\sfile2/ExecOffice/JordanBerninger/GIS_clustering")

## NEED TO GRAB ALL THE FILES BELOW AND SAME THEM TO THE APPROPRIATE WORKING DIRECTORY IN MY FOLDER IN THE E-DRIVE

#general election
general2016 <- read_excel("PRES_AND_VICE_PRES_11-08-16_by_Precinct_3496-4802.xls", skip=2) %>%
  filter(TYPE == "POLLING PLACE",
         REGISTRATION != 0) %>%
  select(2,6,8) %>%
  mutate(election_id = 3496,
         TURNOUT = `BALLOTS CAST`/REGISTRATION) %>%
  filter(TURNOUT <=1)

#bind all parties primary election result
primary2016.AI <- read_excel("PRESIDENTIAL_PREFERENCE_06-07-16_American_Independent_by_Precinct_980-4132.xls", skip=2) %>%
  filter(TYPE == "POLLING PLACE",
         REGISTRATION != 0) %>%
  select(2,6,8) 

primary2016.DEM <- read_excel("PRESIDENTIAL_PREFERENCE_06-07-16_Democratic_by_Precinct_980-4129.xls", skip=2)%>%
  filter(TYPE == "POLLING PLACE",
         REGISTRATION != 0) %>%
  select(2,6,8) 

primary2016.GRN <- read_excel("PRESIDENTIAL_PREFERENCE_06-07-16_Green_by_Precinct_980-4133.xls", skip=2)%>%
  filter(TYPE == "POLLING PLACE",
         REGISTRATION != 0) %>%
  select(2,6,8) 

primary2016.LIB <- read_excel("PRESIDENTIAL_PREFERENCE_06-07-16_Libertarian_by_Precinct_980-4134.xls", skip=2)%>%
  filter(TYPE == "POLLING PLACE",
         REGISTRATION != 0) %>%
  select(2,6,8)

primary2016.PF <- read_excel("PRESIDENTIAL_PREFERENCE_06-07-16_Peace_and_Freedom_by_Precinct_980-4135.xls", skip=2)%>%
  filter(TYPE == "POLLING PLACE",
         REGISTRATION != 0) %>%
  select(2,6,8) 

primary2016.REP <- read_excel("PRESIDENTIAL_PREFERENCE_06-07-16_Republican_by_Precinct_980-4131.xls", skip=2)%>%
  filter(TYPE == "POLLING PLACE",
         REGISTRATION != 0) %>%
  select(2,6,8) 

primary2016 <- bind_rows(primary2016.AI, primary2016.DEM,primary2016.GRN,primary2016.LIB,
                         primary2016.PF,primary2016.REP) %>%
  group_by(PRECINCT) %>%
  summarise_all(funs(sum)) %>%
  mutate(election_id = 980,
         TURNOUT = `BALLOTS CAST`/REGISTRATION )
## CHANGED SUMMARISE_EACH() TO SUMMARISE_ALL()


#get consolidated precinct from PrecinctDistrict to join with SVC of primary and general 2016
system.time(pdist.precinct_db <- sqlQuery(channel,"SELECT * FROM PrecinctDistrict 
                                          WHERE election_id IN ('980','3496')")) #1 min

pdist.precinct <- pdist.precinct_db %>%
  select(4,7,1,5) %>% 
  mutate(index = paste(election_id, precinct_id, sep = "_"))  %>% 
  #create an index to join with pid/CENSUS TRACT data 
  unique()%>%
  mutate(precinct_id = as.character(precinct_id))

pdist1 <- left_join(pdist, pdist.precinct[,3:5],"index") %>%
  filter(votes_by_mail == "N") %>%
  filter(CENSUS.TRACT != "NANA")
#output includes CENSUS TRACT, pid, and consolidated precinct

general2016 <- left_join(general2016, 
                         pdist1[c(1,4,5)] %>% filter(election_id ==3496) %>% select(2,3), 
                         by = c("PRECINCT"="consolidation")) %>%
  unique() %>%
  group_by(CENSUS.TRACT) %>%
  dplyr::summarise(mean(TURNOUT, na.rm=T)) %>%
  setnames(2, "GENERAL 2016 TURNOUT")

primary2016 <- left_join(primary2016, 
                         pdist1[c(1,4,5)] %>% filter(election_id ==980) %>% select(2,3), 
                         by = c("PRECINCT"="consolidation")) %>%
  unique()%>%
  group_by(CENSUS.TRACT) %>%
  dplyr::summarise(mean(TURNOUT, na.rm=T)) %>%
  setnames(2, "PRIMARY 2016 TURNOUT")


vf <- left_join(vf, general2016, "CENSUS.TRACT") %>%
  left_join(primary2016, "CENSUS.TRACT")

###################################################################################
#kmean clustering by census tract centroid

#get census tracts & centroid lat/long
api.key.install(key = "0058ab93da8b64b6b5d316d94bb26a206b2638f4")
tracts <- tracts(state= 'CA', county= 37, year = 2015)

###################################################################################
## Just need to run from here down to make a new map after loading all the data ##
###################################################################################

set.seed(11235813) #try different seed to see the distribution of cluster
cluster.n <- 100

#get tract # and lon/lat for kmean clustering
centroid.data <- as.data.frame(cbind(tracts@data$TRACTCE, 
                                     tracts@data$INTPTLAT, tracts@data$INTPTLON)) %>%
  setnames(c("CENSUS.TRACT", "INT.LAT", "INT.LONG")) %>%
  mutate(INT.LAT = as.numeric(as.character(INT.LAT)),
         INT.LONG = as.numeric(as.character(INT.LONG)))

# use kmeans to create cluster
km <- kmeans(centroid.data[,2:3], centers = cluster.n )  
km.kcca = as.kcca(km, centroid.data[,2:3])
cluster.train <- predict(km.kcca)
centroid.data <- mutate(centroid.data, cluster = cluster.train) %>%
  mutate(cluster = as.factor(cluster))

census.map <- left_join(vf, centroid.data, "CENSUS.TRACT") #join tract data with voter data
census.map1 <- geo_join(tracts, census.map, "TRACTCE","CENSUS.TRACT") #keep a new tract data for total voter density map

#summarise cluster info by cluster #
cluster.info <- group_by(census.map, cluster) %>%
  dplyr:: summarise(
    sum(total, na.rm= T),
    round(mean(voter.age, na.rm= T), 1),
    round(mean(perm, na.rm= T), 1),
    round(mean(female.pct, na.rm= T), 1),
    round(mean(AGE, na.rm= T), 1),
    round(mean(FEMALE, na.rm=T), 1),
    round(mean(EMPLOYMENT, na.rm= T), 1),
    round(mean(`GENERAL 2016 TURNOUT`, na.rm= T), 2),
    round(mean(`PRIMARY 2016 TURNOUT`, na.rm= T), 2),
    sum(n(), na.rm= T),
    
    round(mean(hearing.per, na.rm =T),1),
    round(mean(vision.per, na.rm =T),1),
    round(mean(cognitive.per, na.rm =T),1),
    round(mean(ambulatory.per, na.rm =T),1),
    round(mean(selfcare.per, na.rm =T),1),
    round(mean(indliving.per, na.rm =T),1),
    
    round(mean(CHN, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_chinese, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_chinese_speak_english_less_than_very_well, na.rm=T),1),
    
    round(mean(FAR, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_persian, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_persian_speak_english_less_than_very_well, na.rm=T),1),
    
    round(mean(HIN, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_hindi, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_hindi_speak_english_less_than_very_well, na.rm=T),1),
    
    round(mean(AR, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_armenian, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_armenian_speak_english_less_than_very_well, na.rm=T),1),
    
    round(mean(JPN, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_japanese, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_japanese_speak_english_less_than_very_well, na.rm=T),1),
    
    round(mean(KHM, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_mon_khmer_cambodian, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_mon_khmer_cambodian_speak_english_less_than_very_well, na.rm=T),1),
    
    round(mean(KOR, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_korean, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_korean_speak_english_less_than_very_well, na.rm=T),1),
    
    round(mean(RUS, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_russian, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_russian_speak_english_less_than_very_well, na.rm=T),1),
    
    round(mean(SPA, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_spanish_or_spanish_creole, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_spanish_or_spanish_creole_speak_english_less_than_very_well, na.rm=T),1),
    
    round(mean(TAG, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_tagalog, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_tagalog_speak_english_less_than_very_well, na.rm=T),1),
    
    round(mean(THA, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_thai, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_thai_speak_english_less_than_very_well, na.rm=T),1),
    
    round(mean(VTN, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_vietnamese, na.rm=T),1),
    round(mean(over18.citizen*estimate_total_vietnamese_speak_english_less_than_very_well, na.rm=T),1)
    
  ) %>%
  setnames(c("cluster", "total.voter", "voter.age", "perm.pct", "female.pct","CT.age",
             "CT.female.pct", "CT.employment.pct", "g16.turnout",
             "p16.turnout", "total.CT", "disability.hearing", "disability.vision", "disability.cognitive",
             "disability.ambulatory", "disability.selfcare", "disability.indliving",
             "voter.chinese", "CT.chinese", "CT.chinese.less","voter.persian", "CT.persian", "CT.persian.less",
             "voter.hindi", "CT.hindi", "CT.hindi.less","voter.armenian", "CT.armenian", "CT.armenian.less",
             "voter.japanese", "CT.japanese", "CT.japanese.less",
             "voter.cambodian", "CT.cambodian", "CT.cambodian.less","voter.korean", "CT.korean", "CT.korean.less",
             "voter.russian", "CT.russian", "CT.russian.less","voter.spanish", "CT.spanish", "CT.spanish.less",
             "voter.tagalog", "CT.tagalog", "CT.tagalog.less","voter.thai", "CT.thai", "CT.thai.less",
             "voter.vietnamese", "CT.vietnamese", "CT.vietnamese.less")) 

cluster.info <- cluster.info[-nrow(cluster.info),]

census.map <- left_join(census.map[,c(1,54:56)], cluster.info, "cluster")
census.map <- tigris::geo_join(tracts, census.map, "TRACTCE", "CENSUS.TRACT") #join with tract data (SPDF) for mapping purpose


#create color palette (no need anymore since clusters are bolded by borders now)
(col.pal <- gsub("[0-9]*", "", colors()) %>% unique())
(col.pal <- gsub(paste(c("medium", "light",  "dim", "pale"), 
                       collapse= "|"), "", col.pal) %>% unique()) #collapse allows to gsub multiple terms pattern
col.pal <- grep(paste(c(palette()[-c(2,3,7)], "green", "pink", "purple"), collapse = "|"), col.pal, value = T) 
#select darker color

set.seed(112358)
pal <- colorFactor(sample(col.pal,cluster.n),census.map@data$cluster)

#create pie image for col legend
#pie(x= rep(1,35), labels = 1:cluster.n,  col = sample(col.pal,cluster.n), radius = 1)

#output pie chart color for legend purpose
#jpeg(filename="pie-legend.jpg", width = 320, height = 320)
#set.seed(seed.n)
#pie(x= rep(1,cluster.n), labels = 1:cluster.n,col = sample(col.pal,cluster.n), radius = 1)
#dev.off()

#voter density color legend
pal1 <- colorNumeric("YlOrRd", census.map1$total) #color chart for total voter dentsity

#get a list of city coord
city.list <- read_csv("city list.csv") 
city.list <- unique(city.list$city)
city.list <- city.list %>% as.data.frame() %>% setnames(1, "city")
city.list.coord <- geocode(paste(city.list$city, "CA, USA", sep = ","), source = "google")
city.list <- bind_cols(as.data.frame(city.list), city.list.coord) %>%
  filter(city.list %nin% c("PIONEER", "ANTELOPE", "FRANKLIN", "GREEN VALLEY", "BELVEDERE", "OCEAN VIEW",
                           "GILMORE", "ROYAL OAKS", "WILSONA", "VETERANS ADMIN CENTER",
                           "MALIBU HEIGHTS", "FOOTHILL", "LA RAMBLA", "RAMONA", "DESERT", "CENTINELA"))
city.list[city.list$city %in% c("Burbank", "Glendale", "Lawndale", "Westlake Village"),]$lon <- c(-118.309,-118.2551,-118.3526,-118.8074)
city.list[city.list$city %in% c("Burbank", "Glendale", "Lawndale", "Westlake Village"),]$lat <- c(34.18084,34.14251,33.88724,34.14665)


#calculate cluster center using man and min long-lat
#get the max an dmin lon/lat of a cluster and take a midpoint 
centroid.list <- census.map@data %>% group_by(cluster) %>% 
  dplyr::summarise(max.centroid.lat = max(INT.LAT),
                   min.centroid.lat = min(INT.LAT),
                   max.centroid.long = max(INT.LONG),
                   min.centroid.long = min(INT.LONG)) %>%
  mutate(centroid.lat = (max.centroid.lat + min.centroid.lat)/2,
         centroid.long = (max.centroid.long + min.centroid.long)/2) %>%
  slice(-101)
## DONT KNOW WHY 2 CLUSTERS ARE MISSING FROM THIS
## when i change the number of clusers, I need to update this last slice() argument to slice(-(k+1)) where k = number of clusters

#reverse geocode those cluster centers to get physical addresses
system.time(address.cluster <- do.call(rbind,lapply(1:nrow(centroid.list),
                                                    function(i) revgeocode(as.numeric(centroid.list[i,7:6])))))

## this hit an error

#any address with Unnamed Road --> Unknown
#join the midpoint with the physical address
centroid.list <- cbind(centroid.list, address.cluster) %>% 
  separate(address, sep = ",", c("add", "city.center", "CA zip", "USA"), remove = F) %>%
  select(-c(9,11,12)) %>%
  mutate(city.center = ifelse(address.cluster == "Unnamed Road, California, USA", "Unknown", city.center))

#join with the overall map data
census.map <- geo_join(census.map, centroid.list[,c(1,9)], "cluster", "cluster")

#create popup content
popup.content <- paste(sep = "<br/>", 
                       paste("Cluster", census.map@data$cluster),
                       paste(census.map@data$total.CT, "tracts"),
                       paste("Center city:", census.map@data$city.center, "<br/>"),
                       paste("Total voter:", census.map@data$total.voter),
                       paste("Avg voter age:", round(census.map1@data$voter.age,1)),
                       paste("Gender (voterfile):", census.map@data$female.pct, "% F",
                             "/", 100-census.map@data$female.pct, "% M"),
                       paste("PVBM voter:", census.map@data$perm.pct, "%"),
                       paste("General 2016 election turnout:", census.map@data$g16.turnout),
                       paste("Primary 2016 election turnout:", census.map@data$p16.turnout),
                       paste("Avg cluster age:", census.map@data$CT.age),
                       paste("Gender:", census.map@data$CT.female.pct, "% F",
                             "/", 100-census.map@data$CT.female.pct, "% M"),
                       paste("Employment:", census.map@data$CT.employment.pct, "%"),
                       "</br> Disability types",
                       paste("<style>
                             table, th, td {
                             border: 1px solid black;
                             }
                             </style>"),
                       paste("<table>", "<tr>", "<th>Hearing</th>",
                             "<th>Vision</th>",
                             "<th>Cognitive</th>",
                             "<th>Ambulatory</th>",
                             "<th>Self-care</th>",
                             "<th>Ind living</th>", "</tr>",
                             "<tr>", "<td>", census.map@data$disability.hearing, "%</td>",
                             "<td>", census.map@data$disability.vision, "% </td>",
                             "<td>", census.map@data$disability.cognitive, "% </td>",
                             "<td>", census.map@data$disability.ambulatory, "% </td>",
                             "<td>", census.map@data$disability.selfcare, "% </td>",
                             "<td>", census.map@data$disability.indliving, "% </td>","</tr>","</table>"),
                       
                       paste("<table>", "<tr>", "<th>Language</th>",
                             "<th>voterfile (preferred lang)</th>",
                             "<th>total population speaking the language</br>
                             (18+ citizen)</th>",
                             "<th>population speak english less than very well </br>
                             (18+ citizen)</th>", "</tr>",
                             
                             "<tr>", "<td>", "ARMENIAN", "</td>",
                             "<td>", census.map@data$voter.armenian, "%</td>",
                             "<td>", census.map@data$CT.armenian, "%</td>",
                             "<td>", census.map@data$CT.armenian.less, "%</td>","</tr>",
                             
                             "<tr>", "<td>", "CHINESE", "</td>",
                             "<td>", census.map@data$voter.chinese, "%</td>",
                             "<td>", census.map@data$CT.chinese, "%</td>",
                             "<td>", census.map@data$CT.chinese.less, "%</td>","</tr>",
                             
                             "<tr>", "<td>", "KHMER/</br>CAMBODIAN", "</td>",
                             "<td>", census.map@data$voter.cambodian, "%</td>",
                             "<td>", census.map@data$CT.cambodian, "%</td>",
                             "<td>", census.map@data$CT.cambodian.less, "%</td>","</tr>",
                             
                             "<tr>", "<td>", "KOREAN", "</td>",
                             "<td>", census.map@data$voter.korean, "%</td>",
                             "<td>", census.map@data$CT.korean, "%</td>",
                             "<td>", census.map@data$CT.korean.less, "%</td>","</tr>",
                             
                             "<tr>", "<td>", "SPANISH", "</td>",
                             "<td>", census.map@data$voter.spanish, "%</td>",
                             "<td>", census.map@data$CT.spanish, "%</td>",
                             "<td>", census.map@data$CT.spanish.less, "%</td>","</tr>",
                             
                             "<tr>", "<td>", "TAGALOG", "</td>",
                             "<td>", census.map@data$voter.tagalog, "%</td>",
                             "<td>", census.map@data$CT.tagalog, "%</td>",
                             "<td>", census.map@data$CT.tagalog.less, "%</td>","</tr>",
                             
                             "<tr>", "<td>", "VIETNAMESE", "</td>",
                             "<td>", census.map@data$voter.vietnamese, "%</td>",
                             "<td>", census.map@data$CT.vietnamese, "%</td>",
                             "<td>", census.map@data$CT.vietnamese.less, "%</td>","</tr>",
                             
                             "<tr>", "<td>", "JAPANESE", "</td>",
                             "<td>", census.map@data$voter.japanese, "%</td>",
                             "<td>", census.map@data$CT.japanese, "%</td>",
                             "<td>", census.map@data$CT.japanese.less, "%</td>","</tr>",
                             
                             "<tr>", "<td>", "HINDI", "</td>",
                             "<td>", census.map@data$voter.hindi, "%</td>",
                             "<td>", census.map@data$CT.hindi, "%</td>",
                             "<td>", census.map@data$CT.hindi.less, "%</td>","</tr>",
                             
                             "<tr>", "<td>", "THAI", "</td>",
                             "<td>", census.map@data$voter.thai, "%</td>",
                             "<td>", census.map@data$CT.thai, "%</td>",
                             "<td>", census.map@data$CT.thai.less, "%</td>","</tr>",
                             
                             "<tr>", "<td>", "PERSIAN", "</td>",
                             "<td>", census.map@data$voter.persian, "%</td>",
                             "<td>", census.map@data$CT.persian, "%</td>",
                             "<td>", census.map@data$CT.persian.less, "%</td>","</tr>",
                             
                             "<tr>", "<td>", "RUSSIAN", "</td>",
                             "<td>", census.map@data$voter.russian, "%</td>",
                             "<td>", census.map@data$CT.russian, "%</td>",
                             "<td>", census.map@data$CT.russian.less, "%</td>", "</tr>","</table>"))


#use dissolve function
census.map.border <- census.map
census.map.border <- census.map.border[is.na(census.map.border$cluster)==F,]
lps <- unionSpatialPolygons(census.map.border, census.map.border@data$cluster.1)
########################################################################################## 

city.icon <- makeIcon(iconUrl = "http://i.imgur.com/NHA3Zkl.png",
                      iconWidth = 25, iconHeight = 25,
                      iconAnchorX = 25, iconAnchorY = 25)

cluster.icon <- makeIcon(iconUrl = "https://cdn1.iconfinder.com/data/icons/orientation-2/32/location-256.png",
                         iconWidth = 18, iconHeight = 18,
                         iconAnchorX = 18, iconAnchorY = 18)

icon.legend <- "<img src= 'https://image.ibb.co/kgt8BR/LEGEND.png'/>"

map100_jrb <- leaflet() %>%
  addProviderTiles("CartoDB.Positron")  %>% 
  
  #legend for voter density
  addLegend("bottomright", pal = pal1, values = census.map1@data$total,title = "Voter Density")  %>%
  
  # selection box
  addLayersControl(overlayGroups = c("City", "Cluster Center"),
                   options = layersControlOptions(collapsed = FALSE)) %>%
  
  addControl(html = icon.legend, position = "bottomleft") %>%
  
  # voter density layer
  addPolygons(data = census.map1, fillColor = ~pal1(total), stroke = F,smoothFactor = 0.5,
              fillOpacity = 0.8 , popup= ~popup.content )  %>%
  
  # city center
  addMarkers(data = city.list, lng = ~lon, lat = ~lat, popup = ~city, icon = city.icon,
             group = "City") %>% 
  
  # cluster center
  addMarkers(data = centroid.list, lng = ~centroid.long, lat = ~centroid.lat, 
             popup = ~address.cluster, icon = cluster.icon,
             group = "Cluster Center") %>% 
  
  #add border 
  addPolygons(data = lps , fill = F, color = "black")


#the below map is same as above (just bolded provider tile layer)
map100_jrb_updated <- map100_jrb %>% addProviderTiles(providers$MtbMap)  %>%
  addProviderTiles(providers$Stamen.TonerLabels)

#############################################################################################################

# there is an oversaturation in the populated city center as compared to the spoarsely populated north towns

# Possible solutions
## we already have 200 extra voting centers to distribute, we can use them to divide up the biggest clusters (size or population)
### this was part of the design of the analysis at the beginning
## We can pre-divide the county into: City + Northtowns OR 5 supervisorial districts
### distribute the resources for the pre-groups, then do seperate cluster analyses
## instead of clustering the census tracts, cluster the old voting centers (GIS has this data)
## do the color coding of this raster map as total number of voters, not voter density?

# histogram of the populations for each cluster
# add the supervisorial district boundaries
# that data is probably in PrecinctDistrict_GroupType or PrecinctDistrict

library(plotly)
p1 <- ggplot(data = cluster.info, aes(x = total.voter, fill = as.factor(total.CT))) + geom_histogram(bins = 50)
ggplotly(p1, main = "Total Voter, Number of tracts in Cluster")

p2 <- ggplot(data = cluster.info, aes(x = perm.pct, fill = as.factor(total.CT))) + geom_histogram(bins = 50)
ggplotly(p2)

p3 <- ggplot(data = cluster.info, aes(x = p16.turnout, fill = as.factor(total.CT))) + geom_histogram(bins = 50)
ggplotly(p3)

# how to add in SV Districts - pdist.censustract object, join in another column and migrate it into cluster.info
table(cluster.info$total.CT) %>% View()

cluster.info %>% filter(total.voter < 1750) 
# 14 cluster are less than 1750 voters

cluster.info %>% filter(total.voter > 11000) 
# 145 clusters have more than 11K voters, these all have more than 3 or more census tracts
# we get 2 random super cluster of 13 census tracts that are +30K total voters

## maybe map the 2 separately
## cant view the 2 different leaflets simltaneously because they are based off dataframes with the same name (cluster.info)
