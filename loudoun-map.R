library(tidyverse)
library(urbnmapr)
library(rworldmap)
library(readxl)
library(rgdal)
library(sf)
library(ggplot2)
library(dplyr)
library(lubridate)



## getting county data for just loudoun 
l <- left_join(countydata, counties, by = "county_fips") %>% 
  filter(state_name %in% c("Virginia"), county_name %in% c("Loudoun County"))


l %>%
  ggplot(aes(long, lat, group = group)) +
  geom_polygon(color = "white", size = 0.05, fill = "light green") +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  theme(legend.position = "right",
        legend.direction = "vertical", 
        legend.title = element_text(face = "bold", size = 11),
        legend.key.height = unit(.25, "in")) +
  ggtitle("Loudoun County") 


## reading in the shape files using town boundaries zip 
town <- readOGR( 
  dsn= "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/Loudoun-County/2021_DSPG_Loudoun/Loudoun_Town_Boundaries" , 
  layer="Loudoun_Town_Boundaries"
)

t_df <- as(town, "data.frame")
t_df2 <- fortify(town)


t_df2 %>%
ggplot(aes(long, lat, group = group)) +
  geom_polygon(color = "white", size = 0.05, fill = "light blue")


## then can read in the excel sheet with all of the programs lat/long and plot onto map 



intake_race <- read_csv(paste0(getwd(), "/data/DJJ-2020-Juvenile_Detention_Locality-Race_Intake.csv")) 
colnames(intake_race) <-intake_race[1,]
intake_race <-intake_race[-1,]
jv_race <- intake_race %>% select(RACE, `FY20 %`, CSU) %>% 
  filter(CSU == "20L") %>% 
  rename(Proportion = `FY20 %`) %>% 
  select(RACE, Proportion) %>% 
  rename(Race = RACE)



