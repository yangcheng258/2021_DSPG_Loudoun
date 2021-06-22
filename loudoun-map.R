## creating maps 
library(tidyverse)
library(urbnmapr)
library(rworldmap)
library(readxl)

## getting county data for just floyd 
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









