library(tidyverse)
library(readxl)

programs <- read_excel("combined-programs.xlsx")

pillar_count <- programs %>% 
  group_by(County) %>% 
  count(Pillars) %>% 
  pivot_wider(names_from = County, values_from = n) %>% 
  relocate(Loudoun, .after = Pillars)
subpop_count <- programs %>% 
  group_by(County) %>% 
  count(Subpopulation) %>% 
pivot_wider(names_from = County, values_from = n) %>% 
  relocate(Loudoun, .after = Subpopulation)


