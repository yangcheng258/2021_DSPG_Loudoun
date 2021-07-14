library(tidyverse)
library(readxl)

programs <- read_excel("combined-programs.xlsx")


count_programs_by_pillar <- function(county) {
  county %>% count(Pillars) 
}

count_programs_by_subpop <- function(county) {
  county %>% count(Subpopulation)
}

programs_loudoun <- programs %>% filter(County == "Loudoun")
loudoun_pillar_counts <- count_programs_by_pillar(programs_loudoun) %>% 
  mutate(County = "Loudoun") %>% relocate(County)
loudoun_subpop_counts <- count_programs_by_subpop(programs_loudoun) %>% 
  mutate(County = "Loudoun") %>% relocate(County)


programs_fairfax <- programs %>% filter(County == "Fairfax")
fairfax_pillar_counts <- count_programs_by_pillar(programs_fairfax) %>% 
  mutate(County = "Fairfax") %>% relocate(County)
fairfax_subpop_counts <- count_programs_by_subpop(programs_fairfax) %>% 
  mutate(County = "Fairfax") %>% relocate(County)

programs_allegheny <- programs %>% filter(County == "Allegheny")
allegheny_pillar_counts <- count_programs_by_pillar(programs_allegheny) %>% 
  mutate(County = "Allegheny") %>% relocate(County)
allegheny_subpop_counts <- count_programs_by_subpop(programs_allegheny) %>% 
  mutate(County = "Allegheny") %>% relocate(County)

pillar_counts_by_county <- 
  bind_rows(loudoun_pillar_counts, fairfax_pillar_counts, allegheny_pillar_counts) %>% 
  pivot_wider(names_from = County, values_from = n) %>% 
  drop_na()

subpop_counts_by_county <- 
  bind_rows(loudoun_subpop_counts, fairfax_subpop_counts, allegheny_subpop_counts) %>% 
  pivot_wider(names_from = County, values_from = n) 


