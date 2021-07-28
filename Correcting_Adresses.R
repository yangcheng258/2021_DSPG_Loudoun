library(tidyverse)
library(readxl)
library(ggmap)

combined_programs <- read_excel("combined-programs.xlsx")
combined_programs <- combined_programs %>%  
  select(-Address) %>% relocate(Latitude, .after = Longitude)
register_google(key = "AIzaSyC4PUO9ekHQBloUR2PtVfaBfdXdqRurk_w")

Addresses <- do.call(rbind, lapply(1:nrow(combined_programs), 
                                     function(x) revgeocode(as.numeric(combined_programs[x, 9:10]))))
Addresses <- as_tibble(Addresses) %>% rename(Address = V1)
combined_programs <- combined_programs %>% mutate(Address = as_vector(Addresses)) 
write_csv(combined_programs, "combined_programs.csv")

