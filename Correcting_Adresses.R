library(tidyverse)
library(readxl)
library(ggmap)

# Read in combined_programs info 
combined_programs <- read_excel("combined-programs.xlsx")

# Remove current address column and reorder Longitude and Latitude to match googles
combined_programs <- combined_programs %>%  
  select(-Address) %>% relocate(Latitude, .after = Longitude)
register_google(key = "AIzaSyC4PUO9ekHQBloUR2PtVfaBfdXdqRurk_w")

# Use reverse geocode to obtain addresses 
Addresses <- do.call(rbind, lapply(1:nrow(combined_programs), 
                                     function(x) revgeocode(as.numeric(combined_programs[x, 9:10]))))
Addresses <- as_tibble(Addresses) %>% rename(Address = V1)

#Add Address column and put in Adresses 
combined_programs <- combined_programs %>% mutate(Address = as_vector(Addresses)) 

# Write to csv
# I just copied pasted the column from below into the excel .xlsx
write_csv(combined_programs, "combined_programs.csv")

