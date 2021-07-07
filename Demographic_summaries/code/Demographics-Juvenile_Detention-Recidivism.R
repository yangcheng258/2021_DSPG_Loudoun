library(tidyverse)
library(tabulizer)

# Creating table for Recidivism in Virginia
djj_recidivism_link <- "http://www.djj.virginia.gov/pdf/about-djj/DRG/Recidivism.pdf"
tables <- extract_tables(djj_recidivism_link, pages = 3)
recidivism_table <- tables[[2]]

# Edited table directly in excel, then used further manipulation
recidivism_table <- read_csv("C:\\Users\\austi\\Documents\\djj_table.csv")
recidivism_table_final <- recidivism_table %>% pivot_longer(
  cols = 3:6, names_to = "Year", values_to = "Rate") %>% 
  mutate(Rate = as.numeric(str_remove(Rate, "%"))) %>% 
  mutate(Rate = Rate/100)

# Rates of Rearrest/Reconviction of Probation Placements over time in state
recidivism_graph <- recidivism_table_final %>%
  ggplot(aes(x = Year, y = Rate, color = `Recidvism Type`, group = `Recidvism Type`,
             shape = `Recidvism Type`)) + facet_wrap(~Placement) + geom_point() + 
  geom_line() + labs(title = "Recidivism Rates by Placement Type", 
                     caption = "Source: DJJ Data Resource Guide FY 2020")  + 
       scale_y_continuous(labels = scales::percent) +
       theme_bw()

# Create table for recidivism rates in Loudoun County 

#Get links
link_recidivism_2018to19 <- "http://www.djj.virginia.gov/pdf/about-djj/DRG/Recidivism.pdf"
link_recidivism_2017to18 <- "http://www.djj.virginia.gov/pdf/about-djj/DRG/FY19_DRG.pdf"
link_recidivism_2016to17 <- "http://www.djj.virginia.gov/pdf/about-djj/DRG/FY18_DRG.pdf"
link_recidivism_2015to16 <- "http://www.djj.virginia.gov/pdf/about-djj/DRG/FY17_DRG.pdf"
link_recidivism_2014to15 <- "http://www.djj.virginia.gov/pdf/about-djj/DRG/FY16_DRG.pdf"
link_recidivism_2013to14 <- "http://www.djj.virginia.gov/pdf/about-djj/DRG/FY2015_DRG.pdf"
link_recidivism_2012to13 <- "http://www.djj.virginia.gov/pdf/about-djj/DRG/FY2014_DRG.pdf"

# Raw tables 
tables_2012to13 <- extract_tables(link_recidivism_2012to13, pages = c(61, 67))
tables_2013to14 <- extract_tables(link_recidivism_2013to14, pages = c(67, 73, 74))
tables_2014to15 <- extract_tables(link_recidivism_2014to15, pages = c(73, 77, 78))
tables_2015to16 <- extract_tables(link_recidivism_2015to16, pages = c(77, 83, 84))
tables_2016to17 <- extract_tables(link_recidivism_2016to17, pages = c(90, 96, 97))
tables_2017to18 <- extract_tables(link_recidivism_2017to18, pages = c(94, 100, 101),
                                  method = 'lattice')
tables_2018to19 <- extract_tables(link_recidivism_2018to19, pages = c(5, 9, 10))


