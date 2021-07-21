library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(maps)
library(plotly)
library(dplyr)
library(tigris)
library(tidyverse)
library(tidycensus)
library(readxl)
library(collapsibleTree)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras)
library(rvest)
library(sf)
library(shinydashboard)
library(shinydashboardPlus)
options(tigris_use_cache = TRUE)

# census_api_key("6f1a78212175773dd80d1a03bd303e8d181a6096", install = TRUE, overwrite = T)
# readRenviron("~/.Renviron")

#Yang's API Key
census_api_key("58cb9357dee9edf8330e47865d207929ab8baeb3", install = FALSE )
Sys.getenv("CENSUS_API_KEY")
# I am seeting my working directory
# setwd("G:/My Drive/PhD/Internship/Loudoun/2021_DSPG_Loudoun/2021_DSPG_Loudoun/ShinyApp")

# Data-----------------------------------------------------------
## gender and age tays
males_tays <- c('B01001_007','B01001_008','B01001_009','B01001_010')
females_tays <- c('B01001_031','B01001_032','B01001_033','B01001_034') 

l_ages_gender <- get_acs(
  geography = 'county', 
  variables = c(males_tays, females_tays) , 
  state= 'VA',
  county='Loudoun', 
  summary_var = 'B01001_001'
) 

# race tays 
white <- 20066
black <- 2403
indian <- 82 
asian <- 3891
native_hawaiian <- 79
other <- 425

race <- data.frame(rbind(white, black, indian, asian, native_hawaiian, other))
colnames(race) <- "Estimate"
race$Race <- c("White", "Black", "Indian", "Asian", "Native Hawaiian", "Other")

# education 
male_edu <- c("B15001_004","B15001_005","B15001_006","B15001_007","B15001_008","B15001_009","B15001_010") 
female_edu <- c("B15001_045", "B15001_046", "B15001_047", "B15001_048", "B15001_049","B15001_050","B15001_051") 
education <- get_acs(geography = 'county', 
                     variables = c(male_edu, female_edu), 
                     state= 'VA',
                     county='Loudoun')

school <- c("Less than 9th", "9th to 12th (no diploma)", "High school diploma", "Some college (no degree)", "Associate's degree", "Bachelor's degree", "Graduate or professional degree")
medu <- education[1:7,]

medu <- medu%>%
  dplyr::select(variable, estimate)

medu$variable <- school

fedu <- education[8:14,]

fedu <- fedu%>%
  dplyr::select(estimate)

## Poverty 
w <- c("B17001A_010","B17001A_024") 
b <- c("B17001B_010", "B17001B_024") 
i <- c( "B17001C_010", "B17001C_024")
as <- c("B17001D_010", "B17001D_024")
n <- c("B17001E_010", "B17001E_024")
o <- c("B17001F_010", "B17001F_024")

poverty <- get_acs(
  geography = 'county', 
  variables = c(w,b,i,as,n,o) , 
  state= 'VA',
  county='Loudoun'
)

w_p <- data.frame(poverty[1,], poverty[2,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "White")
b_p <- data.frame(poverty[3,], poverty[4,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "Black")
i_p <- data.frame(poverty[5,], poverty[6,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "Indian")
as_p <- data.frame(poverty[7,], poverty[8,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "Asian")
n_p <- data.frame(poverty[9,], poverty[10,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "Native")
o_p <- data.frame(poverty[11,], poverty[12,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "Other")

# Health Care
covered <- 1473 
private_cov <- 25418
medicare <- 58 
va <- 69

healthcare <- data.frame(rbind(covered, private_cov, medicare, va))
healthcare$Type <- c("Public", "Private", "Medicare", "VA Health")
colnames(healthcare) <- c("Estimate", "Type")


# Mental Illness
smiwaitlist <- read_excel(paste0(getwd(),"/data/smi-waitlist.xlsx") ) 
smi <- smiwaitlist[6:20,1:3]
colnames(smi) <- c("Group", "Year", "Persons")
smi$Persons <- as.numeric(smi$Persons)

waitlist <- smiwaitlist[6:25,9:11]
colnames(waitlist) <- c( "Program", "Year", "Persons")

waitlist$Persons <- as.numeric(waitlist$Persons)

#ADult Literacy Individuals Served
classroom <- 396
tutoring <- 20
jobsite <- 49 
ged <- 2 
workforce <- 105 
#Medicaid
enroll <- read_excel(paste0(getwd(),"/data/medicaid-enrollment.xlsx")) 
total <- enroll[1:13,]
total$`Adults, Pregnant Women and Children` <- as.numeric(total$`Adults, Pregnant Women and Children`)
med <- enroll[16:20,1:3]
colnames(med) <- c("Year", "Children", "Childless Adults")
med$`Childless Adults` <- as.numeric(med$`Childless Adults`)
med$Children <- as.numeric(med$Children)

# Foster Care -----------------------------------------------------------
fc_virginia <- read_excel(paste0(getwd(),"/data/foster-care-2020-all.xlsx")) 
fc_2020 <- read_excel(paste0(getwd(),"/data/foster-care-2020.xlsx")) 
#Age
totals <- data.frame(fc_virginia[c(2:38),])

colnames(totals) <- c("Age Group", "Value")
groups <- c("<1", "1-5", "6-9", "10-12", "13-15", "16-18", "19+")
fc_ages <-data.frame(totals[c(24, 26, 28,30,32,34,36),])
fc_ages$Value <- as.numeric(fc_ages$Value)
#Race and ethnicity 
fc_races <- data.frame(totals[c(7,9,11,13,15,17),])
colnames(fc_races) <- c("Race", "Value")
fc_races$Race <- c("Black", "White", "Indian", "Asian", "Hawaiian", "Multi") 

eth <- rep(c("Hispanic" , "Non-Hispanic") , 1) 
value <- c(14, 34)
fc_eth <- data.frame(eth,value)

#TAYs
age_19 <- rep(c("<19" , "19+") , 1) 
value <- c(40, 8)
fc_tays <- data.frame(age_19, value)
# Sex
fc_sex <- data.frame(totals[c(1,3),])
colnames(fc_sex) <- c("Gender", "Value")

# Juvenille Detention -----------------------------------------------------------
# Race 
intake_race <- read_csv(paste0(getwd(), "/data/DJJ-2020-Juvenile_Detention_Locality-Race_Intake.csv")) 
colnames(intake_race) <-intake_race[1,]
intake_race <-intake_race[-1,]
jv_race <- intake_race %>% dplyr::select(RACE, `FY20 %`, CSU) %>% 
  filter(CSU == "20L") %>% 
  dplyr::rename(Proportion = `FY20 %`) %>% 
  dplyr::select(RACE, Proportion) %>% 
  rename(Race = RACE)%>%
  mutate(Proportion = readr::parse_number(as.character(Proportion)))


#Eth 
intake_eth <- read_csv(paste0(getwd(), "/data/DJJ-2020-Juvenile_Detention_Locality-Ethnicity_Intake.csv")) 
colnames(intake_eth) <-intake_eth[1,]
intake_eth <- intake_eth[-1,]
jv_eth <- intake_eth %>% dplyr::select(ETHNICITY, `FY20 %`, CSU) %>% 
  filter(CSU == "20L") %>% 
  dplyr::rename(Proportion = `FY20 %`, Ethnicity = ETHNICITY) %>%
  dplyr::select(Ethnicity, Proportion)%>%
  mutate(Proportion = readr::parse_number(as.character(Proportion)))

# Sex
intake_sex <- read_csv(paste0(getwd(),"/data/DJJ-2020-Juvenile_Detention_Locality-Sex_Intake.csv")) 
jv_sex <- intake_sex %>% dplyr::select(SEX, `FY20 %`, CSU) %>% 
  filter(CSU == "20L") %>% 
  dplyr::rename(Proportion = `FY20 %`, Sex = SEX) %>%
  dplyr::select(Sex, Proportion)%>%
  mutate(Proportion = readr::parse_number(as.character(Proportion)))

# Age
intake_age <- read_csv(paste0(getwd(),"/data/DJJ-2020-Juvenile_Detention_Locality-Age_Intake.csv")) 
colnames(intake_age) <-intake_age[1,]
intake_age <- intake_age[-1,]
jv_age <- intake_age %>% dplyr::select(AGE, `FY20 %`, CSU) %>% 
  filter(CSU == "20L") %>% 
  dplyr::rename(Proportion = `FY20 %`, Age = AGE) %>%
  dplyr::select(Age, Proportion) %>% 
  filter(Age != "Total Cases")%>%
  mutate(Proportion = readr::parse_number(as.character(Proportion)))


# Population Density
m_pop <- get_acs(geography = "tract", 
                 variables = c("B01001_007", "B01001_008", "B01001_009", "B01001_010"), 
                 state = "VA",
                 county = "Loudoun County",
                 geometry = TRUE)
f_pop <- get_acs(geography = "tract", 
                 variables = c("B01001_022", "B01001_023", "B01001_024", "B01001_025"), 
                 state = "VA",
                 county = "Loudoun County",
                 geometry = TRUE)

both <- m_pop %>%
  mutate(f = f_pop$estimate)%>%
  mutate(count = estimate+f)%>%
  dplyr::select(GEOID, NAME, variable, geometry, count)

both <- both%>%
  group_by(NAME) %>%
  summarize(both = sum(count))

# Trees -----------------------------------------------------------
Tree <- read_excel(paste0(getwd(),"/data/combined-programs.xlsx")) 
# Maps -----------------------------------------------------------
## Locations ---------
map <- read_excel(paste0(getwd(),"/data/combined-programs.xlsx")) 

loudoun_locations <- map %>%
  filter(County == "Loudoun") %>% 
  dplyr::select(Program, Longitude, Latitude, Office, Pillars, Subpopulation, Qualification, Description, Website) %>%
  filter(Longitude != "Online" & Longitude != "Mulitple locations") %>% drop_na()

loudoun_locations$Longitude <- as.numeric(loudoun_locations$Longitude)
loudoun_locations$Latitude <- as.numeric(loudoun_locations$Latitude)

allegheny_locations <- map%>%
  filter(County == "Allegheny") %>% 
  dplyr::select(Program, Longitude, Latitude, Office, Pillars, Subpopulation,  Qualification, Description, Website) %>%
  filter(Longitude != "Online" & Longitude != "Mulitple locations") %>% drop_na()

allegheny_locations$Longitude <- as.numeric(allegheny_locations$Longitude)
allegheny_locations$Latitude <- as.numeric(allegheny_locations$Latitude)

fairfax <- map%>%
  filter(County == "Fairfax") %>% 
  dplyr::select(Program, Longitude, Latitude, Office, Pillars, Subpopulation,  Qualification, Description, Website) %>%
  filter(Longitude != "Online" & Longitude != "Mulitple locations") %>% drop_na()

fairfax$Longitude <- as.numeric(fairfax$Longitude)
fairfax$Latitude <- as.numeric(fairfax$Latitude)

subpop_levels <- c("TAYs", "Foster Care", "Juvenile Detention")
subpop_pal <- colorFactor(pal = c('darkorange1', 'mediumpurple1', "firebrick1"),
                          levels = subpop_levels)

Pillar_levels <- c("Education", "Employment", "Housing", "Transportation", "Health Services")
Pillar_pal <- colorFactor(pal = c('red', 'yellow', 'blue', 'orange', 'green'), 
                          levels = Pillar_levels)
## Zipcodes and map of Loudoun-------
va_zips <- zctas(state = "VA", year = 2010)
loudoun_zip_link <- "http://ciclt.net/sn/clt/capitolimpact/gw_ziplist.aspx?ClientCode=capitolimpact&State=va&StName=Virginia&StFIPS=51&FIPS=51107"
loudoun_zip_codes <- read_html(loudoun_zip_link) %>% html_node("td table") %>%  
  html_table() %>% dplyr::select(c(1,2)) %>% dplyr::rename(`Zip Code` = X1, City = X2) %>%
  slice(-c(1, 2)) 
loudoun_zip_code_city_names <- loudoun_zip_codes %>% pull(City)
loudoun_zip_codes <- pull(loudoun_zip_codes, `Zip Code`)
loudoun_zips <- va_zips %>% filter(ZCTA5CE10 %in% loudoun_zip_codes)


overtime <- read_excel(paste0(getwd(), "/data/program-services-overtime.xlsx"))

## Persons Served------------
classroom <- 396
tutoring <- 20
jobsite <- 49 
ged <- 2 
workforce <- 105 

programs <- data.frame((rbind(classroom, tutoring, jobsite, ged, workforce)))
programs$Type <- c("Classroom", "Tutoring", "Jobsite", "GED", "Workforce Development Week")
colnames(programs) <- c("Number", "Type")

served <- 543 
hispanic <- 58
asian <- 24
white <- 10
other <- 6 


persons <- data.frame((rbind(hispanic, asian, white, other)))
persons$Race <- c("Hispanic", "Asian", "White", "Other")
colnames(persons) <- c("Number", "Race")


enroll <- read_excel(paste0(getwd(), "/data/medicaid-enrollment.xlsx")) 

total <- enroll[1:13,]
total$`Adults, Pregnant Women and Children` <- as.numeric(total$`Adults, Pregnant Women and Children`)
med <- enroll[16:20,1:3]
colnames(med) <- c("Year", "Children", "Childless Adults")
med$`Childless Adults` <- as.numeric(med$`Childless Adults`)
med$Children <- as.numeric(med$Children)



average_stay <- 9 #months 
median_stay <- 5 
prior_home <- 61.3 #percent 
prior_jail <- 79.6
edu <- 12.1 #years 

ox <- data.frame(rbind(average_stay, median_stay, prior_home, prior_jail, edu)) 
ox$Category <- c("Average Stay (mo) ", 
                 "Median Stay (mo) ", 
                 "Prior Homelessness (%) ",
                 "Prior Jail time (%) ",
                 "Average Education (yrs) ") 
colnames(ox) <-c("Number", "Category") 
ox$Category <- factor(ox$Category, levels=unique(ox$Category))


l <- 180
c <- 3355
m <- 75
f <- 23
undisclosed <- 2

tay <- 30
no_high <- 17
college <- 11 

all <- data.frame(rbind(l, c, m, f, undisclosed, tay, no_high, college)) 
all$Category <- c("Served in Loduoun", 
                  "Total Served", 
                  "Male",
                  "Female",
                  "Undisclosed Gender", 
                  "TAYs",
                  "Did not finish high school",
                  "Have a college education")
colnames(all) <-c("Number", "Category") 
all$Category <- factor(all$Category, levels=unique(all$Category))


# CODE TO DETECT ORIGIN OF LINK AND CHANGE LOGO ACCORDINGLY
jscode <- "function getUrlVars() {
                var vars = {};
                var parts = window.location.href.replace(/[?&]+([^=&]+)=([^&]*)/gi, function(m,key,value) {
                    vars[key] = value;
                });
                return vars;
            }

           function getUrlParam(parameter, defaultvalue){
                var urlparameter = defaultvalue;
                if(window.location.href.indexOf(parameter) > -1){
                    urlparameter = getUrlVars()[parameter];
                    }
                return urlparameter;
            }

            var mytype = getUrlParam('type','Empty');

            function changeLinks(parameter) {
                links = document.getElementsByTagName(\"a\");

                for(var i = 0; i < links.length; i++) {
                   var link = links[i];
                   var newurl = link.href + '?type=' + parameter;
                   link.setAttribute('href', newurl);
                 }
            }

           var x = document.getElementsByClassName('navbar-brand');

           if (mytype != 'economic') {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/events/symposium2020/poster-sessions\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2020 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic'); 
           }
           "




# UI -----------------------------------------------------------
ui <- navbarPage(title = "DSPG 2021",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
                 useShinyjs(),
                 
                 
                 
                 ## Tab Overview--------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 2px;",
                                   align = "center",
                                   # br("", style = "padding-top:2px;"),
                                   # img(src = "uva-dspg-logo.jpg", class = "topimage", width = "20%", style = "display: block; margin-left: auto; margin-right: auto;"),
                                   br(""),
                                   h1(strong("Service Provision For Vulnerable Transition Aged Youth In Loudoun County"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Project Background")),
                                          p(strong("The problem."), "Rural counties often face challenges in providing health care access to their residents given limited", a(href = "https://www.ruralhealthinfo.org/topics/hospitals", "health facilities", target = "_blank"),
                                            "available, lack of broadband infrastructure that makes it difficult to provide", a(href = "https://www.ruralhealthinfo.org/topics/telehealth", "telemedicine access", target = "_blank"), "or communicate health information, and individual-level",
                                            a(href = "https://www.ruralhealthinfo.org/topics/social-determinants-of-health", "inequalities", target = "_blank"), "that pose barriers to health care use and health
                                            behaviors. Identifying areas of high need or potential solutions may also be difficult for rural areas without adequate resources to acquire, analyze, and interpret
                                            relevant data."),
                                          p(),
                                          p(strong("The setting."), a(href = "https://www.loudoun.gov", "Loudoun County", target = "_blank"), 
                                            "is located in the northern part of the Commonwealth of Virginia in the United States. It covers 515.6 square miles ranking 20th-largest county 
                               in Virginia by area. In 2019, the population was estimated at 413,538, making it Virginia’s third-most populous county. Loudoun County is part of the Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area.
                               The county's ", a(href = "https://www.census.gov/quickfacts/loudouncountyvirginia", "median household income", target = "_blank"),
                                            "of $142,299, 3.1% poverty rate, and median home value of $508,100 makes it the richest county in Virginia in 2019. Our target population, ages 18-24, makes up about 5% of the total population.  
                               "),
                                          
                                          
                                          
                                          p(),
                                          p(strong("The project."), "This Virginia Tech", a(href = "https://aaec.vt.edu/index.html", "Department of Argicultural and Applied Economics", target = "_blank"),
                                            "Data Science for Public Good (DSPG) project aimed to build local capacity, leverage social and data science to address current and future resident well-being, and enhance
                                             data-driven decision making about rural health in Floyd County, Virginia.")
                                   ),
                                   column(4,
                                          h2(strong("Our Work")),
                                          p("Our research team worked closely with Floyd County Extension Office, Virginia Department of Health, and Healthy Floyd County coalition stakeholders
                                            to identify the county’s priority challenges in the area of health. The research team reviewed a prior", a(href = "https://www.vdh.virginia.gov/west-piedmont/2020/05/27/patrick-county-health-needs-improvement-plan-completed/",
                                                                                                                                                       "community health assessment,", target = "blank"), a(href = "https://www.pubs.ext.vt.edu/VCE/VCE-596/VCE-596-75/VCE-1002-75.html", "situation analysis", target = "_blank"),
                                            "relevant funding applications, and held a listening meeting with stakeholders to identify these challenges. Lack of
                                            data on health care access, food access as related to diabetes and heart disease prevalence, older adult health, and digital connectivity that would facilitate
                                            access to telemedicine emerged as key problems where providing actionable insights could address barriers to Patrick County residents’ health."),
                                          p(),
                                          p("We implemented the", a(href = "https://doi.org/10.1162/99608f92.2d83f7f5", "data science framework", target = "_blank"), "and identified, acquired, profiled, and used
                                            publicly available data to provide Floyd County with data-driven resources in each of the four priority areas. We:"),
                                          tags$li("Provided census tract- and census block group-level maps of Floyd County residents'", strong("sociodemographic and socioeconomic characteristics,"), " highlighting underprivileged areas."),
                                          tags$li("Created barplots of", strong("monthly temperatures and precipitation levels"), "to show the geographic distribution of older adults in the county by gender and
                                                  type of disability, identifying areas where providing telehealth or travelling preventive care services may be particularly important."),
                                          tags$li("Mapped locations of", strong("streams, lakes, and mines"), "at census block group level, and constructed 10- and 15-minute isochrones (areas of equal travel time) from households to free
                                                  wifi hotspots to highlight internet gaps that could suggest where new wi-fi hotspots could be optimally placed to provide internet access to more residents."),
                                          tags$li("Calculated and mapped", strong("water usage"), "of households within 8-, 10-, and 12-minute travel times, identifying areas difficult to reach within
                                                   standard EMS travel thresholds."),
                                          tags$li("Constructed", strong("land parcel"), "maps by census tract, 10- and 15-minute isochrones from households to grocery stores and farmers markets, and maps of food security resources in the county,
                                                highlighting food deserts and areas that could benefit from programs facilitating access to fresh produce."),
                                          p(),
                                          p("This dashboard compiles our findings and allows extension professionals, stakeholders, and other users to explore the information interactively.")
                                   ),
                                   column(4,
                                          h2(strong("Dashboard Aims")),
                                          p("Our dashboard is aimed at:"),
                                          p(strong("Floyd County extension professionals and the communities they serve."), "Information available through the interface helps extension
                                            agents identify areas where residents may not have access to internet, or areas with a high smartphone ownership share, suggesting what channels agents may
                                            want to use to disseminate health-related information most effectively. Information on older adult populations and grocery store access can help extension agents
                                            better understand where underserved populations live and how to advocate on their behalf."),
                                          p(strong("Local health-related agencies and departments seeking data insights to inform their decision-making."), "For local stakeholders, identifying broadband
                                            access gaps that limit access to telemedicine, grocery store access gaps, and areas with high proportions of older adults with independent living difficulty can suggest
                                            optimal locations for placing free wifi hotspots, providing grocery delivery services, devising mobile health unit routes, or can inform other solutions that would benefit
                                            a broad population base."),
                                          p(strong("State government representatives in the Virginia Department of Health and the State Office of Rural Health."), "These and similar stakeholders may
                                            need small or rural area-specific insights that Centers for Disease Control and other county-level datasets cannot provide.")
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021')))
                                   
                                   
                          ) 
                          
                 ),
                 
                 ## Tab Introduction to Loudoun County& TAY--------------------------------------------
                 navbarMenu("Sociodemographics",
                            # tabPanel("Loudoun",
                            #          fluidRow(style = "margin: 6px;",
                            #                     h1(strong("Loudoun County Residents' Sociodemographic Characteristics"), align = "center"),
                            #                     p("", style = "padding-top:10px;"), 
                            #                     h4(strong("Who does Loudoun County Serve?")),
                            #                  
                            #                       p("The median income of households in Loudoun County, Virginia was $142,299, the poverty rate is 3.4% in 2019. Since 2008, the county has been ranked first in the U.S. in median household income among jurisdictions
                            #                      with a population of 65,000 or more. " ), 
                            #                      p("Our targeted population are youths from 18-24 years old, the transitional aged youth, with two subpopulation of those who have aged out of the foster
                            #                      care system and those who exiting Juvenille Detention. Transitional Aged youth have a harder time adjusting to living 
                            #                      independently especially when majority of them do not have a at home support system if they have come out the system. In Loudoun county,
                            #                      this age group makes up about 5% of the population with 28,917 in total according to the American Community Survey 1-year estimates 2019. " ))
                            #          
                            #          
                            #       ),
                            tabPanel("Target Population",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Transition Aged Youths' Sociodemographic Characteristics"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(4, 
                                                     h4(strong("Who does Loudoun County Serve?")),
                                                     p("We examined Loudoun County population sociodemographic and socioeconomic characteristics to better understand the residents that the county serves."),
                                                     p("We retrieved American Community Survey (ACS) data to calculate this information and visualize through graphs.
                                                      ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets. 
                                                      We used the most recently available 1-year estimates from 2018/19 to compute statistics for Loudoun county residents by age, race, ethnicity, 
                                                      employment, health insurance coverage, and other relevant characteristics."), 
                                                     
                                                     p("In Loudoun county, 5% of the population makes up transition aged youth (18-24) who are at a disadvantage when trying to live indpendently especially those who have aged out of the foster care
                                                       system or who have gotten out of juvenile detnetion.They are looking for a way to be more independent and responsible, but
                                                    because of their past experiences may not have enough resources on their own to make a living and survive. "),
                                                     p("Based on our literature review done in the first 2 weeks of research, 
                                                    the problem these young adults face is they want their independence and to create a life for themselves but they do not have the resources (finanical or material) or knowledge to do so on their own. 
                                                    With many of the programs and services provided in the past, the landlords or renters of apartments and homes would create extra barriers for youths coming out of the foster care system or juvenille detention and still treat
                                                    them like children but expect them to be adults. However, within the past decade, Loudoun County has created many new programs and services in order to help the TAYs be able to transition more smoothly.")
                                                 ), 
                                              column(8, 
                                                     h4(strong("Visualizations of Resident Socioeconomic Characteristics")),
                                                     selectInput("var1", "Select Demographic:", width = "100%", choices = c(
                                                       "Gender and Age" = "age",
                                                       "Percentage of TAYs" = "percent", 
                                                       "Educational Attainment" = "education",
                                                       "Races" = "race",
                                                       "Poverty Level" = "poverty",
                                                       "Healthcare Coverage" = "health", 
                                                       "Severe Mental Illness" = "mental")
                                                     ),
                                                     plotlyOutput("plot1"),
                                                     p(tags$small("Data Source: American Community Survey 2019 1-Year Estimates.")),
                                                     p(tags$small("Data Source: Department of Mental Health, Substance Abuse, and Developmental Services (MHSADS)")) 
                                              )
                                     )) ,
                            tabPanel("Subpopulation",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Subpopulations" ), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(4, 
                                                     h4(strong("Foster Care")),
                                                     # p("In the US 2019, 423,997 children were in the foster system with 251,359 
                                                     #   newly entered children and 248,669 exiting. The average age of a child in 
                                                     #   foster care is 8.4 years old and males are the majority by 4%. For the Transitional 
                                                     #   Aged Youth (18-24), they only make up about 4% of the total foster care youth in 
                                                     #   the US. 44% of foster care youth are white and 23% were black. Similar to foster 
                                                     #   care statistics in Virginia alone, the average time in care is 19.6 months [2]. 
                                                     #   According to The AFCARS Report in 2019, only 3,335 (1%) children who entered 
                                                     #   the foster care system were 18+ and it was most likely due to neglect. However, 
                                                     #   there were 20,465 (8%) youths 18+ who exited the system most likely due to
                                                     #   aging out and emancipation."), 
                                                     
                                                     p("According to the The Adoption and Foster Care Analysis and Reporting System,
                                                      in 2020 there were 48 children in foster care in only Loudoun County which was
                                                      .8% in the state of Virginia. Over 2/3
                                                      of those children were boys and 1/3 were girls and the minority of them of
                                                      ethnicity of Hispanic. Almost 50% of those children were white, 25% black
                                                      and less than 5% Asian and multi-racial as you can see from the barplot below.
                                                      When we are looking at only transitional aged youth from 18-24 where 21 years
                                                      old is the average time a foster child ages out, there were only 8 children.
                                                      In Loudoun County, it does not seem like there are many foster care youths
                                                      who are aging out of the system but only 9 other counties have greater than 9
                                                      foster care kids over the age of 18. "), 
                                                     p("It was difficult to retrieve data specifically on those who have aged out of the foster care system and are ages 18-24 because once they have aged out, there
                                                       is not public record of where they go or how they are."), 
                                                     br(), 
                                                     h4(strong("Juvenile Detention")),
                                                     
                                                     # p("Youth incarceration in Virginia is run by the Virginia Department of Juvenile Justice (DJJ) 
                                                     #   and is split between juvenile detention centers (JDCs), group homes, and youth prisons. 
                                                     #   Black youth are overrepresented among offenders in residential placement, making up 
                                                     #   40.9% of residents, as compared to 33.3%, 20.3%, .98% and 2.1% for whites, hispanics, 
                                                     #   asians and native americans, respectively (OJJDP 2019). In regards to sex, males make up 
                                                     #   the vast majority of offenders in residential placement, at 85.2% of residents. 
                                                     #   Broken down by age, those aged 16-17 years made up the bulk of residents, accounting for 
                                                     #   52.4% of them (OJJDP 2019). With respect to mental health, about 73% of all youth entering 
                                                     #   youth prisons demonstrated significant symptoms of mental disorder and more than 94.9% of
                                                     #   youth who entered Virginia youth detainment facilities showed some symptoms of Attention 
                                                     #   Deficit Hyperactivity Disorder (ADHD), Conduct Disorder (CD), Oppositional Defiant Disorder 
                                                     #   (ODD), Substance Abuse, or Substance Dependence (DJJ 2020)."), 
                                                     
                                                     p("Virginia has some of the highest referral and incarceration rates of youth,
                                                      with the highest number of student referrals in the country and a rate of youth
                                                      incarceration at 75 percent higher than the national average at 79 per 100,000 youths
                                                      (Data Snapshot of Youth Incarceration in Virginia, smarter_choices_FINAL). While Virginia
                                                      spends around $171,588 per incarcerated youth annually (DJJ 2016), it still deals with high
                                                      recidivism rates, with 34.4% of probation placements, 54.4% of direct care Placements and
                                                      60.7% of parole placements being repeat offenders. In addition to high recidivism rates,
                                                      youths being released from direct care for the Fiscal Year of 2015 only received high school
                                                      diplomas or a GED at a rate of 19 percent. During the 2019-2020 school year only 35 total
                                                      youth offenders received a high school diploma or GED (DJJ 2019). However, the public
                                                      pattern of youth imprisonment in the U.S. has been declining, and did so too in Virginia,
                                                      with youth imprisonment down 65% in Virginia between 2003 and 2016. ")
                                                               
                                                        ), 
                                              column(8, h4(strong("Visualizations of Subpopulation")),
                                                     tabsetPanel(
                                                       tabPanel("Foster Care",
                                                                selectInput("var2", "Select Demographic:", width = "100%", choices = c(
                                                                  "Age" = "age",
                                                                  "Sex" = "sex",
                                                                  "Race" = "race",
                                                                  "Ethnicity" = "eth",
                                                                  "TAYs" = "tays")
                                                                ),
                                                                plotlyOutput("plot2"),
                                                                p(tags$small("Data source: The Adoption and Foster Care Analysis and Reporting System 2019"))
                                                                
                                                       ),
                                                       tabPanel("Juvenile Detention",
                                                                selectInput("var3", "Select Demographic:", width = "100%", choices = c(
                                                                  "Age" = "age",
                                                                  "Sex" = "sex",
                                                                  "Race" = "race",
                                                                  "Ethnicity" = "eth")
                                                                ),
                                                                plotlyOutput("plot3"),
                                                                p(tags$small("Data source: Department of Juvenile Justice (DJJ) "))
                                                                
                                                       )
                                                     ) 
                                              )    
                                     )) 
                 ),
                 
                 
                 
                 ## Tab Data and Methodology--------------------------------------------
                 tabPanel("Data and Methodology", value = "data",
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Data and Methodology"), align = "center"),
                                   p("", style = "padding-top:10px;"), 
                                   column(4,
                                          p("We examined Loudoun County population sociodemographic and socioeconomic characteristics to better understand the residents that the county serves."),
                                          br(), 
                                          img(src = 'data-acs.png', style = "display: inline; float: left;", width = "200px"),
                                          p("We retrieved American Community Survey (ACS) data to graph the different characteristics of our targeted population. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year  and 5-year datasets. We used
                      the most recently available 1-year estimates from 2018/2019 to compute percent Loudoun County residents by age, race, gender,educational attainment, health insurance coverage, and poverty level."),
                                          
                                          img(src = 'data-afcars.png', style = "display: inline; float: left;", width = "200px"),
                                          p("We used The Adoption and Foster Care Analysis and Reporting System to report on the number of youths in foster care from 2019 in Loudoun County. 
                         We needed a better idea of how many youths need services to transition out of the system. "),
                                          
                                          
                                          img(src = 'data-djj.jpg', style = "display: inline; float: left;", width = "100px"),
                                          p("We used Department of Juvenile Justice to report on the number of youths in Juvenille Detention and how many are transitioning out from 2019 in Loudoun County. 
                        We used these numbers to get a better idea of how many youths need services to transition out of the system.")),
                                   
                                   column(4,
                                          img(src = 'data-virginiaDSS.jpeg', style = "display: inline; float: left;", width = "200px"),
                                          p("We used Virginia Department of Social Services to report on the number of youths in Juvenille Detention and how many are transitioning out from 2019 in Loudoun County. 
                         We used these numbers to get a better idea of how many youths need services to transition out of the system."),
                                          
                                          img(src = 'data-usCensus.png', style = "display: inline; float: left;", width = "150px"),
                                          p("We used US Census Bureau to report on the number of youths in Juvenille Detention and how many are transitioning out from 2019 in Loudoun County. 
                         We used these numbers to get a better idea of how many youths need services to transition out of the system.")),
                                   
                                   column(4, 
                                          
                                          p("We began our research with a literature review...... "), 
                                          br(), 
                                          p("Next we webscraped information on the demographics of our target population: 18-24; in foster care or juvenile detention. "),
                                          p("Webscrapped the services and programs available and got their locations to map. "),
                                          br(),
                                          p("Compare against Fairfax, VA and Allegheny, PA "))
                          )
                          
                 ),
                 
                 
                 ## Tab Services--------------------------------------------
                 navbarMenu("Service",
                            tabPanel("Availability", value = "services", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Service Availability"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(6, 
                                                     h4(strong("What services and programs are available?")),
                                                     p("The following tables provide counts of programs  
                                                       and services available in each county by subpopulation and by pillar.
                                                       They allow for simple comparison of program types across counties."), 
                                                     
                                                     p("To gather this information for the interactive tree diagrams, our team researched for three weeks the different programs available to transition aged youth, either
                                                       specific to foster care or juvenile detention subpopulations or in general for 18-24 year olds in Loudoun County, Fairfax County and Allegheny County. 
                                                       Our team searched as if we were the young adult looking for this information and noticed how some programs and services regarding the transportation and housing 
                                                       pillar were not easily accessible and were hidden in multiple documents. "),
                                                     
                                                     p("It is to be noted that the greater majority of the programs/services found 
                                                       across all counties were directed towards vulnerable TAY in general.  This highlights
                                                       a lack of targeted programs and a potential need to increase the number of those
                                                       programs and services specifically catering to the needs of those formerly involved
                                                       in the juvenile detention or foster care system."), 
                                                     
                                                     p("Looking at the distribution of programs by pillar in Loudoun, it can be seen that 
                                                       the number of programs/services are fairly equal by pillar, with the notible exception 
                                                       of transportation. Transportation services in Loudoun county useful to vulnerale TAY 
                                                       are not targeted to those formely involved in Foster Care or Juvenile detention, and 
                                                       are rather limited outside of medical transportation and commuter buses. This illustrates 
                                                       the potential need to expand transportation services to those TAY formerly involved in Foster Care 
                                                       or the Juvenile Detention system.")) ,
                                              column(6, 
                                                     h4(strong("Number of Programs by Subpopulation")), 
                                                     tableOutput("table1"),
                                                     tags$br(), 
                                                     h4(strong("Number of Programs by Pillar")), 
                                                     tableOutput("table2")) 
                                     ) ,
                                     tags$br(), 
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Trees")),
                                                     p("These interactive tree diagrams allow for the comparison of programs/services at the county 
                                                       level in Loudoun, Fairfax and Allegheny county. Each diagram represents
                                                       the programs of the selected pillar in the selected county. Nodes represent first the pillar, then the targeted subpopulation,
                                                       followed by the program/service name, then the intended age range of the program/service and lastly the city or county location of its office"),
                                                     
                                                     p("We chose to compare Loudoun County to Farifax County and Alleghney County for two reasons. 
                                                       As the stakeholders had mentioned, Fairfax and Loudoun are constantly being compared because of their close proximity and similar demographics. Therefore, we looked at programs in each of these counties to see 
                                                       if the gaps in services was for only Loudoun county or in the DC Metropolitan area, Northern Virginia area. Second, we chose Allegheny County because based on our research, they have a successful
                                                       transition rate of young adults 18-24 and looking at the programs available for those previously involved in foster care and juvenile detention, it seems that they have more programs specific towards them. 
                                                       Allegheny County is a great county to look at and see what they are doing compared to a county like Loudoun who wants to provide more programs and services to vulnerable transition aged youth in the future. "), 
                                                     
                                                     p("Using these interactive trees, we can see all of the programs we webscrapped for for each of the counties with the last node being where the person must go to apply. 
                                                       Since the pandemic, most of the applications can be done online but obviously some of the programs and services are in person. With this information, we can see how easy or difficult it is for 
                                                       these transition aged youth to access each program depending if they have to travel far in order to even apply or if that can be done online within a couple of hours. These trees leads us into the question of where are the gaps 
                                                       in services and programs and which pillar are they under. ")
                                              ),
                                              column(8, 
                                                     h4(strong("Tree Diagram of Programs")),
                                                     tabsetPanel(
                                                       tabPanel("Loudoun",
                                                                br(),
                                                                radioButtons(
                                                                  "pillar1",
                                                                  label = "Select Pillar" ,
                                                                  choices = list(
                                                                    "Education",
                                                                    "Employment",
                                                                    "Housing",
                                                                    "Transportation",
                                                                    "Health Services")
                                                                ),
                                                                selected = "Education", 
                                                                collapsibleTreeOutput("tree1") 
                                                                
                                                                
                                                                
                                                       ),
                                                       tabPanel("Fairfax",
                                                                br(),
                                                                radioButtons(
                                                                  "pillar3",
                                                                  label = "Select Pillar" ,
                                                                  choices = list(
                                                                    "Education",
                                                                    "Employment",
                                                                    "Housing",
                                                                    "Transportation",
                                                                    "Health Services")
                                                                ),
                                                                selected = "Education", 
                                                                collapsibleTreeOutput("tree3") 
                                                                
                                                                
                                                       ), 
                                                       
                                                       tabPanel("Allegheny",
                                                                br(),
                                                                radioButtons(
                                                                  "pillar2",
                                                                  label = "Select Pillar" ,
                                                                  choices = list(
                                                                    "Education",
                                                                    "Employment",
                                                                    "Housing",
                                                                    "Transportation",
                                                                    "Health Services")
                                                                ),
                                                                selected = "Education", 
                                                                collapsibleTreeOutput("tree2")
                                                                
                                                       )) 
                                              )) ,
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"), 
                                              column(4,  
                                                     h4(strong("Where are the gaps?")), 
                                                      p("These interactive tree diagrams contain all programs and services available in Loudoun, Fairfax and Allegheny county organized by pillar. The diagrams are intended for 
                                                      comparison between counties. Its nodes first represent pillar, then subpopulation, followed by program name, intended age range of program, and then finally the county 
                                                      which the program is accessible from."),
                                                     
                                                     p("While going through the interactive trees, you may see that there are an abundant amount of services and programs for pillars like 
                                                       education and employment in Loudoun but again, not many for transportation and housing. Here is where we noticed the first pillars in what type of programs Loudoun are lacking in. 
                                                       While Loudoun county has many programs supporting the education and employment pillars, the transition aged youth may have a hard time taking advantage of those programs without 
                                                       transportation or health services available to them. Due to Loudoun having a gap in the transportation pillar, the other pillars' programs are being used as much as youths want to which is
                                                       why this age group (18-24) especially those aging out of the foster care and getting out of juvenile detention are having trouble living independently. 
                                                       "), 
                                                    
                                                     p("This one combined tree is used to better compare with Fairfax County and Allegheny County. As noted above,
                                                       the last node represents the county the program is accessible from when we can use to better visualize the gaps in which pillar in Loudoun County. Most of the programs 
                                                       in each county are specific to those who live in the county with the exception of Great Expectations, Workforce Innovation and Opportunity Act, Education and Training Vounchers and Medicaid. 
                                                       With this interactive tree, we can better see where Loudoun County is lacking versus where Allegheny county is sufficient in which led them to having a high success transition rate. "), 
                                                     br(), 
                                                     br(), 
                                                     br()
                                                     
                                                     
                                                     ) ,
                                              column(8, 
                                                     h4(strong("Comparison Tree by Pillar")),
                                                     radioButtons(
                                                       "compare1",
                                                       label = "Select Pillar" ,
                                                       choices = list(
                                                         "Education",
                                                         "Employment",
                                                         "Housing",
                                                         "Transportation",
                                                         "Health Services")
                                                     ),
                                                     selected = "Education", 
                                                     collapsibleTreeOutput("compare") , 
                                                     br(),
                                                     br()
                                                     
                                              ) 
                                              
                                     )
                            ),
                            
                            
                            tabPanel("Locations", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Location of Programs and Services"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Where are these services and programs located?")),
                                                     p("The following interactive maps mark the office locations 
                                                       of the services and programs found available to vulnerable
                                                       TAY. Hovering over the circle markers will display the name of the service/program,
                                                       while clicking on the circle markers will open a popup with a detailed description of 
                                                       the program/service. Maps are available by county; each county's services and programs can be 
                                                       catagorized by subpopulation served and by the pillar the program/service best 
                                                       fits. The switchboards at the topright allow the user to further filter locations."),
                                                     p("The Loudoun county maps make more apparent the higher concentration of 
                                                     offices for programs/services 
                                                       found towards the east side of the county, near 
                                                       the DC Metropolitan area. This divide between the eastern part of the county and the rest of the county
                                                      highlights the need for greater access to 
                                                      in-person services and 
                                                       programs useful to vulnerable TAY in all parts of the county. ")
                                                     
                                              ), 
                                              column(8, 
                                                     h4(strong("Map of Locations of Services")),
                                                     column(4,
                                                            selectInput(
                                                              "county",
                                                              "Select County",
                                                              choices = unique(map$County),
                                                              selected = "Loudoun",
                                                              width = 400
                                                            )),
                                                     column(4, 
                                                            radioButtons(
                                                              "category",
                                                              label = "Select Category" ,
                                                              choices = c("Subpopulation", "Pillars"),
                                                            ),
                                                            selected = "Subpopulation" ), 
                                                     leafletOutput(outputId = "map1", height = "400px")) 
                                              
                                              
                                     )
                            )
                            
                 ),
                 
                 
                 
                 ## Tab Utilization-------------------------------------------
                 navbarMenu("Utilization",
                            ### Individual served------------
                            tabPanel("Individuals Served", value = "served",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Individuals Served by Program"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(6, 
                                                     h4(strong("Statistics")), 
                                                     infoBoxOutput("medicaid", width = 6),
                                                     infoBoxOutput("wioa", width = 6),
                                                     infoBoxOutput("transit" , width = 6),
                                                     infoBoxOutput("food", width = 6),
                                                     infoBoxOutput("shelters", width = 6),
                                                     infoBoxOutput("homeless", width = 6)
                                              ), 
                                              column(6, 
                                                     h4(strong("Graphs of Program Demographics")), 
                                                     p("", style = "padding-top:10px;"), 
                                                     radioButtons(
                                                       "type2",
                                                       label = "Select Program Type" ,
                                                       choices = list(
                                                         "Adult Literacy Program Loudoun" = "literacy",
                                                         # "Continuum of Care" = "care",
                                                         # "Affordable Dwelling Unit Program" ="afford", 
                                                         "Oxford House" = "oxford",
                                                         "OAR"= "oar", 
                                                         # "Route 54 Safe-T" = "bus",
                                                         "Medicaid" = "med")
                                                     ),
                                                     selected = "literacy",
                                                     tags$br(), 
                                                     plotlyOutput(outputId = "plotServed") ,
                                                     tags$br(), 
                                                     plotlyOutput(outputId = "plotServed2"))
                                     ) 
                            ) ,
                            
                            ### DMHSA------
                            tabPanel("DMHSA", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Department of Mental Health, Substance Abuse, and Developmental Services"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4,
                                                     h4(strong("Vulnerable Transition Aged Youth")),
                                                     p("Although we are looking specifically are transition aged youth, those aging out of the foster care system or getting out of juvenile detention, there is another subpopulation we have information on
                                                     that shows us how important it is to find and close the gaps espeically in the health service pillar. Those living with mental health issues are also at a disadvantage when trying to live independently
                                                     because receiving treatment and care can be expensive without insurance and again, getting to a clinic or facility can be difficult. Those without a full-time job that provides health benefits
                                                       either have to qualify for medicaid or other grants or have to pay full payment out of pocket which can thousands of dollars. In addition,  TAYs, espeically those who were in foster care or in juvenile detention are vulnerable to 
                                                      substance abuse, mental illnesses and radical behavior because they likely did not have family support or a role model to guide them in their developmental stages.  "),
                                                     tags$br(),
                                                     p("Loudoun County Department of Mental Health, Substance Abuse, and Developmental Services (MHSADS) provided a number of 
                                                      programs and services to transition age youth (18-24) from 2016-2021YD in various zipcodes in Loudoun. The types of programs include Case Management, Discharge Planning,
                                                      Emergency Services, Employment and Day Support Services, Outpatient and Residental. As you can see in 2019, there was a large dip in 'Outpatient' waitlist persons because of the Same Day Access Program. 
                                                      Same Day Access is now being offered via tele-health whichone can call 703-771-5155 Monday-Friday, from 9:00 a.m. to 2:00 p.m. to begin the process. 
                                                      A person in need of a mental health evaluation can now access walk-in hours at any CSB throughout Virginia without an appointment, 
                                                      instead of waiting days or even weeks to receive an assessment. This has decreased the need for a waitlist for certain programs where a person can receive help in a couple of hours based on their insurance. 
                                                      ")), 
                                              column(8,
                                                     h4(strong("Waitlist of DMHSA by Program")), 
                                                     plotlyOutput("waitlist"),
                                                     p(tags$small("*The Case Management waitlist does not include I/DD individuals waiting for Support Coordination as this is largely dependent on state-allotted waivers."))  ,  
                                                     p(tags$small("**Since the start of the Same Day Access program in 2019, MHSADS has gotten rid of the Outpatient Services waitlist. ")))
                                              
                                     ),
                                     tags$br(),
                                     tags$br(),
                                     fluidRow(style = "margin: 6px;",
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Individuals Served Overtime")),
                                                     p("Visualizing those who have received help compared to those who live in the area give us a better idea of how these programs are being used and if they are being used to 
                                                       their full potential. Loudoun County Department of Mental Health, Substance Abuse and Developmental Services provided us with a list of zipcodes corresponding to the number of transition aged youth being served
                                                       from 2016 to 2020 by program. Using the dropdown, you can select a specific program and then use the slider to the right to see how the number of individuals being served changes overtime. 
                                                       The orange dots are being mapped onto a population density map by census tract of those who live in the area to visualize the utilization of each program within the past 5 years. "),
                                                     p("The number of individuals served mapped on top of the population density map gives us a better idea of those who have mental health issues in the different zipcodes and census tracts but also that the majority 
                                                       of those being served by these programs are located on the east side of the county. This leads to another interesting question that could be explored further: Are the gaps in service just by type or also geographically? Do those
                                                       living on the west side at a disadvantage in receiving aid for mental health services and potentailly all Living Independently services and programs? ")) ,
                                              column(8, 
                                                     h4(strong("Map of Individuals Served by Population Density")), 
                                                     column(4, 
                                                     radioButtons(
                                                       "type",
                                                       label = "Select Program Type" ,
                                                       choices = list(
                                                         "Case Management" = "case",
                                                         "Discharge Planning" = "dis",
                                                         "Emergency Services" = "emer",
                                                         "Employment & Day Support Services" = "employ",
                                                         "Outpatient"= "out", 
                                                         "Residential" = "res")
                                                     ),
                                                     selected = "Case Management") , 
                                                     column(4,
                                                     sliderInput(inputId = "year", 
                                                                 label = "Select a year:",
                                                                 value = 2016,
                                                                 min = 2016,
                                                                 max = 2020,
                                                                 animate = animationOptions(interval = 1500))) , 
                                                     
                                                     
                                                     leafletOutput(outputId = "overtime", height = "70vh"), 
                                                     p(tags$small("Data source: Department of Mental Health, Substance Abuse, and Developmental Services"))
                                                     
                                                     
                                              )
                                     ) 
                            ) 
                            
                            
                            
                            
                            
                 ),
                 ## Tab Conclusion --------------------------------------------
                 tabPanel("Conclusion",  value = "conclusion", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Conclusion"), align = "center")),  
                          p("", style = "padding-top:10px;"), 
                          p()
                          
                 ), 
                 ## Tab Team------
                 tabPanel("Team", value = "team",
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h1(strong("Team"), align = "center"),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good")),
                                   p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program offered by the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural'), "and", a(href = 'https://ext.vt.edu/','Applied Economics and the Virginia Cooperative Extension Service.'),
                                     "In its eighth year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around
                              critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences
                              to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program
                              highlights, how to apply, and our annual symposium, please visit", 
                                     a(href = 'https://aaec.vt.edu/content/aaec_vt_edu/en/academics/undergraduate/beyond-classroom/dspg.html#select=1.html', 'the official VT DSPG website.', target = "_blank")),
                                   p("", style = "padding-top:10px;")
                          ),
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   column(6, align = "center",
                                          h4(strong("DSPG Team Members")),
                                          img(src = "team-yang.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-jaida.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-rebstock.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "team-austin.png", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/yang-cheng-200118191/', 'Yang Cheng', target = '_blank'), "(Virginia Tech, Applied Microeconomics);",
                                            a(href = 'https://www.linkedin.com/in/jaida-robinson-92856b194/', 'JaiDa Robinson', target = '_blank'), "(Virginia State University, Applied Microeconomics);",
                                            a(href = 'https://www.linkedin.com/in/julie-rebstock', 'Julie Rebstock', target = '_blank'), "(Virgina Tech, Economics and Computational Modeling and Data Analytics);",
                                            a(href = 'https://www.linkedin.com/in/austin-burcham-9b32a81ab/', 'Austin Burcham', target = '_blank'), "(Virginia Tech, Statistical and Data Science).",
                                            a(href = 'https://www.linkedin.com/in/kyle-jacobs7/', 'Kyle Jacobs', target = '_blank'), "(Virginia State Univeristy, Statistical and Data Science)."),
                                          p("", style = "padding-top:10px;") 
                                   ),
                                   column(6, align = "center",
                                          h4(strong("VT Faculty Team Members")),
                                          img(src = "", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "", 'Chainta Holmes', target = '_blank'), "(Project Lead, Research Assistant Professor);",
                                            a(href = "", 'Isabel Bradburn', target = '_blank'), "(VT, Postdoctoral Research Associate);") , 
                                          p("", style = "padding-top:10px;")
                                   )) ,
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("Project Stakeholders")),
                                   p(a(href = '', 'Dawn Barnes', target = '_blank'), "(Virginia Cooperative Extension, Floyd County at Virginia Tech);",
                                     a(href = '', 'Terri Alt', target = '_blank'), "(Virginia Cooperative Extension, Patrick County at Virginia Tech)."),
                                   p("", style = "padding-top:10px;"),
                                   h4(strong("Acknowledgments"))
                          )
                 )
                 , inverse = T ) 


# server -----------------------------------------------------------
server <- function(input, output) {
  
  runjs(jscode)
  ## demographics -----------------------------------------------------------
  
  var1 <- reactive({
    input$var1
  })
  output$plot1 <- renderPlotly({
    if(var1() == "age") { 
      
      l_ages_gender <- l_ages_gender%>%
        select(variable, estimate, summary_est)
      
      names <- c("18-19", "20", "21", "22-24", "18-19", "20", "21",  "22-24")
      l_ages_gender$variable <- names
      l_ages_gender$gender <-  c("Male", "Male", "Male", "Male", "Female", "Female", "Female",  "Female")
      
      ggplot(l_ages_gender, aes(x = estimate, y = variable)) + 
        geom_bar(position="dodge", stat="identity", fill = "blue") + 
        labs(title = "Gender and Age Groups ", 
             y = "", 
             x = "Population Estimate") +coord_flip()
      
      
    } else if(var1() == "percent") {
      
      m_p <- sum(l_ages_gender$estimate[1:4])/205374 * 100 
      f_p <-sum(l_ages_gender$estimate[5:8])/208164 * 100 
      
      percent <- data.frame(rbind(m_p, f_p))
      colnames(percent) <- "Percent"
      percent$Gender <- c("Male", "Female")
      
      
      percent %>%
        ggplot() + geom_col(mapping = aes(x = Percent, y = Gender ), fill = "cadetblue")+
        labs(title = "Percent of TAYs by Gender", 
             y = "", 
             x = "Percent %") + coord_flip() 
      
    }else if(var1() == "race"){
      
      race %>%
        ggplot() + geom_col(mapping = aes(x = Estimate, y = Race ), fill = "lightblue")+ 
        labs(title = "Racial Demographics", 
             y = "Races", 
             x = "Population Estimate") 
      
    } 
    else if (var1() == "education")  { 
      
      both <- data.frame(medu, fedu)%>%
        mutate(sum = estimate+estimate.1)%>%
        select(variable, sum)
      
      both$variable <- factor(both$variable, levels=unique(both$variable))
      
      both %>%
        ggplot() + geom_col(mapping = aes(x = sum, y = variable ), fill = "paleturquoise")+ 
        labs(title = "Educational Attainment", 
             y = "", 
             x = "Population Estimate") 
      
      
    }
    else if (var1() == "health"){
      healthcare %>%
        ggplot(mapping = aes(x = Estimate, y = Type ))  + geom_col(fill = "dodgerblue")+ 
        labs(title = "Types of Healthcare Coverage", 
             y = "", 
             x = "Population Estimate") + 
        coord_flip() + 
        theme(legend.position = "none")
    }else if (var1() == "mental") {
      
      ggplot(smi, aes(x=Year)) + 
        geom_line(aes(y = Persons, group = Group, color = Group)) + 
        labs(title = "Severe Mental Illness from FY 2016 - FY 2021")
      
      
    }
    else {
      pov <- rbind(w_p, b_p, i_p, as_p, n_p, o_p)
      pov %>%
        ggplot() + geom_col(mapping = aes(x = sum, y = variable ), fill = "lightblue3")+ 
        labs(title = "Poverty by Race", 
             y = "", 
             x = "Population Estimate") + coord_flip()
      
    }
    
  })  
  
  
  var2 <- reactive({
    input$var2
  })
  ##Render Plot for Foster Care 
  output$plot2 <- renderPlotly({
    if(var2() == "age") {
      fc_ages$Age.Group <- factor(fc_ages$Age.Group, levels=unique(fc_ages$Age.Group))
      
      ggplot(data = fc_ages, aes(x= Age.Group, y = Value) ) + 
        geom_col(width = .95, alpha = .75, fill = "royalblue2") + coord_flip()+
        labs(x = "", 
             y = "Population Estimate",
             title = "Age Groups for Foster Care", 
             fill ="") + theme(legend.position = "none") 
      
    }else if(var2() == "race"){
      ggplot(data = fc_races, aes(x= Race, y = Value) ) + 
        geom_col(width = .95, alpha = .75, fill = "navy") + coord_flip()+
        labs(x = "", 
             y = "Population Estimate",
             title = "Racial Demographics for Foster Care", 
             fill ="") + theme(legend.position = "none")
      
    } else if (var2() == "eth") {
      g <- ggplot(fc_eth, aes(eth, value))
      g + geom_bar(stat = "identity",fill = rgb(0.2,0.1,0.9,0.9)) + 
        labs(title = "Ethinic Demographics of Foster children",
             x = "Ethnicity", 
             y = "Population Estimate") + theme(legend.position = "none")
      
    }else if(var2() =="sex") {
      ggplot(fc_sex, aes(x = Gender, y = Value)) + 
        geom_bar(stat="identity",fill = rgb(0.4,0.4,0.6,0.7)) + 
        labs(x = "" , y = "Population Estimate", 
             title = "Sex of Youths in Foster Care") + theme(legend.position = "none")
    }else{
      ggplot(fc_tays, aes(x = age_19, y = value)) + 
        geom_bar(stat="identity", fill = rgb(0.1,0.4,0.5,0.7)) + 
        labs(x = "Age Group" , y = "Population Estimate", 
             title = "Transitional Aged Youth vs Children in Foster Care") + theme(legend.position = "none")
    }
    
  }) 
  
  
  var3 <- reactive({
    input$var3
  })
  ##Render Plot for Juvenille Detention 
  output$plot3 <- renderPlotly({
    if(var3() == "age") {
      jv_age$Age <- factor(jv_age$Age, levels=unique(jv_age$Age))
      jv_age <- jv_age  %>% 
        rename(`Relative Frequency` = Proportion) %>% 
        mutate(Age = recode(Age, `12-Aug` = "8-12" ))
      jv_age %>% 
        ggplot(aes(x = Age, y = `Relative Frequency`)) +
        geom_col(fill = "midnightblue") +
        labs(x = "Age", y = "Relative Frequency",
             title = "Age Groups of Loudoun Intakes") + 
        theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        coord_flip()
    }
    else if(var3() == "race"){
      jv_race$Race <- factor(jv_race$Race, levels=unique(jv_race$Race))
      jv_race <- jv_race %>% mutate(`Relative Frequency` = Proportion)
      jv_race %>% 
        ggplot(aes(x = Race, y = `Relative Frequency`)) +
        geom_col(fill = "slateblue1") +
        labs(x = "Race", y = "Relative Frequency",
             title = "Racial Demographics of Loudoun Intakes") + 
        theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        coord_flip() 
      
    }
    else if (var3() == "eth") {
      jv_eth$Ethnicity <- factor(jv_eth$Ethnicity, levels=unique(jv_eth$Ethnicity))
      jv_eth <- jv_eth %>% mutate(`Relative Frequency` = Proportion)
      jv_eth %>% 
        ggplot(aes(x = Ethnicity, y = `Relative Frequency`))  +
        geom_col(fill = "lightblue4") +
        labs(x = "Ethnicity", y = "Relative Frequency",
             title = "Ethnic Demographics of Loudoun Intakes") + 
        theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        coord_flip()
      
    }
    else{
      jv_sex$Sex <- factor(jv_sex$Sex, levels=unique(jv_sex$Sex))
      jv_sex <- jv_sex %>% mutate(`Relative Frequency` = Proportion)
      jv_sex %>% 
        ggplot(aes(x = Sex, y = `Relative Frequency`)) +
        geom_col(fill = "cyan" ) +
        labs(x = "Sex", y = "Relative Frequency",
             title = "Sex Demographics of Loudoun Intakes") + 
        theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        coord_flip()
      
      
    }
    
  }) 
  
  #Waitlist for programs in Loudoun 
  output$waitlist <- renderPlotly({
    
    ggplot(waitlist, aes(x=Year)) + 
      geom_line(aes(y = Persons, group = Program, color = Program))
    
  })
  
  
  ## Trees -----------------------------------------------------------
  output$tree1 <- renderCollapsibleTree({
    if(input$pillar1%in%"Education"){
      Tree%>%filter(County == "Loudoun")%>%
        filter(Pillars == "Education")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
    }else if(input$pillar1%in%"Employment"){
      Tree%>%filter(County == "Loudoun")%>%
        filter(Pillars == "Employment")%>%
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar1%in%"Housing"){
      Tree%>%filter(County == "Loudoun")%>%
        filter(Pillars == "Housing")%>%
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar1%in%"Transportation"){
      Tree%>%filter(County == "Loudoun")%>%
        filter(Pillars == "Transportation")%>%
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else {
      Tree%>%filter(County == "Loudoun")%>%
        filter(Pillars == "Health Services")%>%
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }
  })
  
  ## tree for Allegheny  
  output$tree2 <- renderCollapsibleTree({
    if(input$pillar2%in%"Education"){
      Tree%>%filter(County == "Allegheny")%>%
        filter(Pillars == "Education")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
    }else if(input$pillar2%in%"Employment"){
      Tree%>%filter(County == "Allegheny")%>%
        filter(Pillars == "Employment")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar2%in%"Housing"){
      Tree%>%filter(County == "Allegheny")%>%
        filter(Pillars == "Housing")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar2%in%"Transportation"){
      Tree%>%filter(County == "Allegheny")%>%
        filter(Pillars == "Transportation")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else{
      Tree%>%filter(County == "Allegheny")%>%
        filter(Pillars == "Health Services")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }
  })
  
  
  ## tree for Fairfax County 
  output$tree3 <- renderCollapsibleTree({
    if(input$pillar3%in%"Education"){
      Tree%>%filter(County == "Fairfax")%>%
        filter(Pillars == "Education")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
    }else if(input$pillar3%in%"Employment"){
      Tree%>%filter(County == "Fairfax")%>%
        filter(Pillars == "Employment")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar3%in%"Housing"){
      Tree%>%filter(County == "Fairfax")%>%
        filter(Pillars == "Housing")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar3%in%"Transportation"){
      Tree%>%filter(County == "Fairfax")%>%
        filter(Pillars == "Transportation")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else{
      Tree%>%filter(County == "Fairfax")%>%
        filter(Pillars == "Health Services")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range", "Office"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }
  })
  
  
  output$compare <- renderCollapsibleTree({
    if(input$compare1%in%"Education"){
      Tree%>%
        filter(Pillars == "Education")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Available"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
    }else if(input$compare1%in%"Employment"){
      Tree%>%
        filter(Pillars == "Employment")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Available"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$compare1%in%"Housing"){
      Tree%>%
        filter(Pillars == "Housing")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Available"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$compare1%in%"Transportation"){
      Tree%>%
        filter(Pillars == "Transportation")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Available"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else{
      Tree%>%
        filter(Pillars == "Health Services")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Available"),
                        root="Pillars",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }
  })
  
  ## map locations-----------------------------------------------------------
  county <- reactive({
    input$county
  })
  category <- reactive({
    input$category
  })
  
  output$map1 <- renderLeaflet({
    
    if (county() == "Loudoun") {
      if (category() == "Subpopulation") {
        
        labels <- loudoun_locations$Program
        popups <- lapply(
          paste("<strong>Name: </strong>",
                str_to_title(loudoun_locations$Program),
                "<br />",
                "<strong>Qualifications:</strong>",
                loudoun_locations$Qualification ,
                "<br />",
                "<strong>Description:</strong>",
                loudoun_locations$Description, 
                "<br />",
                "<strong>Location:</strong>",
                loudoun_locations$Office,
                "<br />",
                "<strong>Website:</strong>",
                loudoun_locations$Website),
          htmltools::HTML
        )
        
        
        
        l_sub <- loudoun_locations %>% 
          leaflet( options = leafletOptions(minzoom = 12)) %>%
          setView(lng= -77.431622, lat = 39, zoom = 10) %>% 
          addTiles() %>%
          addCircleMarkers(lng = ~Longitude,
                           lat = ~Latitude,
                           label = labels,
                           popup = popups,
                           group = ~loudoun_locations$Subpopulation, radius = 8, color = ~subpop_pal(Subpopulation)) %>%
          addLayersControl(overlayGroups = c("TAYs", "Foster Care", "Juvenile Detention"),
                           options = layersControlOptions(collapsed = FALSE))
        l_sub
        
      }else{
        labels <- loudoun_locations$Program
        popups <- lapply(
          paste("<strong>Name: </strong>",
                str_to_title(loudoun_locations$Program),
                "<br />",
                "<strong>Qualifications:</strong>",
                (loudoun_locations$Qualification) ,
                "<br />",
                "<strong>Description:</strong>",
                (loudoun_locations$Description), 
                "<br />",
                "<strong>Location:</strong>",
                loudoun_locations$Office,
                "<br />",
                "<strong>Website:</strong>",
                loudoun_locations$Website),
          htmltools::HTML
        )
        
        l_pill <- loudoun_locations %>%  
          leaflet(options = leafletOptions(minzoom = 12)) %>% 
          setView(lng= -77.431622, lat = 39, zoom = 10) %>% 
          addTiles() %>%
          addCircleMarkers(lng = ~Longitude, 
                           lat = ~Latitude,
                           radius = 8, 
                           label = labels,
                           popup = popups, 
                           group = ~loudoun_locations$Pillars, 
                           color = ~Pillar_pal(Pillars)) %>%  
          addLayersControl(position = "topright",
                           overlayGroups = Pillar_levels, 
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addLegend(title = "Service Type", position = "topleft", pal = Pillar_pal, values = Pillar_levels)
        
        l_pill
        
      }
    }
    else if (county() == "Fairfax") {
      if (category() == "Subpopulation"){
        
        labels <- fairfax$Program
        popups <- lapply(
          paste("<strong>Name: </strong>",
                str_to_title(fairfax$Program),
                "<br />",
                "<strong>Qualifications:</strong>",
                (fairfax$Qualification) ,
                "<br />",
                "<strong>Description:</strong>",
                (fairfax$Description), 
                "<br />",
                "<strong>Location:</strong>",
                fairfax$Office,
                "<br />",
                "<strong>Website:</strong>",
                fairfax$Website),
          htmltools::HTML
        )
        
        f_sub <- fairfax %>% 
          leaflet( options = leafletOptions(minzoom = 12)) %>%
          setView(lng = -77.2, lat = 38.8, zoom = 10) %>% 
          addTiles() %>%
          addCircleMarkers(lng = ~Longitude,
                           lat = ~Latitude,
                           label = labels,
                           popup = popups,
                           group = ~fairfax$Subpopulation, radius = 8, color = ~subpop_pal(Subpopulation)) %>%
          addLayersControl(overlayGroups = c("TAYs","Foster Care", "Juvenile Detention"),
                           options = layersControlOptions(collapsed = FALSE))
        f_sub
        
      }else {
        labels <- fairfax$Program
        popups <- lapply(
          paste("<strong>Name: </strong>",
                str_to_title(fairfax$Program),
                "<br />",
                "<strong>Qualifications:</strong>",
                (fairfax$Qualification) ,
                "<br />",
                "<strong>Description:</strong>",
                (fairfax$Description), 
                "<br />",
                "<strong>Location:</strong>",
                fairfax$Office,
                "<br />",
                "<strong>Website:</strong>",
                fairfax$Website),
          htmltools::HTML
        )
        
        f_pill <- fairfax %>%  
          leaflet(options = leafletOptions(minzoom = 12)) %>% 
          setView(lng = -77.2, lat = 38.8, zoom = 10) %>% 
          addTiles() %>%
          addCircleMarkers(lng = ~Longitude, 
                           lat = ~Latitude,
                           label = labels, 
                           radius = 8, 
                           popup = popups , 
                           group = ~fairfax$Pillars, 
                           color = ~Pillar_pal(Pillars)) %>%  
          addLayersControl(position = "topright",
                           overlayGroups = Pillar_levels, 
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addLegend(title = "Service Type", position = "topleft", pal = Pillar_pal, values = Pillar_levels)
        
        f_pill
        
        
      }
    }
    else {
      if (category() == "Subpopulation"){
        labels <- allegheny_locations$Program
        popups <- lapply(
          paste("<strong>Name: </strong>",
                str_to_title(allegheny_locations$Program),
                "<br />",
                "<strong>Qualifications:</strong>",
                (allegheny_locations$Qualification) ,
                "<br />",
                "<strong>Description:</strong>",
                (allegheny_locations$Description), 
                "<br />",
                "<strong>Location:</strong>",
                allegheny_locations$Office,
                "<br />",
                "<strong>Website:</strong>",
                allegheny_locations$Website),
          htmltools::HTML
        )
        
        a_sub <- allegheny_locations %>% 
          leaflet( options = leafletOptions(minzoom = 12)) %>%
          setView(lng = -79.997030, lat = 40.5, zoom = 10) %>% 
          addTiles() %>%
          addCircleMarkers(lng = ~Longitude,
                           lat = ~Latitude,
                           label = labels,
                           popup = popups , 
                           group = ~allegheny_locations$Subpopulation, radius = 8, color = ~subpop_pal(Subpopulation)) %>%
          addLayersControl(overlayGroups = c("TAYs","Foster Care", "Juvenile Detention"),
                           options = layersControlOptions(collapsed = FALSE))
        a_sub
        
        
        
      }
      else {
        labels <- allegheny_locations$Program
        popups <- lapply(
          paste("<strong>Name: </strong>",
                str_to_title(allegheny_locations$Program),
                "<br />",
                "<strong>Qualifications:</strong>",
                (allegheny_locations$Qualification) ,
                "<br />",
                "<strong>Description:</strong>",
                (allegheny_locations$Description), 
                "<br />",
                "<strong>Location:</strong>",
                allegheny_locations$Office,
                "<br />",
                "<strong>Website:</strong>",
                allegheny_locations$Website),
          htmltools::HTML
        )
        
        
        a_pill <- allegheny_locations %>%  
          leaflet(options = leafletOptions(minzoom = 12)) %>% 
          setView(lng = -79.997030, lat = 40.5, zoom = 10) %>% 
          addTiles() %>%
          addCircleMarkers(lng = ~Longitude, 
                           lat = ~Latitude,
                           radius = 8, 
                           label = labels,
                           popup = popups , 
                           group = ~allegheny_locations$Pillars, 
                           color = ~Pillar_pal(Pillars)) %>%  
          addLayersControl(position = "topright",
                           overlayGroups = Pillar_levels, 
                           options = layersControlOptions(collapsed = FALSE)) %>%
          addLegend(title = "Service Type", position = "topleft", pal = Pillar_pal, values = Pillar_levels)
        
        a_pill
        
        
        
        
      }
    }
    
    
  })
  ## overtime slider -----------------------------------------------------------
  
  type <- reactive({
    input$type
  })
  output$overtime <- renderLeaflet({
    
    if (type() == "case"){
      
      if (input$year == 2016){
        case <- data.frame(overtime[2:4,2:10]) %>%
          dplyr::select("...2", "X2016", Lat, Long) 
        
      }else if (input$year == 2017){
        case <- data.frame(overtime[2:4,2:10]) %>%
          dplyr::select("...2", "X2017", Lat, Long) 
        
      }else if (input$year == 2018){
        case <- data.frame(overtime[2:4,2:10]) %>%
          dplyr::select("...2", "X2018", Lat, Long) 
        
      }else if (input$year == 2019){
        case <- data.frame(overtime[2:4,2:10]) %>%
          dplyr::select("...2", "X2019", Lat, Long) 
        
      }else {
        case <- data.frame(overtime[2:4,2:10]) %>%
          dplyr::select("...2", "X2020", Lat, Long) 
        
      }
      
      
      
      colnames(case) <- c("Zip", "Number", "Lat", "Long")
      labels <- lapply(
        paste("<strong>Zip Code: </strong>",
              str_to_title(case$Zip),
              "<br />",
              "<strong>Individuals Served:</strong>",
              case$Number),
        htmltools::HTML
      )
      
      pal <- colorNumeric(palette = "viridis", 
                          domain = both$both)
      
      
      labelsP <- lapply(
        paste("<strong>Area: </strong>",
              both$NAME,
              "<br />",
              "<strong>Total Population: </strong>",
              formatC(both$both, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      
      both %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet(width = "100%") %>%
        addTiles() %>%
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.5,
                    label = labelsP,
                    color = ~ pal(both)) %>%
        addCircleMarkers(lng = ~case$Long, lat = ~case$Lat,
                         radius = ~case$Number/5, 
                         color = "orange",
                         fillOpacity = 2,
                         label = labels)%>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = ~both,
                  title = "Total Population",
                  opacity = .7)
      
    } else if (type() == "dis") {
      
      if (input$year == 2016){
        dis <- data.frame(overtime[8:9,2:10]) %>%
          dplyr::select("...2", "X2016", Lat, Long) 
        
      }else if (input$year == 2017){
        dis <- data.frame(overtime[8:9,2:10]) %>%
          dplyr::select("...2", "X2017", Lat, Long) 
        
      }else if (input$year == 2018){
        dis <- data.frame(overtime[8:9,2:10]) %>%
          dplyr::select("...2", "X2018", Lat, Long) 
        
      }else if (input$year == 2019){
        dis <- data.frame(overtime[8:9,2:10]) %>%
          dplyr::select("...2", "X2019", Lat, Long) 
        
      }else {
        dis <- data.frame(overtime[8:9,2:10]) %>%
          dplyr::select("...2", "X2020", Lat, Long) 
        
      }
      
      
      colnames(dis) <- c("Zip", "Number", "Lat", "Long")
      labels <- lapply(
        paste("<strong>Zip Code: </strong>",
              str_to_title(dis$Zip),
              "<br />",
              "<strong>Individuals Served:</strong>",
              dis$Number),
        htmltools::HTML
      )
      
      pal <- colorNumeric(palette = "viridis", 
                          domain = both$both)
      
      
      labelsP <- lapply(
        paste("<strong>Area: </strong>",
              both$NAME,
              "<br />",
              "<strong>Total Population: </strong>",
              formatC(both$both, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      
      both %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet(width = "100%") %>%
        addTiles() %>%
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.5,
                    label = labelsP,
                    color = ~ pal(both)) %>%
        addCircleMarkers(lng = ~dis$Long, lat = ~dis$Lat,
                         radius = ~dis$Number/5, 
                         color = "orange",
                         fillOpacity = 2,
                         label = labels)%>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = ~both,
                  title = "Total Population",
                  opacity = .7)
      
      
    }else if (type() == "emer"){
      
      if (input$year == 2016){
        emer <- data.frame(overtime[12:14,2:10]) %>%
          dplyr::select("...2", "X2016", Lat, Long) 
        
      }else if (input$year == 2017){
        emer <- data.frame(overtime[12:14,2:10]) %>%
          dplyr::select("...2", "X2017", Lat, Long) 
        
      }else if (input$year == 2018){
        emer <- data.frame(overtime[12:14,2:10]) %>%
          dplyr::select("...2", "X2018", Lat, Long) 
        
      }else if (input$year ==2019){
        emer <- data.frame(overtime[12:14,2:10]) %>%
          dplyr::select("...2", "X2019", Lat, Long) 
        
      }else {
        emer <- data.frame(overtime[12:14,2:10]) %>%
          dplyr::select("...2", "X2020", Lat, Long) 
        
      }
      
      
      colnames(emer) <- c("Zip", "Number", "Lat", "Long")
      labels <- lapply(
        paste("<strong>Zip Code: </strong>",
              str_to_title(emer$Zip),
              "<br />",
              "<strong>Individuals Served:</strong>",
              emer$Number),
        htmltools::HTML
      )
      
      pal <- colorNumeric(palette = "viridis", 
                          domain = both$both)
      
      
      labelsP <- lapply(
        paste("<strong>Area: </strong>",
              both$NAME,
              "<br />",
              "<strong>Total Population: </strong>",
              formatC(both$both, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      
      both %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet(width = "100%") %>%
        addTiles() %>%
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.5,
                    label = labelsP,
                    color = ~ pal(both)) %>%
        addCircleMarkers(lng = ~emer$Long, lat = ~emer$Lat,
                         radius = ~emer$Number/5, 
                         color = "orange",
                         fillOpacity = 2,
                         label = labels)%>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = ~both,
                  title = "Total Population",
                  opacity = .7)
      
      
    }else if (type() == "employ"){
      
      if (input$year == 2016){
        employ <- data.frame(overtime[18,2:10]) %>%
          dplyr::select("...2", "X2016", Lat, Long) 
        
      }else if (input$year == 2017){
        employ <- data.frame(overtime[18,2:10]) %>%
          dplyr::select("...2", "X2017", Lat, Long) 
        
      }else if (input$year == 2018){
        employ <- data.frame(overtime[18,2:10]) %>%
          dplyr::select("...2", "X2018", Lat, Long) 
        
      }else if (input$year == 2019){
        employ <- data.frame(overtime[18,2:10]) %>%
          dplyr::select("...2", "X2019", Lat, Long) 
        
      }else {
        employ <- data.frame(overtime[18,2:10]) %>%
          dplyr::select("...2", "X2020", Lat, Long) 
        
      }
      
      
      colnames(employ) <- c("Zip", "Number", "Lat", "Long")
      labels <- lapply(
        paste("<strong>Zip Code: </strong>",
              str_to_title(employ$Zip),
              "<br />",
              "<strong>Individuals Served:</strong>",
              employ$Number),
        htmltools::HTML
      )
      
      pal <- colorNumeric(palette = "viridis", 
                          domain = both$both)
      
      
      labelsP <- lapply(
        paste("<strong>Area: </strong>",
              both$NAME,
              "<br />",
              "<strong>Total Population: </strong>",
              formatC(both$both, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      
      both %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet(width = "100%") %>%
        addTiles() %>%
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.5,
                    label = labelsP,
                    color = ~ pal(both)) %>%
        addCircleMarkers(lng = ~employ$Long, lat = ~employ$Lat,
                         radius = ~employ$Number/5, 
                         color = "orange",
                         fillOpacity = 2,
                         label = labels)%>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = ~both,
                  title = "Total Population",
                  opacity = .7)
      
      
    }else if (type() == "out"){
      
      if (input$year == 2016){
        out <- data.frame(overtime[21:23,2:10]) %>%
          dplyr::select("...2", "X2016", Lat, Long) 
        
      }else if (input$year == 2017){
        out <- data.frame(overtime[21:23,2:10]) %>%
          dplyr::select("...2", "X2017", Lat, Long) 
        
      }else if (input$year == 2018){
        out <- data.frame(overtime[21:23,2:10]) %>%
          dplyr::select("...2", "X2018", Lat, Long) 
        
      }else if (input$year == 2019){
        out <- data.frame(overtime[21:23,2:10]) %>%
          dplyr::select("...2", "X2019", Lat, Long) 
        
      }else {
        out <- data.frame(overtime[21:23,2:10]) %>%
          dplyr::select("...2", "X2020", Lat, Long) 
        
      }
      
      
      colnames(out) <- c("Zip", "Number", "Lat", "Long")
      labels <- lapply(
        paste("<strong>Zip Code: </strong>",
              str_to_title(out$Zip),
              "<br />",
              "<strong>Individuals Served:</strong>",
              out$Number),
        htmltools::HTML
      )
      
      pal <- colorNumeric(palette = "viridis", 
                          domain = both$both)
      
      
      labelsP <- lapply(
        paste("<strong>Area: </strong>",
              both$NAME,
              "<br />",
              "<strong>Total Population: </strong>",
              formatC(both$both, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      
      both %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet(width = "100%") %>%
        addTiles() %>%
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.5,
                    label = labelsP,
                    color = ~ pal(both)) %>%
        addCircleMarkers(lng = ~out$Long, lat = ~out$Lat,
                         radius = ~out$Number/5, 
                         color = "orange",
                         fillOpacity = 2,
                         label = labels)%>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = ~both,
                  title = "Total Population",
                  opacity = .7)
      
      
    }else {
      
      if (input$year == 2016){
        res <- data.frame(overtime[27:31,2:10]) %>%
          dplyr::select("...2", "X2016", Lat, Long) 
        
      }else if (input$year == 2017){
        res <- data.frame(overtime[27:31,2:10]) %>%
          dplyr::select("...2", "X2017", Lat, Long) 
        
      }else if (input$year == 2018){
        res <- data.frame(overtime[27:31,2:10]) %>%
          dplyr::select("...2", "X2018", Lat, Long) 
        
      }else if (input$year == 2019){
        res <- data.frame(overtime[27:31,2:10]) %>%
          dplyr::select("...2", "X2019", Lat, Long) 
        
      }else{
        res <- data.frame(overtime[27:31,2:10]) %>%
          dplyr::select("...2", "X2020", Lat, Long) 
        
      }
      
      
      colnames(res) <- c("Zip", "Number", "Lat", "Long")
      labels <- lapply(
        paste("<strong>Zip Code: </strong>",
              str_to_title(res$Zip),
              "<br />",
              "<strong>Individuals Served:</strong>",
              res$Number),
        htmltools::HTML
      )
      
      pal <- colorNumeric(palette = "viridis", 
                          domain = both$both)
      
      
      labelsP <- lapply(
        paste("<strong>Area: </strong>",
              both$NAME,
              "<br />",
              "<strong>Total Population: </strong>",
              formatC(both$both, format = "f", big.mark =",", digits = 0)),
        htmltools::HTML
      )
      
      
      both %>%
        st_transform(crs = "+init=epsg:4326") %>%
        leaflet(width = "100%") %>%
        addTiles() %>%
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.5,
                    label = labelsP,
                    color = ~ pal(both)) %>%
        addCircleMarkers(lng = ~res$Long, lat = ~res$Lat,
                         radius = ~res$Number/5, 
                         color = "orange",
                         fillOpacity = 2,
                         label = labels)%>%
        addLegend("bottomright", 
                  pal = pal, 
                  values = ~both,
                  title = "Total Population",
                  opacity = .7)
      
      
      
    }
    
    
  })
  
  
  ## count -----------------------------------------------------------
  
  output$table1 <- renderTable({
    table <- read.csv("data/program_subpop_counts.csv")
    table
    
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  output$table2 <- renderTable({
    table <- read.csv("data/program_pillar_counts.csv")
    table
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  
  type2 <- reactive({
    input$type2
  })
  
  output$plotServed <- renderPlotly({
    if (type2() == "med"){
      ggplot() + 
        geom_line(mapping = aes(Year, `Adults, Pregnant Women and Children`, group = 1), data = total) + 
        geom_line(mapping = aes(Year, Total, group = 1), data = total, linetype = "dashed", color="blue", size = 2) + 
        labs(y = "Persons", 
             title = "Medcaid for Adults, Pregnant Women and Children compared with the Total")
      
    }else if (type2() == "literacy"){
      ggplot() + geom_col(mapping = aes(Type, Number), data = programs,fill =  "deepskyblue1") + 
        labs(title = "Adult Literacy Program",
             y = "Persons Served")+
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"))
      
    }else if (type2() == "oar") {
      
      ggplot() + geom_col(mapping = aes(Category, Number), data = all[1:2,],fill =  "lightblue2") + 
        labs(title = "Oar Nova 2020",
             y = "Persons Served")+
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"))
      
    }
    #Oxford houses
    else {
      
      ggplot() + geom_col(mapping = aes(Category, Number), data = ox[1:2,],fill =  "cornflowerblue") + 
        labs(title = "Oxford Houses in VA 2019-2020",
             y = "Months") +
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"))
      
    }
    
    
  })
  output$plotServed2 <- renderPlotly({
    if (type2() == "med"){
      ggplot() + 
        geom_line(mapping = aes(Year, `Childless Adults`, group = 1), data = med) + 
        geom_line(mapping = aes(Year, Children, group = 1), data = med, linetype = "dotted", color="blue", size = 2) + 
        labs(y = "Persons", 
             title = "Medcaid for Children and Childless Adults")
      
    }else if (type2() == "literacy"){
      
      # ggplot(persons, aes(x="", y=Number, fill=Race)) +
      #   geom_bar(stat="identity", width=1) +
      #   coord_polar("y", start=0)+theme_void() 
      
    }else if (type2() == "oar") {
      
      ggplot() + geom_col(mapping = aes(Category, Number), data = all[3:8,],fill =  "lightblue2") + 
        labs(title = "Oar Nova 2020",
             y = "Percent Served") +
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"))
      
    }
    #Oxford houses
    else {
      
      ggplot() + geom_col(mapping = aes(Category, Number), data = ox[3:5,],fill =  "cornflowerblue") + 
        labs(title = "Oxford Houses in VA 2019-2020",
             y = "%/Years") +
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"))
      
    }
    
    
  })
  ## infoBox -----------------------------------------------------------
  
  output$medicaid <- renderInfoBox({
    
    infoBox(title = "Medicaid", 
            value = 244362, 
            subtitle = "Childless Adults Served by Medicaid 2021",
            icon = icon("clinic-medical"),
            fill = TRUE)
  })
  
  output$wioa <- renderInfoBox({
    
    infoBox(title = "WIOA", value = 28, 
            subtitle = "Youths enrolled at WIOA 2020",
            icon = icon("briefcase"), color = "green",
            fill = T)
  })
  
  output$transit <- renderInfoBox({
    
    infoBox(title = "Route 54 Safe-T", value = "52, 57, 42", 
            subtitle = "Average Riders Weekdays, Saturday, Sunday",
            icon = icon("bus"),
            color = "maroon",
            fill = T)
  })
  
  output$food <- renderInfoBox({
    
    infoBox(title = "SNAP", value = 175, 
            subtitle = "TAYs were on SNAP benefits in 2020",
            icon = icon("utensils"),
            color = "purple",
            fill = T)
  })
  
  output$shelters <- renderInfoBox({
    
    infoBox(title = "Shelters", value = 27, 
            subtitle = "TAYs used Emergency Shelters in 2019" , 
            icon = icon("house-user"),
            color = "red",
            fill = T)
  })
  
  output$homeless <- renderInfoBox({
    
    infoBox(title = "Homelessness", value = 22, 
            subtitle = "Homeless TAYs in 2020" , 
            icon = icon("home"),
            color = "light-blue",
            fill = T)
  })
}


# Run the application-----------
shinyApp(ui = ui, server = server)
