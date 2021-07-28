library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(maps)
library(plotly)
library(DT)
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
library(tidygeocoder)
options(tigris_use_cache = TRUE)

# census_api_key("6f1a78212175773dd80d1a03bd303e8d181a6096", install = TRUE, overwrite = T)
# readRenviron("~/.Renviron")



# # Yang's API Key
# census_api_key("58cb9357dee9edf8330e47865d207929ab8baeb3", install = FALSE )
# Sys.getenv("CENSUS_API_KEY")
# # I am seeting my working directory
# setwd("G:/My Drive/PhD/Internship/Loudoun/2021_DSPG_Loudoun/2021_DSPG_Loudoun/ShinyApp")

# Data-----------------------------------------------------------
loudoun <- read_excel(paste0(getwd(), "/data/demographics-loudoun.xlsx"))
## gender and age tays
l_ages_gender <- read.csv(paste0(getwd(),"/data/ages_gender.csv")) 

# race tays 
race <- loudoun[1:6, 1:2]

colnames(race) <- c("Race", "Estimate", "Sum") 
race <- race%>%
  mutate(Sum = sum(race$Estimate))%>%
  mutate(Percent = Estimate/Sum*100)

race$Race <- factor(race$Race, levels=unique(race$Race))

eth_totals <- read.csv(paste0(getwd(), "/data/ethniciy.csv")) 

# education 
education <- read.csv(paste0(getwd(),"/data/both_education.csv")) 

## Poverty 
poverty <- read.csv(paste0(getwd(),"/data/poverty.csv")) 
s <- sum(poverty$estimate)

w_p <- data.frame(poverty[1,], poverty[2,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "White")%>%
  mutate(Percent = sum/s*100)
b_p <- data.frame(poverty[3,], poverty[4,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "Black")%>%
  mutate(Percent = sum/s*100)
i_p <- data.frame(poverty[5,], poverty[6,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "American Indian/Alaska Native")%>%
  mutate(Percent = sum/s*100)
as_p <- data.frame(poverty[7,], poverty[8,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "Asian")%>%
  mutate(Percent = sum/s*100)
n_p <- data.frame(poverty[9,], poverty[10,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "Native Hawai’ian/Pacific Islander")%>%
  mutate(Percent = sum/s*100)
o_p <- data.frame(poverty[11,], poverty[12,])%>%
  mutate(sum = estimate + estimate.1)%>%
  dplyr::select(variable, sum)%>%
  mutate(variable = "Other")%>%
  mutate(Percent = sum/s*100)

# Health Care
healthcare <- loudoun[1:4, 4:5]
colnames(healthcare) <- c("Type", "Estimate")
healthcare$Type <- factor(healthcare$Type, levels=unique(healthcare$Type))

#Homelessness
homeless <- loudoun[1:5, 22:23]
colnames(homeless) <- c("Year", "TAY")


# Mental Illness
smiwaitlist <- read_excel(paste0(getwd(),"/data/smi-waitlist.xlsx") ) 
smi <- smiwaitlist[11:20,1:3]
colnames(smi) <- c("Group", "Year", "Persons")
smi$Persons <- as.numeric(smi$Persons)

waitlist <- smiwaitlist[6:25,9:11]
colnames(waitlist) <- c( "Program", "Year", "Persons")

waitlist$Persons <- as.numeric(waitlist$Persons)
waitlist$Program <- factor(waitlist$Program, levels=unique(waitlist$Program))


# Foster Care -----------------------------------------------------------
fc_virginia <- read_excel(paste0(getwd(),"/data/foster-care-2020-all.xlsx")) 
#Age
totals <- data.frame(fc_virginia[c(2:38),])

colnames(totals) <- c("Age Group", "Value")
groups <- c("Under 1", "1-5", "6-9", "10-12", "13-15", "16-18", "19+")
fc_ages <-data.frame(totals[c(24, 26, 28,30,32,34,36),])
fc_ages$Value <- as.numeric(fc_ages$Value)
fc_ages$Age.Group <- groups
#Race and ethnicity 
fc_races <- data.frame(totals[c(7,9,13,17),])

colnames(fc_races) <- c("Race", "Value")
fc_races[5,2] <- 9
fc_races$Race <- c("Black", "White", "Asian", "Multi", "Unknown/Missing") 
fc_races$Race <- factor(fc_races$Race, levels=unique(fc_races$Race))

eth <- rep(c("Hispanic" , "Non-Hispanic") , 1) 
value <- c(14, 34)
fc_eth <- data.frame(eth,value)

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

jv_race$Race <- factor(jv_race$Race, levels=unique(jv_race$Race))



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
both <- readRDS(paste0(getwd(),"/data/pop.rds")) 
both <- st_transform(both, '+proj=longlat +datum=WGS84')


# Trees -----------------------------------------------------------
Tree <- read_excel(paste0(getwd(),"/data/combined-programs.xlsx")) 
# List -----------------------------------------------------------
list <- read_excel(paste0(getwd(),"/data/complete-list.xlsx")) 
# Maps -----------------------------------------------------------
## Locations ---------
map <- read_excel(paste0(getwd(),"/data/combined-programs.xlsx")) 
loudoun_locations <- map %>%
  filter(County == "Loudoun") %>% 
  filter(Longitude != "Online" & Longitude != "Mulitple locations") %>% drop_na()

loudoun_locations$Longitude <- as.numeric(loudoun_locations$Longitude)
loudoun_locations$Latitude <- as.numeric(loudoun_locations$Latitude)


allegheny_locations <- map%>%
  filter(County == "Allegheny") %>% 
  filter(Longitude != "Online" & Longitude != "Mulitple locations") %>% drop_na()

allegheny_locations$Longitude <- as.numeric(allegheny_locations$Longitude)
allegheny_locations$Latitude <- as.numeric(allegheny_locations$Latitude)


fairfax <- map%>%
  filter(County == "Fairfax") %>% 
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
va_zips <- readRDS(paste0(getwd(),"/data/va_zips.rds")) 
va_zips <- st_transform(va_zips, '+proj=longlat +datum=WGS84')
loudoun_zip_link <- "http://ciclt.net/sn/clt/capitolimpact/gw_ziplist.aspx?ClientCode=capitolimpact&State=va&StName=Virginia&StFIPS=51&FIPS=51107"
loudoun_zip_codes <- read_html(loudoun_zip_link) %>% html_node("td table") %>%  
  html_table() %>% dplyr::select(c(1,2)) %>% dplyr::rename(`Zip Code` = X1, City = X2) %>%
  slice(-c(1, 2)) 
loudoun_zip_code_city_names <- loudoun_zip_codes %>% pull(City)
loudoun_zip_codes <- pull(loudoun_zip_codes, `Zip Code`)
loudoun_zips <- va_zips %>% filter(ZCTA5CE10 %in% loudoun_zip_codes)


overtime <- read_excel(paste0(getwd(), "/data/program-services-overtime.xlsx"))

## Persons Served------------
enroll <- read_excel(paste0(getwd(), "/data/medicaid-enrollment.xlsx")) 

total <- enroll[1:13,]
total$`Adults, Pregnant Women and Children` <- as.numeric(total$`Adults, Pregnant Women and Children`)
med <- enroll[16:20,1:3]
colnames(med) <- c("Year", "Children", "Childless Adults")
med$`Childless Adults` <- as.numeric(med$`Childless Adults`)
med$Children <- as.numeric(med$Children)


#Oxford
ox <- loudoun[1:5, 13:14]
colnames(ox) <- c("Category", "Number")
ox$Category <- factor(ox$Category, levels=unique(ox$Category))

#Adult Literacy Program
literacy <- loudoun[1:5, 7:8]
colnames(literacy) <- c("Type", "Number")
literacy$Type <- factor(literacy$Type, levels=unique(literacy$Type))


literacy_demo <- loudoun[1:4, 10:11]
colnames(literacy_demo) <- c("Race", "Percent")
literacy_demo$Race <- factor(literacy_demo$Race, levels=unique(literacy_demo$Race))


#OAR
all <- loudoun[1:7, 16:17]
colnames(all) <- c("Category", "Number")
all$Category <- factor(all$Category, levels=unique(all$Category))


# Public Benefits
youth <- read_excel(paste0(getwd(), "/data/vce-youth.xlsx"))
emerR <- youth[1:4, 1:2]
colnames(emerR) <- c("Race", "Number")
emerG <- youth[1:2, 4:5]
colnames(emerG) <- c("Gender", "Number")
emerT <- youth[1:5,7:9]%>%mutate(Percent...8 = Percent...8*100)
emerT$Program <- "Emergency Shelter"
colnames(emerT) <- c("Number", "Percent", "Year", "Program")

publicR <- youth[1:6, 11:12]
colnames(publicR) <- c("Race", "Number")
publicG <- youth[1:2, 14:15]
colnames(publicG) <- c("Gender", "Number")
publicT <- youth[1:15, 18:20]%>%mutate(Percent...19 = Percent...19*100)
publicT$Program <- youth$Group
colnames(publicT) <- c("Number", "Percent", "Year","Program")

wrc <- youth[1:15, 22:25]%>%mutate(Percent...24 = Percent...24*100)
colnames(wrc) <- c("Age", "Number", "Percent", "Year")

wioa <- youth[1:4, 28]
wioa$Percent <- 0 
wioa$Year <- youth$Year...29[1:4]
wioa$Program <- c("WIOA Youth")
colnames(wioa) <- c("Number", "Percent", "Year", "Program")

transR <- youth[1:4, 31:32]
colnames(transR) <- c("Race", "Number")
transG <- youth[1:2, 34:35]
colnames(transG) <- c("Gender", "Number")
transT <- youth[1:5, 37:39]
transT$Program <- "Transitional Housing"
colnames(transT) <- c( "Number", "Percent", "Year", "Program")


#DMHSA 
mhs_race_relative <- read.csv(paste0(getwd(), "/data/race_line.csv")) 
mhs_sex_relative <- read.csv(paste0(getwd(), "/data/sex_line.csv")) 

# Loudoun Cares
cares <- loudoun[1:9, 19:20]
colnames(cares) <- c("Stat", "Name")
cares$Name <- factor(cares$Name, levels=unique(cares$Name))



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
                 
                 ## Tab Introduction to Loudoun County-------------------------------------------
                 navbarMenu("Sociodemographics",
                            ### Tab TAY--------------------------------------------
                            tabPanel("Target Population",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Transition Aged Youths' Sociodemographic Characteristics"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(4, 
                                                     h4(strong("Who does Loudoun County Serve?")),
                                                     p("The 2019 American Community Survey (ACS) estimates that transition-aged youths (ages 18-24) make up 5% of the population in Loudoun county.  
                                                       Of these youths, are living below the poverty line, and only percent attaining high school degree.  
                                                       The number of homeless transition-age youth (TAY) has increased in Loudoun, as 18 more individuals were 
                                                       counted as homeless in 2020 than in 201. This represents a 450% increase.  "), 
                                                     p("TAYs are usually faced with these challenges on their way to adulthood, given the lack of financial 
                                                       resources, support, or knowledge to maintain independence. This includes the difficulty to obtain health 
                                                       insurance through Medicaid or SCHIP (State Children’s Health Insurance Program). This transition is even more 
                                                       difficult for those “aging out” of foster care or leaving juvenile detention facilities who face 
                                                       significant challenges in finding employment and affordable housing given their history. ")
                                                     
                                                 ), 
                                              column(8, 
                                                     h4(strong("Residents' Socioeconomic Characteristics")),
                                                     selectInput("var1", "Select Socioeconomic Characteristic:", width = "100%", choices = c(
                                                       "Gender and Age Groups" = "age",
                                                       # "Percentage of TAYs by Gender" = "percent", 
                                                       "Educational Attainment" = "education",
                                                       "Race by Number" = "race", 
                                                       "Race by Percent" = "raceP", 
                                                       "Ethnicity by Gender" = "ethP", 
                                                       "Poverty Level by Race" = "poverty",
                                                       "Homeless TAY in Loudoun" = "home", 
                                                       "Types of Healthcare Coverage" = "health")
                                                     ),
                                                     plotlyOutput("plot1"),
                                                     p(tags$small("Data Source: American Community Survey 2019 1-Year/5-Year Estimates."))
                                              )
                                     )) ,
                            ### Tab subpopulation--------------------------------------------
                            tabPanel("Subpopulation",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Subpopulations' Sociodemographic Characteristics" ), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(4, 
                                                     h4(strong("Foster Care")),
                                                     p("According to the Adoption and Foster Care Analysis and Reporting System, in 2020 there were 48 children from infancy to age 
                                                     21 in foster care in Loudoun County with over half (67%) being boys. There are also significant differences in the racial 
                                                     demographics as almost 50% are white and about 25% black and less than 5% being Asian.  "),
                                                     p("When we examine only transitional-aged youths, there were only 8 children reported in the foster care system. 
                                                       No public record exists for young adults after they leave foster care as such, we do not have information for 
                                                       youth ages 21-24. Notably, the largest age group currently in foster care, ages 16-18, will be aging out over 
                                                       the next 5 years. It is therefore important to develop and provide services that will help with their transition into
                                                       adulthood within the next few years. ") , 
                                                     tags$br(), 
                                                     h4(strong("Juvenile Detention")),
                                                     
                                                     p("Juvenile legal status lasts through age 20 in Virginia so we could not conduct analysis 
                                                        for the full age range of the TAY population.  However, consistent with the state-level 
                                                        trend, the total number of new youth intakes (ages 8-20) has been declining in Loudoun 
                                                        over the last several years. Between 2018 and 2020, juvenile intake cases decreased by 
                                                        about 20 percent, moving from 1,137 to 913 cases."),
                                                        
                                                      p("Though intakes have declined, there 
                                                        are some significant differences across socioeconomic characteristics.  70% of 
                                                        newly admitted cases in 2020 are male. While Black youth make up only * percent of Loudoun 
                                                        TAY population, they accounted for 15% of juvenile intakes. The majority of youth admitted 
                                                        are aged 14-17; the largest category (almost 30%) being age 17."),
                                                        
                                                        p("It is difficult to analyze socioeconomic patterns for juvenile delinquents after age 20 which would provide us with a 
                                                        guide of the necessary services they would need to help with their transition to adulthood. 
                                                        However, in 2019, over 90 percent of youth incarcerated in Virginia had significant mental 
                                                        health disorder. 1 In addition, there is a high recidivism rate (over 50%) and only 35 total 
                                                        youth offenders received a high school diploma or GED. This snapshot suggests that TAYs leaving 
                                                        the juvenile system would greatly benefit from the availability of health services, education, and employment.")
                                                        ), 
                                              column(8, h4(strong("Subpopulations' Socioeconomic Characteristics")),
                                                     tabsetPanel(
                                                       tabPanel("Foster Care",
                                                                p("", style = "padding-top:10px;"),
                                                                selectInput("var2", "Select Socioeconomic Characteristic:", width = "100%", choices = c(
                                                                  "Age" = "age",
                                                                  "Sex" = "sex",
                                                                  "Race" = "race",
                                                                  "Ethnicity" = "eth")
                                                                ),
                                                                plotlyOutput("plot2"),
                                                                p(tags$small("Data source: The Adoption and Foster Care Analysis and Reporting System 2019"))
                                                                
                                                       ),
                                                       tabPanel("Juvenile Detention",
                                                                p("", style = "padding-top:10px;"),
                                                                selectInput("var3", "Select Socioeconomic Characteristic:", width = "100%", choices = c(
                                                                  "Age" = "age",
                                                                  "Sex" = "sex",
                                                                  "Race" = "race",
                                                                  "Ethnicity" = "eth")
                                                                ),
                                                                plotlyOutput("plot3"),
                                                                p(tags$small("Data source: Department of Juvenile Justice (DJJ) 2020 Data Resource Guide"))
                                                                
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
                                          h4(strong("Methodology")),
                                          p("Yang")), 
                                   column(8,  h4(strong("Data Sources"))), 
                                   column(4,
                                          img(src = 'data-acs.png', style = "display: inline; float: left;", width = "200px"),
                                          p("We retrieve ",strong("American Community Survey (ACS)")," data to examine demographic and socioeconomic characteristics of our target population. 
                                            ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets.
                                            We used the most recently available 1-year/5-year estimates, to characterize Loudoun County’s transition aged youths by age, race,
                                            gender, educational attainment, health insurance coverage, and poverty level. "),
                                          br(), 
                                          img(src = 'family-services.jpg', style = "display: inline; float: left;", width = "200px"),
                                          p("The ", strong("Loudoun County Department of Family Services"), "holds record of those who use their provided services based on number of persons, percent of transition aged youth 
                                            and year. We graphed several demographics like gender, race and age for mulitple programs and showed a timeseries of utilization from 2016-2020. "), 
                                          br(), 
                                          img(src = 'data-virginiaDSS.jpeg', style = "display: inline; float: left;", width = "200px"),
                                          p("The ", strong("Virginia Department of Social Services"), " ensures that thousands of Virginia's most vulnerable citizens have access to the best services and benefits available to them.
                                            We researched through their website and found various programs and services that are available to transition aged youth in Loudoun and Fairfax in order to find the gaps in
                                            certain pillars. ")) ,
                                  column(4,
                                          img(src = 'family-services.jpg', style = "display: inline; float: left;", width = "200px"),
                                          p("The ", strong("Loudoun County Department of Mental Health, Substance Abuse, and Developmental Services"), "reports the number of individuals that use their provided programs and different demographics like
                                            age, gender and race. They split their data based on zipcodes which we used to map out the utilization of their provided services and compare to the area's population density. "),
                                         br(), 
                                         img(src = 'data-afcars.png', style = "display: inline; float: left;", width = "300px"),
                                         p("We used ", strong("The Adoption and Foster Care Analysis and Reporting System")," to report the number of youths in foster care in Loudoun County. 
                                            This allowed us to determine how many youths need services to help with the transition out of the foster care system.  "),
                                         br(), 
                                         img(src = 'data-djj.jpg', style = "display: inline; float: left;", width = "150px"),
                                         p("The ", strong("Virginia’s Department of Juvenile Justice")," produces a Data Resource Guide annually highlighting data and trends on
                                                  the juvenile detention centers through the Commonwealth. We used the 2019 report to determine the demographic characteristics 
                                                  and the total number youth intakes and those leaving the centers.  ")
                                          
                                          )
                                   
                          )
                          
                 ),
                 
                 
                 ## Tab Services--------------------------------------------
                 navbarMenu("Service",
                            tabPanel("Availability", value = "services", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Service Availability"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                     tabsetPanel(
                                           tabPanel("Services", 
                                             fluidRow(style = "margin: 6px;",
                                                             p("", style = "padding-top:10px;"), 
                                                    column(6, 
                                                           h4(strong("What Services and Programs are available? ")),
                                                           p("The literature suggests that there are five main areas 
                                                             TAYs struggle in their transition to adulthood: educational 
                                                             attainment, employment, housing, transportation, and access 
                                                             to healthcare services. Motivated by the literature, we examine 
                                                             the availability of services in Loudoun in these five pillars along 
                                                             with the availability specific to the foster care or juvenile delinquents’ 
                                                             subpopulation. We also cross-county comparison with Fairfax (also in Virginia) 
                                                             and Allegheny (Pennsylvania) to determine whether service gaps differ within 
                                                             Virginia or by states."), 
                                                           p("The following tables provide counts of programs  
                                                             and services available in each county by subpopulation and by pillar.
                                                             They allow for simple comparison of program types across counties."), 
                                                           
                                                           p("To gather this information for the interactive tree diagrams, our team researched and web scrapping for three weeks the different programs available to transition aged youth, either
                                                             specific to foster care or juvenile detention subpopulations or in general for 18-24 year olds in Loudoun County, Fairfax County and Allegheny County. 
                                                             Our team searched as if we were the young adult looking for this information and noticed how some programs and services regarding the transportation and housing 
                                                             pillar were not easily accessible and were hidden in multiple documents. "),
                                                           
                                                           p("It is to be noted that the greater majority of the programs/services found 
                                                             across all counties were directed towards vulnerable TAY in general.  This highlights
                                                             a lack of targeted programs and a potential need to increase the number of those
                                                             programs and services specifically catering to the needs of those formerly involved
                                                             in the juvenile detention or foster care system."), 
                                                           
                                                           p("Looking at the distribution of programs by pillar in Loudoun, it can be seen that 
                                                             the number of programs/services are fairly equal by pillar, with the notable exception 
                                                             of transportation. Transportation services in Loudoun county useful to vulnerable TAY 
                                                             are not targeted to those formally involved in Foster Care or Juvenile detention, and 
                                                             are rather limited outside of medical transportation and commuter buses. This illustrates 
                                                             the potential need to expand transportation services to those TAY formerly involved in Foster Care 
                                                             or the Juvenile Detention system. ")) ,
                                                    column(6, 
                                                           h4(strong("Number of Programs by Subpopulation")), 
                                                           tableOutput("table1"),
                                                           tags$br(), 
                                                           h4(strong("Number of Programs by Pillar")), 
                                                           tableOutput("table2")))  ,
                                           tags$br(), 
                                           fluidRow(style = "margin: 6px;",
                                                    p("", style = "padding-top:10px;"), 
                                                    column(4, 
                                                           h4(strong("What Services and Programs are Available? ")),
                                                           p("These interactive tree diagrams allow for the comparison of programs/services at the county 
                                                             level in Loudoun, Fairfax and Allegheny county. Each diagram represents
                                                             the programs of the selected pillar in the selected county. Nodes represent first the pillar, then the targeted subpopulation,
                                                             followed by the program/service name, then the intended age range of the program/service and lastly the city or county location of its office. "),
                                                           
                                                           p("We chose to compare Loudoun County to Fairfax County and Allegheny County for two reasons. 
                                                             As the stakeholders had mentioned, Fairfax and Loudoun are constantly being compared because of their close proximity and similar demographics. Therefore, we looked at programs in each of these counties to see 
                                                             if the gaps in services was for only Loudoun county or in the DC Metropolitan area, Northern Virginia area. Second, we chose Allegheny County because based on our research, they have a successful
                                                             transition rate of young adults 18-24 and looking at the programs available for those previously involved in foster care and juvenile detention, it seems that they have more programs specific towards them. 
                                                             Allegheny County is a great county to look at and see what they are doing compared to a county like Loudoun who wants to provide more programs and services to vulnerable transition aged youth in the future. 
                                                             "), 
                                                           
                                                           p("Using these interactive trees, we can see all of the programs we web scrapped for each of the counties with the last node being where the person must go to apply. 
                                                             Since the pandemic, most of the applications can be done online but obviously some of the programs and services are in person. With this information, we can see how easy or difficult it is for 
                                                             these transition aged youth to access each program depending if they have to travel far in order to even apply or if that can be done online within a couple of hours. These trees leads us into the question of where are the gaps 
                                                             in services and programs and which pillar are they under. ")
                                                    ),
                                                    column(8, 
                                                           h4(strong("Interactive Trees of Programs")),
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
                                                                      collapsibleTreeOutput("tree1",height = "550px") 
                                                                      
                                                                      
                                                                      
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
                                                                      collapsibleTreeOutput("tree3",height = "550px") 
                                                                      
                                                                      
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
                                                                      collapsibleTreeOutput("tree2",height = "550px")
                                                                      
                                                             )) 
                                                    )) ) ,
                                          
                                          tabPanel("Cross-County Comparison", 
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
                                                          
                                                           p("This combined tree is used to better compare Loudoun County with Fairfax and Allegheny County. As noted above,
                                                             the last node represents the county the program is accessible from when we can use to better visualize the gaps in which pillar in Loudoun County. Most of the programs 
                                                             in each county are specific to those who live in the county with the exception of Great Expectations, Workforce Innovation and Opportunity Act, Education and Training Vouchers and Medicaid. 
                                                             With this interactive tree, we can better see where Loudoun County is lacking versus where Allegheny county is sufficient in which led them to having a high success transition rate. Also, we can see that 
                                                             many of the programs available in Fairfax are also available to those in Loudoun and vice versa. Since the counties are located close, transition aged youth living in Loudoun may have easy access to those
                                                             programs and services in Fairfax depending on their location. ")
                                                           
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
                                                           collapsibleTreeOutput("compare", height = "500px") , 
                                                           br(),
                                                           br()
                                                           
                                                    ) 
                                                    
                                           )) 
                                     )
                                     ) 
                            ),
                            
                            
                            tabPanel("Locations", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Location of Programs and Services"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Where are these Services and Programs located?")),
                                                     p("The following interactive maps show the office locations of the services and programs available to 
                                                       TAYs in Loudoun county. Hovering over the circle markers will display the name of the service or program. 
                                                       Clicking on the circle markers will open a popup with a detailed description of the service, website link, and whether 
                                                       service delivery is online, in-person, or hybrid. "),
                                                     p("Each service/program is grouped by pillars (education, employment, health, housing, and transportation) and subpopulation
                                                       (foster care, juvenile delinquent, or generally to TAYs). The radio buttons at the top-right corner of the graph filter the
                                                       services or programs by subpopulation.  For comparison, we also include county maps for Fairfax, VA, and Alleghany, PA.  "), 
                                                     p("The Loudoun maps reveal a high concentration of offices in the eastern side of the county, nearer the DC Metropolitan area. 
                                                       This suggests that TAY that resides in the western region may have difficulty in accessing these in-person services/programs. 
                                                       Our maps, therefore, highlight the need for greater access to in-person services/programs to vulnerable TAYs in the 
                                                       non-eastern regions of the county. ")
                                                     
                                              ), 
                                              column(8, 
                                                     h4(strong("Locations of Services")),
                                                     column(4,
                                                            selectInput(
                                                              "county",
                                                              "Select County",
                                                              choices = c("Loudoun", "Fairfax", "Allegheny"), 
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
                            ), 
                            
                            tabPanel("Complete List", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Programs"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              DT::dataTableOutput("mytable")
                                                       
                                              
                                              
                                     )
                            )
                            
                            
                 ),
                 
                 
                 
                 ## Tab Utilization-------------------------------------------
                 navbarMenu("Utilization",
                            ### DMHSA------
                            tabPanel("MHSADS", value = "MHSADS", 
                                     fluidRow(style = "margin: 6,px;",
                                              h1(strong("Department of Mental Health, Substance Abuse, and Developmental Services"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              tabsetPanel(
                                                tabPanel("Overview", 
                                                         fluidRow(style = "margin: 6px;",
                                                                  p("", style = "padding-top:10px;"), 
                                                                  column(4, 
                                                                         h4(strong("Vulnerable Transition Aged Youth")),
                                                                         p("Our target population for this project is transition aged youths. This include those that are “aging out” of the foster care system or 
                                                                           leaving the juvenile detention system. However, there is another subpopulation of TAY that we examine – youths with mental health, 
                                                                           substance use, or developmental/intellectual disabilities.  "),
                                                                       p("We present data on the number of TAYs on the different program waitlists at Loudoun’s Department of Mental Health, Substance Abuse, 
                                                                         and Developmental Services (MHSADS). The programs provided for TAYs with disabilities or mental health issues include:  "),
                                                                       br(),
                                                                       tags$li("Case Management – services delivered for youths with intellectual and developmental disabilities"),
                                                                       tags$li("Employment and Day Support – assist youths with obtaining employment and volunteering"),
                                                                       tags$li("Outpatient – psychiatry and individual and group therapy"),
                                                                       tags$li("Residential – housing programs, including group homes. "),
                                                                       tags$li("Emergency – mental health evaluation, crisis intervention, and stabilization services"),
                                                                       br(), 
                                                                       p("The number of TAYs with severe mental illness served by the Department of Mental Health, Substance Abuse, 
                                                                         and Developmental Services has increased between 2016 and 2019. Moreover, the employment and day support waitlist 
                                                                         also increased from 2 in 2016 to 10 in 2019. The waitlist of residential service has been consistent over this period 
                                                                         moving from 23 to 20 TAYs. These trends highlight the increasing need for such services by TAYs. Note, we did not include 
                                                                         2020 in our analysis as the usage rate may be affected by the COVID pandemic. ")
                                                                  ), 
                                                                column(8,
                                                                       h4(strong("Waitlist of MHSADS by Program")), 
                                                                       plotlyOutput("waitlist"),
                                                                       br(), 
                                                                       p(tags$small("Data Source: Department of Mental Health, Substance Abuse and Developmental Services"))  ,  
                                                                       p(tags$small("*The Case Management waitlist does not include I/DD individuals waiting for Support Coordination as this is largely dependent on state-allotted waivers."))  ,  
                                                                       p(tags$small("**Since the start of the Same Day Access program in 2019, MHSADS has gotten rid of the Outpatient Services waitlist. ")))
                                              
                                     )), 
                                     tabPanel("Demographics", 
                                              fluidRow(style = "margin: 6px;",
                                                       p("", style = "padding-top:10px;"), 
                                                       column(4, 
                                                              h4(strong("Who does MHSADS Serve?")),
                                                              p("TAYs with disabilities or mental or substance issues face even more barriers as they move towards independence.  
                                                                         These youths experience an adult poverty rate three times higher than their peers.2 This high level of poverty,
                                                                         difficulty in accessing health services, and homelessness tend to have a very negative impact on the likelihood of 
                                                                         employment thereby, compounding the transition to self-sufficient adulthood. 3 Moreover, it is estimated that over 
                                                                         90% of youth incarcerated in Virginia have significant symptoms of a mental health disorder. Guided by this, we examine 
                                                                         mental health service utilization and gaps. ")
                                                            ), 
                                                       column(8, 
                                                              h4(strong("Demographics")), 
                                                              selectInput(
                                                                "dmhsaDemos",
                                                                "Select Demographic",
                                                                choices = list(
                                                                  "Race/Ethnicity" = "dmhsaRace", 
                                                                  "Gender" = "dmhsaGender", 
                                                                  "Severe Mental Illness" = "mental"
                                                                )
                                                                
                                                              ), 
                                                              plotlyOutput(outputId = "dmhsaPlot", height = "500px"),
                                                              p(tags$small("Data Source: Department of Mental Health, Substance Abuse and Developmental Services 2019"))
                                                              )
                                                       
                                              )), 
                                     tabPanel("Trends", 
                                              fluidRow(style = "margin: 6px;",
                                                       p("", style = "padding-top:10px;"), 
                                                       column(4, 
                                                             h4(strong("Individuals Served from 2016 – 2020")),
                                                             p("Visualizing the usage of services allows us to determine whether some services are used more than others. 
                                                               Loudoun County’s Department of Mental Health, Substance Abuse, and Developmental Services (DMHSA) provided us with data on 
                                                               the number of transition aged youth (TAY) served by zip code level. The dropdown box allows for the selection of the various programs. 
                                                               The slider at the right shows how the service number, represented by the orange dots, changes between 2016 and 2020. 
                                                               The orange dots are mapped onto the population density map by census tract of those who live in the area – this shows 
                                                               the utilization of each program by population density."),
                                                             p("TAYs seems to be consistently utilizing the emergency and outpatient services from the DMHSA. Notably, 
                                                               in comparison to the other services, the residential and employment program is underutilized regardless of location or year.  ")) ,
                                                      column(8, 
                                                             h4(strong("Individuals Served by Population Density")), 
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
                                                                         sep = "", 
                                                                         animate = animationOptions(interval = 1200))) , 
                                                             
                                                             
                                                             leafletOutput(outputId = "overtime", height = "70vh"), 
                                                             p(tags$small("Data source: Department of Mental Health, Substance Abuse, and Developmental Services 2019 "))
                                                             
                                                             
                                                      )
                                              ) 
                                     )
                                     )  
                            ))  ,
                            
                            ### Family Services served------------
                            tabPanel("Family Services", value = "family",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Department of Family Services"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              tabsetPanel(
                                                tabPanel("Overview", 
                                                         fluidRow(style = "margin: 6px;",
                                                                  p("", style = "padding-top:10px;"), 
                                                                  column(4, 
                                                                         h4(strong("Who does Family Services serve?")), 
                                                                         p("Family Services prodivded programs served over 100 transition aged youth from 2016 - 2020. As you can see, some programs are used more
                                                                           often by males and some by females while Asians are served the most with Public benefits like SNAP, Medicaid and TANF while the majority
                                                                           of those who used the Emergency Shelters and Transtional Housing Program were either White or Black. "), 
                                                                         p("")
                                                                         
                                                                  ), 
                                                                  column(8, 
                                                                         
                                                                         h4(strong("Demographics")), 
                                                                         selectInput(
                                                                           "familyDemo",
                                                                           "Select Demographic",
                                                                           choices = list(
                                                                             "Public Benefits Race" = "publicRace",
                                                                             "Public Benefits Gender" = "publicGender",
                                                                             "Emergency Shelter Gender" = "emerG",
                                                                             "Emergency Shelter Race" = "emerR",
                                                                             "Transitional Housing Gender" = "transG",
                                                                             "Transitional Housing Race" = "transR"
                                                                           )
                                                                           
                                                                         ), 
                                                                         plotlyOutput(outputId = "familyPlot", height = "500px") ,
                                                                         p(tags$small("Data Source: Family Services Data Warehouse 2015-2020 varying years"))
                                                                         
                                                                  )
                                                         )
                                                ),
                                                tabPanel("Trends", 
                                                         fluidRow(style = "margin: 6px;",
                                                                  p("", style = "padding-top:10px;"), 
                                                                  column(4, 
                                                                         h4(strong("Utilization Trends")),
                                                                         p("Visualizing trends of enrollment and usage for Family Service provided programs is essential in finding and filling gaps for vulnerable transition
                                                                           aged youth in Loudoun County. Based on data provided by Loudoun County, the animation to the right visualizes persons served for mutliple programs 
                                                                           from 2015 to 2020. ")
                                                                        ) ,
                                                                  column(8, 
                                                                         h4(strong("Enrollment by Program")), 
                                                                         tags$img(src="gganim.gif", controls = "controls", height = "800px", width = "900px") , 
                                                                         br(), 
                                                                         p(tags$small("Data source: Family Services Data Warehouse 2015-2020 varying years"))
                                                                         
                                                                         
                                                                  )
                                                         ) 
                                                )
                                                
                                              )
                                     ) 
                            ) ,
                            
                            ### Individual served------------
                            tabPanel("Other Programs", value = "served",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Individuals Served by Program"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              tabsetPanel(
                                                tabPanel("Overview", 
                                                         fluidRow(style = "margin: 6px;",
                                                                  p("", style = "padding-top:10px;"), 
                                                                  column(4, 
                                                                         h4(strong("What Other Programs are in Loudoun? ")), 
                                                                         infoBoxOutput("transit" , width = 4),
                                                                         p("Public transportation"),
                                                                         
                                                                         p("")), 
                                                                  
                                                                  column(8, 
                                                                         h4(strong(""))
                                                                         
                                                                         
                                                                  )
                                                                  )), 
                                                
                                                tabPanel("Demographics", 
                                                         fluidRow(style = "margin: 6px;",
                                                                  p("", style = "padding-top:10px;"), 
                                                                  column(4, 
                                                                         h4(strong("Who do Other Programs serve?")), 
                                                                         p("Race, Age, backgrounds of programs"), 
                                                                         p("")
                                                                         
                                                                  ), 
                                                                  column(8, 
                                                                         
                                                                         h4(strong("Graphs of Demographics")), 
                                                                         selectInput(
                                                                           "stat",
                                                                           "Select Program",
                                                                           choices = list(
                                                                             "Adult Literacy Programs" = "literacy",
                                                                             "Adult Literacy Program Race" = "literacyR",
                                                                             "OAR Enrollment"= "oar", 
                                                                             "OAR Demographics"= "oarD",
                                                                             "Loudoun Cares" = "cares", 
                                                                             "Oxford House Average Stay" = "oxford1",
                                                                             "Oxford House Prior" = "oxford2"
                                                                             
                                                                           )
                                                                           
                                                                         ), 
                                                                         plotlyOutput(outputId = "demographics", height = "500px"),
                                                                         p(tags$small("Data Source Vary based on Program"))
                                                                         
                                                                  )
                                                         )
                                                )
                                                
                                              )
                                     ) 
                            ) 
                            
                            
                            
                            
                            
                 ),
                 ## Tab Conclusion --------------------------------------------
                 tabPanel("Takeaway",  value = "conclusion", 
                          fluidRow(style = "margin: 6px;",
                                   h1(strong("Conclusion"), align = "center")),  
                          p("", style = "padding-top:10px;"), 
                          
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                           h2(strong("Provision")),
                                            tags$ul(
                                            tags$li("Beginning our project with a literature review and initial research, we knew the 5 pillars we wanted to focus our search on: Education, Employment, Housing, Transportation, and Health Services. 
                                            For vulnerable Transtion Aged Youth, ages 18-24, it is difficult for them to live independently especially those who have just aged out of the foster care system or gotten out of 
                                            juvenile detention. Once we started our webscrapping for the programs available in Loudoun County, we noticed that some of the programs available were difficult to access to gain information to
                                            and most of these programs, the person had to call or email for more information. After we webscrapped all of the programs in Loudoun County available for Transition Aged Youth and those specific to former foster care
                                            youths or juvenile delinquents, we started a cross-county analysis with Fairfax County, VA and Allegheny County, PA. We repeated the webscrapping process with both counties, searching for programs within the same 
                                            5 pillars for transition aged youth, those who have aged out of the foster care system and those who have gotten out of juvenile detention. To visualize the data we found, we created interactive trees shown
                                            in the Services, Availability tab and leaflet maps shown in Services, Locations. "), 
                                            tags$li("The interactive trees are a simple and easy way to display all of the programs and its most important information for each county seperately and together. We noticed gaps in transportation and housing programs in 
                                            Loudoun compared to Allegheny and Fairfax. The leaflet maps are used to visualize the locations of the services for each county and we noticed geographic gaps in services on the west side of the county. "), 
                                            tags$li("Third list item")
                                          )
                                        ),
                                   
                                   column(4,
                                          h2(strong("Utilization")),
                                          tags$ul(
                                            tags$li("Once we noticed the gaps in services in specific pillars like transportation and housing, we searched for more information regarding those who 
                                                    use these programs in Loudoun. If we can graph which demographics are using these services the most and the least, we may be able to see another gap in services regarding race, socieconomic status, sex, etc. 
                                                    "), 
                                            tags$li("Second list item"), 
                                            tags$li("Third list item")
                                          )
                                   ),
                                   column(4,
                                          h2(strong("Gaps")),
                                          tags$ul(
                                            tags$li("As noted above, once we mapped and visualized the programs' locations and descriptions, it was apparent which pillars Loudoun was lacking in and had an abundance in. 
                                                    One of the most important gaps that needs to be addressed is the gap in the Transportation pillar. We can infer that most transition aged youth do not have a vehicle of their own and those who have aged out
                                                    of the foster care system or juvenile detention are even more unlikely to own a vehicle because of the lack of family and friends support. This means that in order to get to one of the programs for employment or education, they would have
                                                    to have some form of transportation other than a car, public transportation. However, in Loudoun county, the only form of public transportation that was free was one bus route, Safe-T that has 10 stops, most of 
                                                    them in 3 shopping centers in Leesburg. Because there is no other transportation service that a transition aged youth could qualify for, they are likely not going to be able to get to other programs for employment or education. "), 
                                            tags$li("Second list item"), 
                                            tags$li("Third list item")
                                          )
                                   )

                        )
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
                                          img(src = "team-kyle.jpg", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = 'https://www.linkedin.com/in/yang-cheng-200118191/', 'Yang Cheng', target = '_blank'), "(Virginia Tech, Graduate in Agricultural and Applied Microeconomics);"),
                                          p(a(href = 'https://www.linkedin.com/in/jaida-robinson-92856b194/', 'JaiDa Robinson', target = '_blank'), "(Virginia State University, Graduate in Counselor Education );"),
                                          p(a(href = 'https://www.linkedin.com/in/julie-rebstock', 'Julie Rebstock', target = '_blank'), "(Virgina Tech, Undergraduate in Economics and Computational Modeling and Data Analytics);"),
                                          p(a(href = 'https://www.linkedin.com/in/austin-burcham-9b32a81ab/', 'Austin Burcham', target = '_blank'), "(Virginia Tech, Undergradutate Computational Modeling and Data Analytics)."),
                                          p(a(href = 'https://www.linkedin.com/in/kyle-jacobs7/', 'Kyle Jacobs', target = '_blank'), "(Virginia State Univeristy, Undergradutate in Agriculture)."),
                                          p("", style = "padding-top:10px;") 
                                   ),
                                   column(6, align = "center",
                                          h4(strong("VT Faculty Team Members")),
                                          img(src = "faculty-chanita.png", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "faculty-isabel.jpg", style = "display: inline; margin-right: 5px; border: 1px solid #C0C0C0;", width = "150px"),
                                          img(src = "", style = "display: inline; border: 1px solid #C0C0C0;", width = "150px"),
                                          p(a(href = "", 'Chanita Holmes', target = '_blank'), "(Project Lead, VT Research Assistant Professor);") , 
                                          p(a(href = "", 'Isabel Bradburn', target = '_blank'), "(Research Faulty, Department of Human Development and Family Science, VT)."),
                                          p("", style = "padding-top:10px;")
                                   )) ,
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("Project Stakeholders")),
                                   p(a(href = 'https://loudoun.ext.vt.edu/staff/Vermaak-Stuart.html', 'Stuart Vermaak', target = '_blank'), "(Virginia Cooperative Extension, Loudoun County at Virginia Tech);"),
                                   p(a(href = 'https://loudoun.ext.vt.edu/staff/Hilleary-James.html', 'James Hilleary', target = '_blank'), "(Virginia Cooperative Extension, Loudoun County at Virginia Tech)."),
                                   p("", style = "padding-top:10px;"),
                                   h4(strong("Acknowledgments")) ,
                                   p("We would like to thank Loudoun officials for providing us with data for our project. Specifically, the Department of Mental Health, Substance Abuse, and Developmental Services and the Family Service Department.  ")
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
      
      p <- ggplot(l_ages_gender, aes(x = estimate, y = variable, fill = gender)) +  scale_fill_viridis_d() + 
        geom_bar(position="dodge", stat="identity") + theme_minimal() + 
        labs(title = "Gender and Age Groups ", 
             y = "", 
             x = "Population Estimate") +coord_flip()+
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "x")
      
      
    # } else if(var1() == "percent") {
    #   
    #   m_p <- sum(l_ages_gender$estimate[1:4])/205374 * 100 
    #   f_p <-sum(l_ages_gender$estimate[5:8])/208164 * 100 
    #   
    #   percent <- data.frame(rbind(m_p, f_p))
    #   colnames(percent) <- "Percent"
    #   percent$Gender <- c("Male", "Female")
    #   
    #   
    #   percent %>%
    #     ggplot() + geom_col(mapping = aes(x = Percent, y = Gender , fill = Gender))+ scale_fill_viridis_d() + 
    #     labs(title = "Percent of TAYs by Gender", 
    #          y = "", 
    #          x = "Percent %") + coord_flip()  + theme_minimal() + 
    #     theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
    }else if(var1() == "race"){
      
      p <- ace %>%
        ggplot() + geom_col(mapping = aes(x = Estimate, y = Race , fill = Race))+  scale_fill_viridis_d() + 
        labs(title = "Race by Number", 
             y = "", 
             x = "Population Estimate")  + theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "x")
      
    } else if (var1() == "raceP"){
      
      p <- race %>% 
        ggplot(aes(x = Race, y = Percent, fill = Race)) + 
        geom_col() +scale_fill_viridis_d()+
        labs(title = "Race by Percent",
             y = "Percent (%)",
             x="") + coord_flip()+ theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "x")
      
    }else if (var1() == "ethP"){
      
      p <- eth_totals %>%
        ggplot() + geom_col(mapping = aes(x = variables, y = percent, fill = variables ))+ scale_fill_viridis_d()+ 
        labs(title = "Ethnicity By Gender", 
             y = "Percent (%)", 
             x = "") + theme_minimal() + theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      ggplotly(p, tooltip = "y")
      
      
    }else if (var1() == "home"){
      
      p <- ggplot() + geom_col(mapping = aes(Year, TAY, fill = TAY), data = homeless) + 
        labs(title = "Homeless TAY in Loudoun ",
             y = "TAY",
             x = "")+ scale_fill_viridis_c()+ theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "y")
      
      
      
    }else if (var1() == "education")  { 
      
      p <- education %>%
        ggplot(mapping = aes(x = sum, y = variable , fill = variable)) + geom_col()+ scale_fill_viridis_d()+ 
        labs(title = "Educational Attainment", 
             y = "", 
             x = "Population Estimate")  + theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "x")
      
      
    }
    else if (var1() == "health"){
      p <- healthcare %>%
        ggplot(mapping = aes(x = Estimate, y = Type , fill = Type))  + geom_col()+  scale_fill_viridis_d() + 
        labs(title = "Types of Healthcare Coverage", 
             y = "", 
             x = "Population Estimate") + 
        coord_flip() + theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "x")
      
      
    }else {
      p <- pov <- rbind(w_p, b_p, i_p, as_p, n_p, o_p)
      pov$variable <- factor(pov$variable, levels=unique(pov$variable))
      
      pov %>%
        ggplot() + geom_col(mapping = aes(x = Percent, y = variable, fill = variable))+  scale_fill_viridis_d() + 
        labs(title = "Poverty Level by Race", 
             y = "", 
             x = "Percent (%)") + 
        theme_minimal() + 
        theme(legend.position = "none",  
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      ggplotly(p, tooltip = "x")
      
    }
    
  })  
  
  
  var2 <- reactive({
    input$var2
  })
  ##Render Plot for Foster Care 
  output$plot2 <- renderPlotly({
    if(var2() == "age") {
      fc_ages$Age.Group <- factor(fc_ages$Age.Group, levels=unique(fc_ages$Age.Group))
      fc_ages <- fc_ages %>% rename(`Population Estimate` = Value)
      
      
      p <- ggplot(data = fc_ages, aes(x= Age.Group, y = `Population Estimate`, fill = Age.Group) ) + 
        geom_col() + coord_flip()+ scale_fill_viridis_d() + theme_minimal() + 
        labs(x = "", 
             y = "Population Estimate",
             title = "Age Groups for Foster Care", 
             fill ="") + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
      
      q <- ggplotly(p, tooltip = "y")
      q
    }else if(var2() == "race"){
      fc_races <- fc_races %>% rename(`Population Estimate` = Value)
      p <- ggplot(data = fc_races, aes(x= Race, y = `Population Estimate`, fill = Race) ) + 
        geom_col() + coord_flip()+scale_fill_viridis_d() + theme_minimal() + 
        labs(x = "", 
             y = "Population Estimate",
             title = "Racial Demographics for Foster Care", 
             fill ="") + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      q <- ggplotly(p, tooltip = "y")
      q
    } else if (var2() == "eth") {
      fc_eth <- fc_eth %>% rename(`Population Estimate` = value)
      g <- ggplot(fc_eth, aes(eth, `Population Estimate`, fill = eth)) + geom_col() + 
        scale_fill_viridis_d() + theme_minimal() + 
        labs(title = "Ethinic Demographics of Foster children",
             x = "", 
             y = "Population Estimate") + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      q <- ggplotly(g, tooltip = "y")
      q
    }else {
      fc_sex <- fc_sex %>% rename(`Population Estimate` = Value)
      p <- ggplot(fc_sex, aes(x = Gender, y = `Population Estimate`, fill = Gender)) + 
        geom_bar(stat="identity") + scale_fill_viridis_d() + theme_minimal() + 
        labs(x = "" , y = "Population Estimate", 
             title = "Sex of Youths in Foster Care") + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      q <- ggplotly(p, tooltip = "y")
      q
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
        mutate(Age = recode(Age, `12-Aug` = "8-12" )) %>% 
        mutate(`Relative Frequency` = `Relative Frequency`/ 100) %>% 
        filter(Age != "Missing")
      
      
      p <- jv_age %>% 
        ggplot(aes(x = Age, y = `Relative Frequency`, fill = Age)) +
        geom_col() +scale_fill_viridis_d() + 
        labs(x = "", y = "Relative Frequency",
             title = "Age Groups of Juvenile Intakes") + 
        theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        coord_flip() 
      q <- ggplotly(p, tooltip = "y")
      q
    }
    else if(var3() == "race"){
      jv_race$Race <- factor(jv_race$Race, levels=unique(jv_race$Race))
      jv_race <- jv_race %>% mutate(`Relative Frequency` = Proportion) %>% 
        mutate(`Relative Frequency` = `Relative Frequency`/100)
      
      p <- jv_race %>% 
        ggplot(aes(x = Race, y = `Relative Frequency`, fill =Race)) +
        geom_col() +
        labs(x = "", y = "Relative Frequency",
             title = "Racial Demographics of Juvenile Intakes") + 
        theme_minimal() + scale_fill_viridis_d() +
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        coord_flip() 
      q <- ggplotly(p, tooltip = "y")
    }
    else if (var3() == "eth") {
      jv_eth$Ethnicity <- factor(jv_eth$Ethnicity, levels=unique(jv_eth$Ethnicity))
      jv_eth <- jv_eth %>% mutate(`Relative Frequency` = Proportion)%>% 
        mutate(`Relative Frequency` = `Relative Frequency`/100)
      
      
      p <- jv_eth %>% 
        ggplot(aes(x = Ethnicity, y = `Relative Frequency`, fill = Ethnicity))  +
        geom_col() +
        labs(x = "", y = "Relative Frequency",
             title = "Ethnicity of Juvenile Intakes") + 
        theme_minimal() + scale_fill_viridis_d() +
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        coord_flip()
      q <- ggplotly(p, tooltip = "y")
      
    }
    else{
      jv_sex$Sex <- factor(jv_sex$Sex, levels=unique(jv_sex$Sex))
      jv_sex <- jv_sex %>% mutate(`Relative Frequency` = Proportion)
      
      
      p <- jv_sex %>% 
        ggplot(aes(x = Sex, y = `Relative Frequency`, fill = Sex)) +
        geom_col() +
        labs(x = "", y = "Relative Frequency",
             title = "Sex Demographics of Juvenile Intakes") + 
        theme_minimal() + scale_fill_viridis_d() +
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        coord_flip()
      
      q <- ggplotly(p, tooltip = "y")
      
      
    }
    
  }) 
  
  #Waitlist for programs in Loudoun 
  output$waitlist <- renderPlotly({
    
    ggplot(waitlist, aes(x=Year)) + 
      geom_line(aes(y = Persons, group = Program, color = Program))+
      labs(y = "Total Number of TAY")
    
  })
  
  
  ## Trees -----------------------------------------------------------
  output$tree1 <- renderCollapsibleTree({
    if(input$pillar1%in%"Education"){
      Tree%>%filter(County == "Loudoun")%>%
        filter(Pillars == "Education")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Education",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
    }else if(input$pillar1%in%"Employment"){
      Tree%>%filter(County == "Loudoun")%>%
        filter(Pillars == "Employment")%>%
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Employment",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar1%in%"Housing"){
      Tree%>%filter(County == "Loudoun")%>%
        filter(Pillars == "Housing")%>%
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Housing",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar1%in%"Transportation"){
      Tree%>%filter(County == "Loudoun")%>%
        filter(Pillars == "Transportation")%>%
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Transportation",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else {
      Tree%>%filter(County == "Loudoun")%>%
        filter(Pillars == "Health Services")%>%
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Health Services",
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
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Education",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
    }else if(input$pillar2%in%"Employment"){
      Tree%>%filter(County == "Allegheny")%>%
        filter(Pillars == "Employment")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Employment",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar2%in%"Housing"){
      Tree%>%filter(County == "Allegheny")%>%
        filter(Pillars == "Housing")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Housing",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar2%in%"Transportation"){
      Tree%>%filter(County == "Allegheny")%>%
        filter(Pillars == "Transportation")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Transportation",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else{
      Tree%>%filter(County == "Allegheny")%>%
        filter(Pillars == "Health Services")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Health Services",
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
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Education",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
    }else if(input$pillar3%in%"Employment"){
      Tree%>%filter(County == "Fairfax")%>%
        filter(Pillars == "Employment")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Employment",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar3%in%"Housing"){
      Tree%>%filter(County == "Fairfax")%>%
        filter(Pillars == "Housing")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Housing",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$pillar3%in%"Transportation"){
      Tree%>%filter(County == "Fairfax")%>%
        filter(Pillars == "Transportation")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Transportation",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else{
      Tree%>%filter(County == "Fairfax")%>%
        filter(Pillars == "Health Services")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "Age_range", "Delivery"),
                        root="Health Services",
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
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "County"),
                        root="Education",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
    }else if(input$compare1%in%"Employment"){
      Tree%>%
        filter(Pillars == "Employment")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "County"),
                        root="Employment",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$compare1%in%"Housing"){
      Tree%>%
        filter(Pillars == "Housing")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "County"),
                        root="Housing",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else if(input$compare1%in%"Transportation"){
      Tree%>%
        filter(Pillars == "Transportation")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "County"),
                        root="Transportation",
                        attribute = "Pillars",
                        width=1800,
                        zoomable=F, 
                        collapsed = T, nodeSize = 'leafCount',
                        fillByLevel = T)
      
    }else{
      Tree%>%
        filter(Pillars == "Health Services")%>% 
        group_by(Pillars)%>%
        collapsibleTree(hierarchy = c("Subpopulation", "Program", "County"),
                        root="Health Services",
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
                loudoun_locations$Address,
                "<br />",
                "<a href = ",loudoun_locations$Website, "> Website </a>",
                "<br />", 
                "<strong>Delivery:</strong>",
                loudoun_locations$Delivery), 
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
                loudoun_locations$Address,
                "<br />",
                "<a href = ",loudoun_locations$Website, "> Website </a>",
                "<br />", 
                "<strong>Delivery:</strong>",
                loudoun_locations$Delivery), 
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
                fairfax$Address,
                "<br />",
                "<a href = ",fairfax$Website, "> Website </a>",
                "<br />", 
                "<strong>Delivery:</strong>",
                fairfax$Delivery), 
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
                fairfax$Address,
                "<br />",
                "<a href = ",fairfax$Website, "> Website </a>",
                "<br />", 
                "<strong>Delivery:</strong>",
                fairfax$Delivery), 
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
                allegheny_locations$Address,
                "<br />",
                "<a href = ",allegheny_locations$Website, "> Website </a>",
                "<br />", 
                "<strong>Delivery:</strong>",
                allegheny_locations$Delivery),
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
                allegheny_locations$Address,
                "<br />",
                "<a href = ",allegheny_locations$Website, "> Website </a>",
                "<br />", 
                "<strong>Delivery:</strong>",
                allegheny_locations$Delivery), 
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
        leaflet(width = "100%") %>%
        addProviderTiles("CartoDB") %>% 
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.6,
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
        leaflet(width = "100%") %>%
        addProviderTiles("CartoDB") %>% 
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.6,
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
        leaflet(width = "100%") %>%
        addProviderTiles("CartoDB") %>% 
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.6,
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
        leaflet(width = "100%") %>%
        addProviderTiles("CartoDB") %>% 
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.6,
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
        leaflet(width = "100%") %>%
        addProviderTiles("CartoDB") %>% 
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.6,
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
        leaflet(width = "100%") %>%
        addProviderTiles("CartoDB") %>% 
        addPolygons(popup = ~ str_extract(NAME, "^([^,]*)"),
                    stroke = FALSE,
                    smoothFactor = 0,
                    fillOpacity = 0.6,
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
  
  
  familyType <- reactive({
    input$familyType
  })
  
  
  output$trendsF <- renderPlot({
    
    
    
  })
  
  ## count -----------------------------------------------------------
  
  output$table1 <- renderTable({
    programs <- read_excel("data/combined-programs.xlsx")
    subpop_count <- programs %>% 
      group_by(County) %>% 
      count(Subpopulation) %>% 
      pivot_wider(names_from = County, values_from = n) %>% 
      relocate(Loudoun, .after = Subpopulation)
    subpop_count
    
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  output$table2 <- renderTable({
    programs <- read_excel("data/combined-programs.xlsx")
    pillar_count <- programs %>% 
      group_by(County) %>% 
      count(Pillars) %>% 
      pivot_wider(names_from = County, values_from = n) %>% 
      relocate(Loudoun, .after = Pillars)
    pillar_count
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  
  ## infoBox -----------------------------------------------------------
  output$transit <- renderInfoBox({
    
    infoBox(title = "Route 54 Safe-T", value = "52, 57, 42", 
            subtitle = "Average Riders Weekdays, Saturday, Sunday",
            icon = icon("bus"),
            color = "maroon",
            fill = T)
  })
  
  
  
  dmhsaDemos <- reactive({
    input$dmhsaDemos
  })
  ##graph for demographics of individuals served Other Programs
  output$dmhsaPlot <- renderPlotly({
    if(dmhsaDemos() == "dmhsaRace") {
      
      p <- mhs_race_relative %>% 
        ggplot(aes(group = `Race.Ethnicity`, x = Year, y = `Percent`,
                   fill = `Race.Ethnicity`)) + 
        geom_col() +
        scale_y_continuous(labels = scales::percent) +
        labs(title = "Race of TAY Served") +
        scale_fill_viridis_d() +
        theme_minimal()+ theme(legend.position="bottom")
      
      ggplotly(p, tooltip = "y")
      
    }else if (dmhsaDemos() == "dmhsaGender"){
      
      p <-mhs_sex_relative %>% 
        ggplot(aes(group = Gender, x = Year, y = `Percent`,
                   fill = Gender)) + 
        geom_col() +theme_minimal() + 
        scale_y_continuous(labels = scales::percent) +
        labs(title = "Gender of TAY Served") +
        scale_fill_viridis_d() +
        theme_minimal()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "y")
      

      
      
    }else {
      
      p <- ggplot(smi, aes(x=Year)) + 
        geom_line(aes(y = Persons, group = Group, color = Group)) + theme_minimal() + 
        labs(title = "Severe Mental Illness, 5 Year Count")+scale_fill_viridis_d() +
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "y")

    }
    
    
  })
  
  
  familyDemo <- reactive({
    input$familyDemo
  })
  ##graph for demographics of individuals served Other Programs
  output$familyPlot <- renderPlotly({
    if(familyDemo() == "publicRace") {
      
      publicR$Race <- factor(publicR$Race, levels=unique(publicR$Race))
      publicR$Number <- as.numeric(publicR$Number)
      
      
      p <- ggplot() + geom_col(mapping = aes(Race, Number, fill = Race), data =publicR) + 
        labs(title = "Public Benefits Race/Ethnicity",
             y = "") +scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
      
      ggplotly(p, tooltip = "y")
      
    }else if (familyDemo() == "publicGender"){
      
      publicG$Gender <- factor(publicG$Gender, levels=unique(publicG$Gender))
      
      p <- ggplot() + geom_col(mapping = aes(Gender, Number, fill = Gender), data =publicG) + 
        labs(title = "Public Benefits Gender",
             y = "") +scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "y")
      
      
    }else if (familyDemo() == "emerG"){
      
      emerG$Gender <- factor(emerG$Gender, levels=unique(emerG$Gender))
      
      p <- ggplot() + geom_col(mapping = aes(Gender, Number, fill = Gender), data =emerG) + 
        labs(title = "Emergency Shelters Gender",
             y = "") +scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "y")
      
      
    }else if (familyDemo() == "transG"){
      
      transG$Gender <- factor(transG$Gender, levels=unique(transG$Gender))
      
      p <- ggplot() + geom_col(mapping = aes(Gender, Number, fill = Gender), data =transG) + 
        labs(title = "Transitional Housing Gender",
             y = "") +scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "y")
      
      
    }else if (familyDemo() == "transR"){
      
      emerR$Race <- factor(emerR$Race, levels=unique(emerR$Race))
      
      p <- ggplot() + geom_col(mapping = aes(Race, Number, fill = Race), data =emerR) + 
        labs(title = "Transitional Housing Race/Ethnicity",
             y = "") +scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "y")
      
      
      }else {
      
        emerR$Race <- factor(emerR$Race, levels=unique(emerR$Race))
        
        p <- ggplot() + geom_col(mapping = aes(Race, Number, fill = Race), data =emerR) + 
          labs(title = "Emergency Shelters Race/Ethnicity",
               y = "") +scale_fill_viridis_d() +theme_minimal() + 
          theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
                legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
        ggplotly(p, tooltip = "y")
    }
    
    
  })

  
  stat <- reactive({
    input$stat
  })
  ##graph for demographics of individuals served Other Programs
  output$demographics <- renderPlotly({
    if(stat() == "literacyR") {
      
      plot <- ggplot() + geom_col(mapping = aes(Race, Percent, fill = Race), data = literacy_demo) + 
        labs(title = "Adult Literacy Program",
             y = "Total TAYs and Adult Residents served in Loudoun")+scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(plot, tooltip = "y") %>% layout(
        annotations = 
          list(x = 1,  y = -.18, text = "Data Source: Loudoun County Public School 2019", 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10))) 
      
    }else if (stat() == "oxford1"){

      plot <- ggplot() + geom_col(mapping = aes(Category, Number, fill = Category), data = ox[1:2,]) +
        labs(title = "Oxford Houses in VA ",
             y = "Months") +scale_fill_viridis_d() +theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())

      ggplotly(plot, tooltip = "y") %>% layout(
        annotations =
          list(x = 1,  y = -.3, text = "Data Source: Oxford Annual Report 2019-2020",
               showarrow = F, xref='paper', yref='paper',
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10)))


    }else if (stat() == "oxford2"){

      plot <- ggplot() + geom_col(mapping = aes(Category, Number, fill = Category), data = ox[3:5,]) +
        labs(title = "Oxford Houses in VA",
             y = "%/Years") +scale_fill_viridis_d() +theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())

      ggplotly(plot, tooltip = "y") %>% layout(
        annotations =
          list(x = 1,  y = -.4, text = "Data Source: Oxford Annual Report 2019-2020",
               showarrow = F, xref='paper', yref='paper',
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10)))
      
    }else if (stat() == "oar") {
      
      
      plot <- ggplot() + geom_col(mapping = aes(Category, Number, fill = Category), data = all[1:2,]) + 
        labs(title = "Number of Individuals in the OAR program",
             y = "Total TAYs and Adult Residents served in Loudoun")+scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),legend.position = "none",
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(plot, tooltip = "y") %>% layout(
        annotations = 
          list(x = 1,  y = -.2, text = "Source: OAR Annual Report 2019-2020", 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10))) 
      
      
      }else if (stat() == "oarD"){
        
        plot <- ggplot() + geom_col(mapping = aes(Category, Number, fill = Category), data = all[3:7,]) + 
          labs(title = "Demographics of OAR program",
               y = "Total TAYs and Adult Residents served in Loudoun") +scale_fill_viridis_d() +theme_minimal() + 
          theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
                legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
        ggplotly(plot, tooltip = "y") %>% layout(
          annotations = 
            list(x = 1,  y = -.43, text = "Data Source: OAR Annual Report 2019-2020", 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=10))) 
        
        
      }else if (stat() == "cares"){
      
        plot <- ggplot() + geom_col(mapping = aes(Name, Stat, fill = Name), data = cares) + 
          labs(title = "Statistics of Loudoun Cares Program in 2020",
               y = "Total Served/Percent Served",
                x= "") +scale_fill_viridis_d() +theme_minimal() + 
          theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
                legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
        ggplotly(plot, tooltip = "y") %>% layout(
          annotations = 
            list(x = 1,  y = -.6, text = "Data Source: Loudoun Cares Annual Report 2020", 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=10))) 
      
      
    }else {
      
      plot <- ggplot() + geom_col(mapping = aes(Type, Number, fill = Type), data = literacy) + 
        labs(title = "Adult Literacy Program", 
             y = "Total TAYs and Adult Residents served in Loudoun")+scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"), legend.position = "none", 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(plot, tooltip = "y") %>% layout(
        annotations = 
          list(x = 1, y = -.2, text = "Source: Loudoun County Public School 2019-2020", 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size= 9))) 
    }
    
    
  }) 
  
  output$mytable = DT::renderDataTable({
    datatable(list, filter = 'top')
  })

  
  
  
  
  
  
}


# Run the application-----------
shinyApp(ui = ui, server = server)
