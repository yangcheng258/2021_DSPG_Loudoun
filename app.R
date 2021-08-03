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
library(janitor)
options(tigris_use_cache = TRUE)


# READING DATA-----------------------------------------------------------
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
smiwaitlist <- read_excel(paste0(getwd(),"/data/smi-waitlist.xlsx")) 
smi <- smiwaitlist[11:20,1:3]
colnames(smi) <- c("Group", "Year", "Persons")
smi$Persons <- as.numeric(smi$Persons)

waitlist <- smiwaitlist[6:25,9:11]
colnames(waitlist) <- c("Program", "Year", "Persons")

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
  filter(County == "Allegheny, PA") %>% 
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
# va_zips <- readRDS(paste0(getwd(),"/data/va_zips.rds")) 
# va_zips <- st_transform(va_zips, '+proj=longlat +datum=WGS84')
# loudoun_zip_link <- "http://ciclt.net/sn/clt/capitolimpact/gw_ziplist.aspx?ClientCode=capitolimpact&State=va&StName=Virginia&StFIPS=51&FIPS=51107"
# loudoun_zip_codes <- read_html(loudoun_zip_link) %>% html_node("td table") %>%  
#   html_table(fill = T) %>% dplyr::select(c(1,2)) %>% dplyr::rename(`Zip Code` = X1, City = X2) %>%
#   slice(-c(1, 2)) 
# loudoun_zip_code_city_names <- loudoun_zip_codes %>% pull(City)
# loudoun_zip_codes <- pull(loudoun_zip_codes, `Zip Code`)
# loudoun_zips <- va_zips %>% filter(ZCTA5CE10 %in% loudoun_zip_codes)


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

# TRansit
transit <- loudoun[1:3, 25:26]
colnames(transit) <- c("Day", "Average") 
transit$Day <- factor(transit$Day, levels=unique(transit$Day))




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
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/events/data%20science%20for%20the%20public%20good%202021/%20symposium%202021%20poster%20sessions\">' +
                              '<img src=\"DSPG_black-01.png\", alt=\"DSPG 2021 Symposium Proceedings\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('dspg');
           } else {
             x[0].innerHTML = '<div style=\"margin-top:-14px\"><a href=\"https://datascienceforthepublicgood.org/economic-mobility/community-insights/case-studies\">' +
                              '<img src=\"AEMLogoGatesColorsBlack-11.png\", alt=\"Gates Economic Mobility Case Studies\", style=\"height:42px;\">' +
                              '</a></div>';

             //changeLinks('economic'); 
           }
           "




# THIS IS THE BEGINNING OF UI -----------------------------------------------------------
ui <- navbarPage(title = "Loudoun",
                 selected = "overview",
                 theme = shinytheme("lumen"),
                 tags$head(tags$style('.selectize-dropdown {z-index: 10000}')), 
                 useShinyjs(),
                 
                 
                 
                 ## Tab Overview--------------------------------------------
                 tabPanel("Overview", value = "overview",
                          fluidRow(style = "margin: 6px;",
                                   align = "center",
                                    br(""),
                                   h1(strong("Service Provision for Vulnerable Transition Aged Youth in Loudoun County"),
                                      br(""),
                                      h4("Data Science for the Public Good Program"),
                                      h4("Virginia Tech"),
                                      br()
                                   )
                          ),
                          fluidRow(style = "margin: 6px;",
                                   column(4,
                                          h2(strong("Background")),
                                          p(a(href = "https://en.wikipedia.org/wiki/Transitional_age_youth#:~:text=Transitional%20age%20youth%20(TAY)%20are,environments%20and%20are%20at%2Drisk.",strong("Transition Aged Youth (TAY): "),target="_blank"), "Transition Aged Youth are young adults aged 18 -24 years old.  Transitioning into adulthood, all TAY can encounter numerous challenges in becoming economically independent. However, the transition can be especially difficult for youth formerly involved in the foster care system or those leaving juvenile detention facilities. A major challenge for many as they age out of these systems is a continued need for basic services with little to no guidance about how to access them. The combination of a disruption in services and for many, a lack of family support put them at high risk.  Our project targets Transition Aged Youth (TAY) with particular attention to those formerly involved in foster care system or those leaving juvenile detention facilities. "),
                                          p(),
                                          p(a(href = "https://www.loudoun.gov", strong("Loudoun County"), target = "_blank"), 
                                            "is located in the northern part of the Commonwealth of Virginia in the United States. Loudoun County is part of the ",a(href="https://www.loudoun.gov/DocumentCenter/View/57003/Map---Washington-DC-Metropolitan-Area","Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area.",target="_blank")," It covers 515.8 square miles ranking 20th-largest county in Virginia by area. Loudoun has a large population. In 2019, the population was estimated at 413,538, making it Virginia’s third-most populous county. Our target population TAY, ages 18-24, makes up about 5% of the total population. Loudoun is one of the richest counties in the US. In 2019, the county's median household income is $151,800, which is more than double the amount in the U.S. ($65,712); in addition, the per capita income is $58,522, while the average amount in the U.S. is $35,672.  The poverty rate of Loudoun County is 3.1% in 2019, which is about one-third of the poverty rate in the US."), 
                                        
                                          p(),
                                          p(a(href = "https://www.loudoun.gov/DocumentCenter/View/152440/Human-Services-Strategic-Plan", strong("The Loudoun County Human Services Strategic Plan 2019-2024 "), target = "_blank"),
                                            "has a vision of creating a healthy, thriving, safe, and inclusive Loudoun community. The first goal, to close critical service gaps for vulnerable or underserved members of the Loudoun community, motivated this project."),

                                          p()
                                          ),
                                   column(4,
                                         h2(strong("Our Work")),
                                          p(strong("The goal: "),"Motivated by the Loudoun County Human Services Strategic Plan, this Data Science for the Public Good project aims to map and describe service provision for TAY and vulnerable TAY in five major pillars critical to achieving or maintaining independence as well as well-being: education, employment, housing, transportation, and healthcare. A thorough services summary can help identify aeras of strength and where gaps may exist. "),
                                         p(),
                                         img(src = "pillars.png", style = "display: inline; margin-left: auto; margin-right: auto;", width = "80%"),
                                         
                                          p(strong("Our work:")),
                                          tags$ul(
                                          tags$li("We applied ",strong("demographics analysis "),"to develop an understanding of the age, sex, and racial composition, poverty rates and health of TAY in Loudoun County by using American Community Survey data. In addition, we took a close look at the TAY aging out of the foster care system and leaving juvenile detention facilities in Loudoun County. "),
                                          tags$li("We ",strong("identified available services "),"for TAY in Loudoun County ( VA) ",a(href="https://www.fairfaxcounty.gov/","Fairfax County (VA)",target="_blank"), "and",a(href="https://www.alleghenycounty.us/","Allegheny County (PA).",target="_blank"), "At the same time, we collected qualification requirements, application methods, location information, delivery formats and other useful information to facilitate access of services for the targeted population. We classified services into 5 pillars as shown above. "),
                                          tags$li("We conducted ",strong("cross-county analysis")," between Loudoun County, VA and Fairfax County, VA and between Loudoun County, VA and Allegheny County, PA to detect and provide insight into service gaps across similar jurisdictions. "),
                                          tags$li("We displayed services in collapsible tree diagrams and geospatial maps that allow users to explore comparisons. The ",strong("interactive tools")," examined both intra-county and inter-state variation in service provision in a manner designed to be meaningful and user centric. "),
                                          tags$li("Where possible, we reported TAY service utilization in Loudoun over time in and by user characteristics.   "),
                                          p(),
                                          )
                                         
                                   ),
                                   column(4,
                                          h2(strong("Dashboard Audience")),
                                          p("This dashboard compiles our findings and allows extension professionals, stakeholders, and other users to explore the information interactively."),
                                          p(strong("Transition Aged Youth. "), "The interactive tools, maps and collapsible tress, in this dashboard supply rich information of services such as location, qualifications, delivery formats, and description of available services in Loudoun County, Fairfax County, and Allegheny County in five major areas: education, employment, housing, transportation, and health care. TAY looking for services can use the dashboard to research programs, eligibility criteria and where to locate them. The maps have attached physical addressees of service providers’ offices and hyperlinks of official website of each program or service.  "),
                                          p(strong("Loudoun County Government Agencies for Human Services."), "Use of the dashboard to identify potential service gaps can inform implementation of the Strategic Plan to achieve outlined goals of providing necessary supports to underserved youth during the critical transition to adulthood. In addition, the service utilization analysis provides an important index of TAY program uptake, another potential gap – programs may be well-targeted to fill a need, but if they are underutilized, will not help youth as envisioned.  "),
                                          p(strong("Extension professionals and academic researchers.")," The transitional phase of emerging adulthood is of developmental importance to individuals and their economic independence is critical to the Commonwealth. The services can offer vulnerable TAY opportunities to live independently. However, the mismatch of service demand and service supply, or the gaps, could cause problems for the human services systems. The reasons for the gaps could be worth investigation and the finding in this dashboard will be helpful.")
                                         
                                   )
                          ),
                          fluidRow(align = "center",
                                   p(tags$small(em('Last updated: August 2021'))),
                                   p("", style = "padding-top:10px;")
                                   
                                   
                          ) 
                          
                 ),
                 
                 ## Tab Demographics-------------------------------------------
                 navbarMenu("Sociodemographics",
                            ### TAY--------------------------------------------
                            tabPanel("Target Population",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Transition Aged Youth's Sociodemographic Characteristics"), align = "center"),
                                              p("", style = "padding-top:10px;"),
                                              column(4, 
                                                     h4(strong("Who does Loudoun County Serve?")),
                                                     p("The 2019 American Community Survey (ACS) estimates that transition-aged youths (ages 18-24) make up 5% of the population in Loudoun county.  
                                                       Of these youths, are living below the poverty line, and only percent attaining high school degree.  
                                                       The number of homeless transition-age youth (TAY) has increased in Loudoun, as 18 more individuals were 
                                                       counted as homeless in 2020 than in 2016. This represents a 450% increase.  "), 
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
                                                     withSpinner(plotlyOutput("plot1")),
                                                     p(tags$small("Source: American Community Survey 2019 1-Year/5-Year Estimates."))
                                              ),
                                              p("", style = "padding-top:10px;")
                                     )) ,
                            ###  subpopulation
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
                                                        newly admitted cases in 2020 are male. While Black youth make up only 8 percent of Loudoun 
                                                        TAY population, they accounted for 15% of juvenile intakes. The majority of youth admitted 
                                                        are aged 14-17; the largest category (almost 30%) being age 17."),
                                                        
                                                        p("It is difficult to analyze socioeconomic patterns for juvenile delinquents after age 20 which would provide us with a 
                                                        guide of the necessary services they would need to help with their transition to adulthood. 
                                                        However, in 2019, over 90 percent of youth incarcerated in Virginia had significant mental 
                                                        health disorder [1]. In addition, there is a high recidivism rate (over 50%) and only 35 total 
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
                                                                p(tags$small(" Source: The Adoption and Foster Care Analysis and Reporting System 2019"))
                                                                
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
                                                                p(tags$small("Source: Department of Juvenile Justice (DJJ) 2020 Data Resource Guide"))
                                                                
                                                       )
                                                     ) 
                                              )   ,
                                              tags$br(), 
                                              h4("References: "),
                                              p("[1] Virginia Dept of Juvenile Justice, & United States of America. (2020). Virginia Department of Juvenile Justice Data Resource Guide Fiscal Year 2020.")
                                     ),
                                     p("", style = "padding-top:10px;")) 
                 ),
                 
                 
                 

                 
                 
                 ## Tab Services--------------------------------------------
                 navbarMenu("Service",
                            tabPanel("Availability", value = "services", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Service Availability"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(12, 
                                                  h4(strong("What Services and Programs are available?")), 
                                                   p("Research literature suggests that there are five main areas that transition aged youths (TAY) struggle 
                                                     with in their transition to adulthood and economic independence:  educational attainment, employment, 
                                                     housing, transportation, and access to healthcare services.    The diagrams presented below show 
                                                     what services exist in Loudoun County along these five dimensions, or pillars.  Branches show services 
                                                     available to all TAY and where available, specifically to TAY from the foster care or juvenile delinquent systems.  
                                                     The same information is presented for the two comparison counties, Fairfax, Virginia and Allegheny, Pennsylvania, 
                                                     to enable comparisons between jurisdictions. "), 
                                                   p("Loudoun and Fairfax, both large, Northern Virginia counties, have similar demographics and close proximity 
                                                     to one another and to large metropolitan areas (Washington D.C., Richmond, VA and Baltimore, MD), making
                                                     them logical comparison peer localities.  Allegheny County, PA, is located in southwest Pennsylvania and is 
                                                     included in the Pittsburgh metropolitan statistical area.   Project stakeholders identified Allegheny County PA 
                                                     as having exemplary transitional service infrastructure for vulnerable TAY.  ")) ,  
                                              column(4, 
                                                     h4(strong("How to find access to and provision of Services and Programs  ")),
                                                     p("The tree diagrams to the right display each service or program available for TAY in Loudoun, Fairfax and Allegheny PA counties as of July, 2021, grouped by pillar (education, employment, health, housing, and transportation) and eligible subpopulation (foster care, juvenile delinquent, or generally to TAY).   "),
                                                     
                                                     p("The AVAILABILTY tab allows users to explore types and targeting of services by locality and pillar.  Users can select the county of interest through tabs and specific pillars by filtering on the radio buttons.  Each tree diagram represents programs of the selected pillar in the selected county.  Nodes - circular points from which branching curves or lines extend - represent first the pillar, then the targeted subpopulation, followed by the service or program name, the intended age range of the program/service, and lastly the office location.  Programs accessible online are so noted.   "), 
                                                     
                                                     p("For service and program locations, select the LOCATION tab.  Users can hover over each program node to activate a description of the service.  To obtain a complete description of all services and programs, select the ALL SERVICES tab. "),
                                                     h5(strong("Key to specific terms used in the branching diagrams: ")),
                                                     p("Any age - No age restriction applies.  If eligibility is restricted to the foster care population, this means that anyone with a history within that system, for example, is eligible for these services or programs, even once they have aged out of them."),
                                                     p("Hybrid - Services or programs are delivered both in-person or online ")
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
                                                          collapsibleTreeOutput("tree1",height = "500px") 
                                                          
                                                          
                                                          
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
                                                          collapsibleTreeOutput("tree3",height = "500px") 
                                                          
                                                          
                                                 ), 
                                                 
                                                 tabPanel("Allegheny, PA",
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
                                                          collapsibleTreeOutput("tree2",height = "500px")
                                                          
                                                       )) 
                                                    )
                                     ) ,
                                     p("", style = "padding-top:10px;")
                            ),
                            
                            tabPanel("Locations", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Location of Programs and Services"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              column(4, 
                                                     h4(strong("Where are these Services and Programs located?")),
                                                     p("The following interactive maps show the office locations of the services and programs available to TAYs in Loudoun county. Hovering over the circular markers will display the name of the service or program. Clicking on the circular markers will open a popup with a detailed description of the service, website link, and whether service delivery is online, in-person, or hybrid. "),
                                                     p("Each service/program is grouped by pillars (education, employment, health, housing, and transportation) and subpopulation (foster care, juvenile delinquent, or generally to TAYs). The radio buttons at the top-right corner of the graph filter the services or programs by subpopulation. For comparison, we also include county maps for Fairfax, VA, and Alleghany, PA.  Not all services require in-person office visits; delivery method of service is listed in the service descriptions, as are links to program websites.  For descriptions of online programs, please select the ALL SERVICES tab. "), 
                                                     p("The Loudoun maps reveal a high concentration of offices in the eastern side of the county, nearer the DC Metropolitan area. This suggests that TAY that reside in the western region may have more difficulty accessing in-person services or programs.  ")
                                                     
                                              ), 
                                              column(8, 
                                                     h4(strong("Locations of Services")),
                                                     column(4,
                                                            selectInput(
                                                              "county",
                                                              "Select County",
                                                              choices = c("Loudoun", "Fairfax", "Allegheny, PA"), 
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
                                                     leafletOutput(outputId = "map1", height = "600px")) 
                                              
                                              
                                     ),
                                     p("", style = "padding-top:10px;")
                            ), 
                            
                            tabPanel("All Services", 
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("All Services"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                              DT::dataTableOutput("mytable")
                                                       
                                              
                                              
                                     )
                            ),
                            p("", style = "padding-top:10px;")
                            
                            
                 ),
                 
                 ## Tab Cross-county----------------
                 
                 navbarMenu( "Cross-County Analysis",
                  
                           tabPanel("Numbers", 
                            fluidRow(style = "margin: 6px;",
                                     h1(strong("Types of Services and Programs"), align = "center"),
                                     p("", style = "padding-top:10px;"), 
                                     column(6,
                                            h4(strong("What services is Loudoun in need of? ")), 
                                            p("Loudoun and Fairfax, both large, Northern Virginia counties, have similar demographics and close proximity 
                                                     to one another and to large metropolitan areas (Washington D.C., Richmond, VA and Baltimore, MD), making
                                                     them logical comparison peer localities.  Allegheny County, PA, is located in southwest Pennsylvania and is 
                                                     included in the Pittsburgh metropolitan statistical area.   Project stakeholders identified Allegheny County PA 
                                                     as having exemplary transitional service infrastructure for vulnerable TAY.  "),
                                            p("The tables to the right summarize the data by providing counts of services and programs
                                                               available in each county by subpopulation and by pillar.  Programs specifically targeted at 
                                                               helping youth transition from the juvenile justice system are a distinct minority.  Programs for foster 
                                                               care and former foster care youth are only slightly more widespread.  The great majority of services 
                                                               and programs are available for vulnerable TAY in general.  "), 
                                            
                                            p("For Loudoun County, the distribution of programs by pillar indicate that the number of 
                                                               services and programs are fairly equal among areas, except there are many fewer transportation services."),
                                            p("While the number of services and programs are an important index of resources available to help TAY in 
                                                               their path toward a viable economic independence, the quality and range of those programs is also critical. 
                                                               In particular, how well are the programs meeting specific needs for these TAY populations?  
                                                               The project shows that most services appear to be located in the eastern portion of the county
                                                               (see Locations tab) and that transportation services are the least well represented. 
                                                               Getting youth to necessary services from all parts of the county may represent a gap that 
                                                               could be further explored in subsequent projects")), 
                                     column(6, 
                                            h4(strong("Number of Programs by Subpopulation")), 
                                            tableOutput("table1"),
                                            tags$br(), 
                                            h4(strong("Number of Programs by Pillar")), 
                                            tableOutput("table2"))
                                     
                            ),
                            p("", style = "padding-top:10px;")), 
                            
                            tabPanel("Gaps",
                            fluidRow(style = "margin: 6px;",
                                     h1(strong("Gaps in Services and Programs"), align = "center"),
                                     
                                     p("", style = "padding-top:10px;"), 
                                     column(4,  
                                            h4(strong("Where are the gaps in Services and Programs? ")), 
                                            p("Some of these tailored programs are embedded within broader services.  
                                                                For example, ",strong("Great Expectations Services"), " for Youth in Foster Care within the Northern Virginia Community College
                                                                system helps connect this population with higher education.  Others include financial assistance or 
                                                                guidance aimed at assisting with the transition from residential care to independence, such as ",strong("LIFT 2")," for 
                                                                youth aging out of the foster care system.    "), 
                                            h5(strong("Juvenile Justice Involved Specific Programs/Services")),
                                            p("Programs specifically targeted at helping youth transition from the juvenile justice system are a distinct minority. 
                                                                In Northern Virginia, ", strong("OAR (Breaking the Cycle of Crime through Opportunities, Alternatives and Resources)"),  " 
                                                                offers post-release and family services, including case manager and emergency services for formerly incarcerated adults, 
                                                                providing programs to residents of both Loudoun and Fairfax (as well as another Virginia county).   
                                                                Youth who only have juvenile justice involvement are also eligible for these services.    
                                                                However, these services are broadly available to all post-incarcerated residents. "), 
                                            p("Three programs that are exclusively tailored to post-detention TAY needs focus on housing and employment.  
                                                                Unique to Loudoun is ", strong("Mobile Hope's Handcuffs to Hope program"),  "which works with incarcerated youth three to 
                                                                six months prior to release to prevent future homelessness.   Allegheny County, PA, hosts two programs tailored to 
                                                                helping TAY with past or current involvement in the criminal justice system train for and find employment.  
                                                                Both administered by Partner4Work, ", strong("Skills and Training from Reentry to Integrated Vocation and Employment (STRIVE)"), " 
                                                                provides comprehensive case management as well as a suite of job-focused programs, while the ", strong("Career Pipeline Project"), 
                                              " offers paid on-the-job training opportunities. "), 
                                            h5(strong("Foster Care Specific Programs")),
                                            p("Programs for foster care and former foster care youth specifically include programs targeting educational,  
                                                                employment, housing and health needs, such as ", strong("LIFT 2"), " in Fairfax that addresses a wide range of potential 
                                                                needs for this population, including mental health concerns.  For Loudoun, the most wide-ranging program is the ", strong("Independent 
                                                                Living Program"), " administered by the Department of Social Services.  However, both these programs stop serving youth after age 21. "), 
                                            h5(strong("The Biggest Gaps")),
                                            p("Reviewing the breadth and number of support services and programs across all counties, 
                                                                it appears the biggest gaps for Loudoun may lie in specialized attention to older transitional youth,
                                                                and lack of a case management system for the TAY subpopulations of interest.   "), 
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
                                     
                            ),
                            p("", style = "padding-top:10px;")
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
                                                                         p("Our target population for this project are transition aged youths. This include those that are “aging out” of the foster care system or 
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
                                                                       p(tags$small("Source: Department of Mental Health, Substance Abuse and Developmental Services"))  ,  
                                                                       p(tags$small("*The Case Management waitlist does not include I/DD individuals waiting for Support Coordination as this is largely dependent on state-allotted waivers."))  ,  
                                                                       p(tags$small("**Since the start of the Same Day Access program in 2019, MHSADS has gotten rid of the Outpatient Services waitlist. ")))
                                              
                                     ),
                                     p("", style = "padding-top:10px;")), 
                                     tabPanel("Demographics", 
                                              fluidRow(style = "margin: 6px;",
                                                       p("", style = "padding-top:10px;"), 
                                                       column(4, 
                                                              h4(strong("Who does MHSADS Serve?")),
                                                              p("TAYs with disabilities or mental or substance issues face even more barriers as they move towards independence.  
                                                                         These youths experience an adult poverty rate three times higher than their peers [1]. This high level of poverty,
                                                                         difficulty in accessing health services, and homelessness tend to have a very negative impact on the likelihood of 
                                                                         employment thereby, compounding the transition to self-sufficient adulthood [2].  Moreover, it is estimated that over 
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
                                                                  "Severe Mental Health" = "mental"
                                                                )
                                                                
                                                              ), 
                                                              plotlyOutput(outputId = "dmhsaPlot", height = "500px") ,
                                                              p(tags$small("Source: Department of Mental Health, Substance Abuse and Developmental Services 2016-2020"))
                                                              ),
                                                       column(12, 
                                                              h4("References: "),
                                                       p(tags$small("[1] Newman, L., Goscha, S. and Coakley, P. (2021). Transitional Aged Youth with Barriers: Support Needed to Achieve Self Sufficiency. Public Consulting Group Inc.  Retrieved 27 July 2021, from  https://www.publicconsultinggroup.com/media/1299/transitional_aged_youth_with_barriers_white_paper.pdf")),
                                                       p(tags$small("[2] Hemmeter, Jeffrey (2014). Earnings and Disability Program Participation of Youth Transition Demonstration Participants after 24 Months. Social Security Bulletin, Vol. 74 No. 1. Retrieved from: http://www.ssa.gov/policy/docs/ssb/v74n1/v74n1p1.html" ))
                                                       )
                                                       
                                              ),
                                              p("", style = "padding-top:10px;")), 
                                     tabPanel("Trends", 
                                              fluidRow(style = "margin: 6px;",
                                                       p("", style = "padding-top:10px;"), 
                                                       column(4, 
                                                             h4(strong("Individuals Served from 2016 – 2020")),
                                                             p("Visualizing the usage of services allows us to determine whether some services are used more than others. 
                                                               Loudoun County’s Department of Mental Health, Substance Abuse, and Developmental Services (DMHSA) provided us with data on 
                                                               the number of transition aged youths (TAY) served by zip code level. The dropdown box allows for the selection of the various programs. 
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
                                                             p(tags$small("Source: Department of Mental Health, Substance Abuse, and Developmental Services 2019 "))
                                                             
                                                             
                                                      )
                                              ) ,
                                              p("", style = "padding-top:10px;")
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
                                                                         p("Loudoun Family Services is a department within the county ",a(href="https://www.loudoun.gov/107/Family-Services-Department","Health and Human 
                                                                        Services agency ",target="_blank")," that administers the local foster care program as well as other support services.  From 2016 – 2020, family service 
                                                                           programs served more than 2,500 transition aged youths (TAY).  "),
                                                                         p("Transition aged youths used public benefits by far the most during this time period and used transitional housing minimally.  
                                                                           Public benefits here include financial assistance for food (Supplemental Nutritional Assistance Program, or SNAP), 
                                                                           health insurance (Medicaid) and for low-income families with children (Temporary Assistance for Needy Families, TANF).  
                                                                           White TAY used these supports more than any other racial group (59%), with 63% of recipients being female.  While more White 
                                                                           TAY used emergency shelters, Black TAY used emergency shelter more than other groups proportional to their representation in the county.  
                                                                           Slightly more male than female TAY (56%) made use of emergency shelter.   The only service Asian residents used to any extent was public benefits.  
                                                                           Hispanic ethnicity was not reported for these statistics. "),
                                                                         p("On the Trends tab, counts of TAY service use are presented for each year for these as well as two workforce programs.  
                                                                           Note that counts differ for transitional housing between the demographic graphs presented here and the trend counts.  
                                                                           For this program only, demographic information includes all program users.  ")
                                                                         
                                                                         
                                                                  ), 
                                                                  column(8, 
                                                                         
                                                                         h4(strong("Demographics")), 
                                                                         selectInput(
                                                                           "familyDemo",
                                                                           "Select Demographic",
                                                                           choices = list(
                                                                             "Public Benefits Gender" = "publicGender",
                                                                             "Public Benefits Race" = "publicRace",
                                                                             "Emergency Shelter Gender" = "emerG",
                                                                             "Emergency Shelter Race" = "emerR",
                                                                             "Transitional Housing Gender" = "transG",
                                                                             "Transitional Housing Race" = "transR"
                                                                           )
                                                                           
                                                                         ), 
                                                                         plotlyOutput(outputId = "familyPlot", height = "500px") 
                                                                         
                                                                  )
                                                         ),
                                                         p("", style = "padding-top:10px;")
                                                ),
                                                tabPanel("Trends", 
                                                         fluidRow(style = "margin: 6px;",
                                                                  p("", style = "padding-top:10px;"), 
                                                                  column(4, 
                                                                         h4(strong("Utilization Trends")),
                                                                         p("Based on data provided by Loudoun County, the animation to the right shows how many TAY were served by year, from 2016 to 2019, for unique programs.  
                                                                           Medicaid was the most widely used benefit by a large margin.  Not surprisingly for these young adults, few used the family support TANF,
                                                                           but approximately 300 TAY used career support services through the Workforce Resource Center. ")
                                                                        ) ,
                                                                  column(8, 
                                                                         tags$img(src="gganim.gif", controls = "controls", height = "800px", width = "900px") , 
                                                                         br(), 
                                                                         p(tags$small("Public Benefits Source: Family Services Data Warehouse 2016-2020")),
                                                                         p(tags$small("Emergency and Transitional Housing Source: ServicePoint & HUD 2016-2020")),
                                                                         p(tags$small("WRC Source: WRC 2015-2019")),
                                                                         p(tags$small("WIOA Source: WRC 2017-2020"))
                                                                         
                                                                         
                                                                  )
                                                         ) ,
                                                         p("", style = "padding-top:10px;")
                                                )
                                                
                                              )
                                     ) 
                            ) ,
                            
                            ### Individual served------------
                            tabPanel("Other Programs", value = "served",
                                     fluidRow(style = "margin: 6px;",
                                              h1(strong("Other Programs"), align = "center"),
                                              p("", style = "padding-top:10px;"), 
                                                column(4, 
                                                     h4(strong("Other programs in Loudoun – who uses them?")),
                                                     p("We offer a diverse array of program implementation statistics, presented in figures to the right, for users interested in details of additional program uptake and delivery.  Because each program collects implementation data differently, featured characteristics vary across program.  Each graph represents a single year."),
                                                     infoBoxOutput("transit" , width = 4)),
                                              
                                
                                              column(8, 
                                                     selectInput(
                                                       "stat",
                                                       "Select Program",
                                                       choices = list(
                                                         "Adult Literacy Programs Enrollment" = "literacy",
                                                         "Adult Literacy Program Race" = "literacyR",
                                                         "OAR Enrollment"= "oar", 
                                                         "OAR Demographics"= "oarD",
                                                         "Loudoun Cares Demographics" = "cares", 
                                                         "Oxford House Average Stay" = "oxford1",
                                                         "Oxford House Prior" = "oxford2",
                                                         "Route 54 Safe-T Weekly Usage" = "transit"
                                                       )
                                                     ), 
                                                     plotlyOutput(outputId = "demographics", height = "500px")
                                                    ) 
                                                                         
                                             ),
                                     p("", style = "padding-top:10px;")
                                              
                              ) 
                            
                 ),
                 ## Tab Takeaway --------------------------------------------
             #     tabPanel("Takeaway",  value = "conclusion", 
             #              fluidRow(style = "margin: 6px;",
             #                       h1(strong("Conclusion"), align = "center")),  
             #              p("", style = "padding-top:10px;"), 
             #              
             #              fluidRow(style = "margin: 6px;",
             #                       column(4,
             #                               h2(strong("Provision")),
             #                                tags$ul(
             #                                tags$li("Beginning our project with a literature review and initial research, we knew the 5 pillars we wanted to focus our search on: Education, Employment, Housing, Transportation, and Health Services. 
             #                                For vulnerable Transtion Aged Youth, ages 18-24, it is difficult for them to live independently especially those who have just aged out of the foster care system or gotten out of 
             #                                juvenile detention. After we webscrapped all of the programs in Loudoun County available for Transition Aged Youth and those specific to former foster care
             #                                youths or juvenile delinquents, we started a cross-county analysis with Fairfax County, VA and Allegheny County, PA. We repeated the webscrapping process with both counties, searching for programs within the same 
             #                                5 pillars for transition aged youth, those who have aged out of the foster care system and those who have gotten out of juvenile detention. 
             #                                 "), 
             #                                tags$li("The interactive trees are a simple and easy way to display all of the programs and its most important information for each county seperately and together. We noticed gaps in transportation and housing programs in 
             #                                Loudoun compared to Allegheny and Fairfax. The leaflet maps are used to visualize the locations of the services for each county and we noticed geographic gaps in services on the west side of the county. "), 
             #                                tags$li("Third list item")
             #                              )
             #                            ),
             #                       
             #                       column(4,
             #                              h2(strong("Utilization")),
             #                              tags$ul(
             #                                tags$li("Once we noticed the gaps in services in specific pillars like transportation and housing, we searched for more information regarding those who 
             #                                        use these programs in Loudoun. If we can graph which demographics are using these services the most and the least, we may be able to see another gap in services regarding race, socieconomic status, sex, etc. 
             #                                        "), 
             #                                tags$li("Second list item"), 
             #                                tags$li("Third list item")
             #                              )
             #                       ),
             #                       column(4,
             #                              h2(strong("Gaps")),
             #                              tags$ul(
             #                                tags$li("As noted above, once we mapped and visualized the programs' locations and descriptions, it was apparent which pillars Loudoun was lacking in and had an abundance in. 
             #                                        One of the most important gaps that needs to be addressed is the gap in the Transportation pillar. We can infer that most transition aged youth do not have a vehicle of their own and those who have aged out
             #                                        of the foster care system or juvenile detention are even more unlikely to own a vehicle because of the lack of family and friends support. This means that in order to get to one of the programs for employment or education, they would have
             #                                        to have some form of transportation other than a car, public transportation. However, in Loudoun county, the only form of public transportation that was free was one bus route, Safe-T that has 10 stops, most of 
             #                                        them in 3 shopping centers in Leesburg. Because there is no other transportation service that a transition aged youth could qualify for, they are likely not going to be able to get to other programs for employment or education. "), 
             #                                tags$li("Second list item"), 
             #                                tags$li("Third list item")
             #                              )
             #                       )
             # 
             #            )
             # ),
             ## Tab Data --------------------------------------------
             tabPanel("Data ", value = "data",
                      fluidRow(style = "margin: 6px;",
                               h1(strong("Data Sources"), align = "center"),
                               p("", style = "padding-top:10px;"), 
                               column(6,
                                      img(src = 'data-acs.png', style = "display: inline; float: left;", width = "250px"),
                                      p("We retrieve ",strong("American Community Survey (ACS)")," data to examine demographic and socioeconomic characteristics of our target population. 
                                            ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets.
                                            We used the most recently available 1-year/5-year estimates, to characterize Loudoun County’s transition aged youths by age, race,
                                            gender, educational attainment, health insurance coverage, and poverty level. "),
                                      br(), 
                                      img(src = 'family-services.jpg', style = "display: inline; float: left;", width = "200px"),
                                      p("The ", strong("Loudoun County Department of Family Services"), "holds record of those who use their provided services based on number of persons, percent of transition aged youths 
                                            and year. We graphed several demographics like gender, race and age for mulitple programs and showed a timeseries of utilization from 2016-2020. "), 
                                      br(), 
                                      br(), 
                                      br(), 
                                      img(src = 'data-virginiaDSS.jpeg', style = "display: inline; float: left;", width = "250px"),
                                      p("The ", strong("Virginia Department of Social Services"), " ensures that thousands of Virginia's most vulnerable citizens have access to the best services and benefits available to them.
                                            We researched through their website and found various programs and services that are available to transition aged youths in Loudoun and Fairfax in order to find the gaps in
                                            certain pillars. "),
                                      p("", style = "padding-top:10px;")) ,
                               column(6,
                                      img(src = 'family-services.jpg', style = "display: inline; float: left;", width = "200px"),
                                      p("The ", strong("Loudoun County Department of Mental Health, Substance Abuse, and Developmental Services"), "reports the number of individuals that use their provided programs and different demographics like
                                            age, gender and race. They split their data based on zipcodes which we used to map out the utilization of their provided services and compare to the area's population density. "),
                                      br(), 
                                      br(), 
                                      br(), 
                                      img(src = 'data-afcars.png', style = "display: inline; float: left;", width = "300px"),
                                      p("We used ", strong("The Adoption and Foster Care Analysis and Reporting System")," to report the number of youths in foster care in Loudoun County. 
                                            This allowed us to determine how many youths need services to help with the transition out of the foster care system.  "),
                                      br(), 
                                      img(src = 'data-djj.jpg', style = "display: inline; float: left;", width = "200px"),
                                      p("The ", strong("Virginia’s Department of Juvenile Justice")," produces a Data Resource Guide annually highlighting data and trends on
                                                  the juvenile detention centers through the Commonwealth. We used the 2019 report to determine the demographic characteristics 
                                                  and the total number youth intakes and those leaving the centers.  "),
                                      p("", style = "padding-top:10px;")
                                      
                               )
                               
                      )
                      
             ),
                 ## Tab Team------
                 tabPanel("Team", value = "team",
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h1(strong("Team"), align = "center"),
                                   br(),
                                   h4(strong("VT Data Science for the Public Good"), align = "center"),
                                   p("The", a(href = 'https://aaec.vt.edu/academics/undergraduate/beyond-classroom/dspg.html', 'Data Science for the Public Good (DSPG) Young Scholars program', target = "_blank"),
                                     "is a summer immersive program offered by the", a(href = 'https://aaec.vt.edu/index.html', 'Virginia Tech Department of Agricultural and Applied Economics'), 
                                     "In its second year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges 
                                     around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to 
                                     determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, 
                                     how to apply, and our annual symposium, please visit", 
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
                                          p("", style = "padding-top:10px;"), 
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
                                          p("", style = "padding-top:10px;"), 
                                          p(a(href = "https://aaec.vt.edu/people/faculty/holmes-chanita.html", 'Chanita Holmes', target = '_blank'), "(Project Lead, Virgina Tech, Research Assistant Professor);") , 
                                          p(a(href = "https://liberalarts.vt.edu/departments-and-schools/department-of-human-development-and-family-science/faculty/isabel-bradburn.html", 'Isabel Bradburn', target = '_blank'), "(Research Faulty,Virgina Tech, Department of Human Development and Family Science)."),
                                          p("", style = "padding-top:10px;")
                                   )) ,
                          fluidRow(style = "margin-left: 100px; margin-right: 100px;",
                                   h4(strong("Project Stakeholders")),
                                   p(a(href = 'https://loudoun.ext.vt.edu/staff/Vermaak-Stuart.html', 'Stuart Vermaak', target = '_blank'), "(Virginia Cooperative Extension, Loudoun County at Virginia Tech);"),
                                   p(a(href = 'https://loudoun.ext.vt.edu/staff/Hilleary-James.html', 'James Hilleary', target = '_blank'), "(Virginia Cooperative Extension, Loudoun County at Virginia Tech)."),
                                   p("", style = "padding-top:10px;"),
                                   h4(strong("Acknowledgments")) ,
                                   p("We would like to thank Loudoun officials for providing us with data for our project. Specifically, the Department of Mental Health, Substance Abuse, and Developmental Services and the Family Service Department.  "),
                                   p("", style = "padding-top:10px;")
                          )
                 ), inverse = T ) 


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
      
      p <- race %>%
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
      
     p <- pov %>%
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
        rename(`Percent (%)` = Proportion) %>% 
        mutate(Age = recode(Age, `12-Aug` = "8-12" )) %>% 
        filter(Age != "Missing")
      
      
      p <- jv_age %>% 
        ggplot(aes(x = Age, y = `Percent (%)`, fill = Age)) +
        geom_col() +scale_fill_viridis_d() + 
        labs(x = "", y = "Percent (%)",
             title = "Age Groups of Juvenile Intakes") + 
        theme_minimal() + 
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
        coord_flip() 
      q <- ggplotly(p, tooltip = "y")
      q
    }
    else if(var3() == "race"){
      jv_race$Race <- factor(jv_race$Race, levels=unique(jv_race$Race))
      jv_race <- jv_race %>% mutate(`Percent (%)` = Proportion) 
      
      p <- jv_race %>% 
        ggplot(aes(x = Race, y = `Percent (%)`, fill =Race)) +
        geom_col() +
        labs(x = "", y = "Percent (%)",
             title = "Racial Demographics of Juvenile Intakes") + 
        theme_minimal() + scale_fill_viridis_d() +
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        coord_flip() 
      q <- ggplotly(p, tooltip = "y")
    }
    else if (var3() == "eth") {
      jv_eth$Ethnicity <- factor(jv_eth$Ethnicity, levels=unique(jv_eth$Ethnicity))
      jv_eth <- jv_eth %>% mutate(`Percent (%)` = Proportion)
      
      
      p <- jv_eth %>% 
        ggplot(aes(x = Ethnicity, y = `Percent (%)`, fill = Ethnicity))  +
        geom_col() +
        labs(x = "", y = "Percent (%)",
             title = "Ethnicity of Juvenile Intakes") + 
        theme_minimal() + scale_fill_viridis_d() +
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        coord_flip()
      q <- ggplotly(p, tooltip = "y")
      
    }
    else{
      jv_sex$Sex <- factor(jv_sex$Sex, levels=unique(jv_sex$Sex))
      jv_sex <- jv_sex %>% mutate(`Percent (%)` = Proportion) %>% 
        mutate(`Percent (%)` = 100 * `Percent (%)` )
      
      
      p <- jv_sex %>% 
        ggplot(aes(x = Sex, y = `Percent (%)`, fill = Sex)) +
        geom_col() +
        labs(x = "", y = "Percent (%)",
             title = "Sex Demographics of Juvenile Intakes") + 
        theme_minimal() + scale_fill_viridis_d() +
        theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
        coord_flip()
      
      q <- ggplotly(p, tooltip = "y")
      
      
    }
    
  }) 
  
  #Waitlist for programs in Loudoun 
  output$waitlist <- renderPlotly({
    
    p <- ggplot(waitlist, aes(x=Year)) + 
      geom_line(aes(y = Persons, group = Program, color = Program))+theme_minimal()+ 
      labs(y = "Total Number of TAY") + theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    
    ggplotly(p, tooltip = c("y", "x", "group")) 
    
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
      Tree%>%filter(County == "Allegheny, PA")%>%
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
      Tree%>%filter(County == "Allegheny, PA")%>%
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
      Tree%>%filter(County == "Allegheny, PA")%>%
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
      Tree%>%filter(County == "Allegheny, PA")%>%
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
      Tree%>%filter(County == "Allegheny, PA")%>%
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
          setView(lng = -77.4, lat = 38.9, zoom = 10) %>% 
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
          setView(lng = -77.4, lat = 38.9, zoom = 10) %>% 
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
      relocate(Loudoun, .after = Subpopulation) %>% 
      relocate(`Allegheny, PA`, .after = Fairfax) %>% 
      adorn_totals("row")
    subpop_count
    
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  output$table2 <- renderTable({
    programs <- read_excel("data/combined-programs.xlsx")
    pillar_count <- programs %>% 
      group_by(County) %>% 
      count(Pillars) %>% 
      pivot_wider(names_from = County, values_from = n) %>% 
      relocate(Loudoun, .after = Pillars) %>% 
      relocate(`Allegheny, PA`, .after = Fairfax) %>% 
      slice(1:2, 4, 5, 3) %>% adorn_totals("row")
  }, striped = TRUE, hover = TRUE, bordered = TRUE, width = "100%", align = "r", colnames = T, digits = 2)
  
  
  
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
        labs(title = "Race of TAY",y = "Percent TAYs served in Loudoun", x= "" ) +
        scale_fill_viridis_d() +
        theme_minimal()+ theme(legend.position="bottom")
      
      ggplotly(p, tooltip = c("y", "group")) %>% layout(legend = list(orientation = "h", y=-0.2)) 
      
    }else if (dmhsaDemos() == "dmhsaGender"){
      
      p <-mhs_sex_relative %>% 
        ggplot(aes(group = Gender, x = Year, y = `Percent`,
                   fill = Gender)) + 
        geom_col() +theme_minimal() + 
        scale_y_continuous(labels = scales::percent) +
        labs(title = "Gender of TAY", y = "Percent TAYs served in Loudoun", x="" ) +
        scale_fill_viridis_d() +
        theme_minimal()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("y", "group"))
      
      
    }else {
      
      p <- ggplot(smi, aes(x=Year)) + 
        geom_line(aes(y = Persons, group = Group, color = Group)) + theme_minimal() + 
        labs(title = "Severe Mental Illness, 5 Year Count", y = "Total TAYs served in Loudoun", x="" )+scale_fill_viridis_d() +
        theme( panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = c("y", "x","group"))

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
             x = "") +scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = 1, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) 
      
      ggplotly(p, tooltip = "y")%>% layout(
        annotations = 
          list(x = 1,  y = -.2, text = "Source: Data Warehouse Public Benefits 2016-20", 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10)))
      
    }else if (familyDemo() == "publicGender"){
      
      publicG$Gender <- factor(publicG$Gender, levels=unique(publicG$Gender))
      
      p <- ggplot() + geom_col(mapping = aes(Gender, Number, fill = Gender), data =publicG) + 
        labs(title = "Public Benefits Gender",y = "Total TAYs served in Loudoun", 
             x = "") +scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "y")%>% layout(
        annotations = 
          list(x = 1,  y = -.14, text = "Source: Data Warehouse Public Benefits 2016-20", 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10)))
      
      
    }else if (familyDemo() == "emerG"){
      
      emerG$Gender <- factor(emerG$Gender, levels=unique(emerG$Gender))
      
      p <- ggplot() + geom_col(mapping = aes(Gender, Number, fill = Gender), data =emerG) + 
        labs(title = "Emergency Shelters Gender",y = "Total TAYs served in Loudoun", 
             x= "") +scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "y")%>% layout(
        annotations = 
          list(x = 1,  y = -.14, text = "Source: ServicePoint & HUD Emergency Shelter 2016-20", 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10)))
      
      
    }else if (familyDemo() == "transG"){
      
      transG$Gender <- factor(transG$Gender, levels=unique(transG$Gender))
      
      p <- ggplot() + geom_col(mapping = aes(Gender, Number, fill = Gender), data =transG) + 
        #Yang's change add "All users)
        #labs(title = "Transitional Housing Gender ",y = "Total TAYs served in Loudoun", 
        labs(title = "Transitional Housing Gender ",y = "All adults served in Loudoun", 
             x = "") +scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "y")%>% layout(
        annotations = 
          list(x = 1,  y = -.15, text = "Source: ServicePoint & HUD Transitional Housing 2016-20 ", 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10)))
      
      
    }else if (familyDemo() == "transR"){
      
      transR$Race <- factor(transR$Race, levels=unique(transR$Race))
      
      p <- ggplot() + geom_col(mapping = aes(Race, Number, fill = Race), data =transR) + 
        labs(title = "Transitional Housing Race/Ethnicity",y = "All adults served in Loudoun", 
             x = "") +scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
              legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(p, tooltip = "y")%>% layout(
        annotations = 
          list(x = 1,  y = -.16, text = "Source: ServicePoint & HUD Transitional Housing 2016-20", 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10)))
      
      
      }else {
      
        emerR$Race <- factor(emerR$Race, levels=unique(emerR$Race))
        
        p <- ggplot() + geom_col(mapping = aes(Race, Number, fill = Race), data =emerR) + 
          labs(title = "Emergency Shelters Race/Ethnicity", y = "Total TAYs served in Loudoun",  
               x = "") +scale_fill_viridis_d() +theme_minimal() + 
          theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
                legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
        ggplotly(p, tooltip = "y",caption = "This program serves all adults including TAY.")%>% layout(
          annotations = 
            list(x = 1,  y = -.15, text = "Source: ServicePoint & HUD Emergency Shelter 2016-20", 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=10))) 
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
             y = "Total TAYs and Adult Residents served in Loudoun", x="")+scale_fill_viridis_d() +theme_minimal() + 
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
        labs(title = "Oxford Houses in Virginia", x="", 
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
        labs(title = "Oxford Houses in Virginia",
             x="", 
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
             y = "Total TAYs and Adult Residents served in Loudoun", x="")+scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),legend.position = "none",
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(plot, tooltip = "y") %>% layout(
        annotations = 
          list(x = 1,  y = -.2, text = "Source: OAR Annual Report 2020", 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size=10))) 
      
      
      }else if (stat() == "oarD"){
        
        plot <- ggplot() + geom_col(mapping = aes(Category, Number, fill = Category), data = all[3:7,]) + 
          labs(title = "Demographics of OAR program",
               y = "Percent of TAYs and Adult Residents served in Loudoun", x="") +scale_fill_viridis_d() +theme_minimal() + 
          theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
                legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
        ggplotly(plot, tooltip = "y") %>% layout(
          annotations = 
            list(x = 1,  y = -.43, text = "Data Source: OAR Annual Report 2020", 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=10))) 
        
        
      }else if (stat() == "cares"){
      
        plot <- ggplot() + geom_col(mapping = aes(Name, Stat, fill = Name), data = cares) + 
          labs(title = "Statistics of Loudoun Cares Program",
               y = "Total TAYs served in Loudoun",
                x= "") +scale_fill_viridis_d() +theme_minimal() + 
          theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"),
                legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank())
        
        ggplotly(plot, tooltip = "y") %>% layout(
          annotations = 
            list(x = 1,  y = -.6, text = "Data Source: Loudoun Cares Annual Report 2020", 
                 showarrow = F, xref='paper', yref='paper', 
                 xanchor='right', yanchor='auto', xshift=0, yshift=0,
                 font=list(size=10))) 
      
      
    }else if (stat() == "transit") {
      
      plot <- transit %>% 
        ggplot(aes( x = Day, y = Average,fill = Day)) + 
        geom_col() +
        labs(title = "Average Riders on Route 54 Safe-T",y = "Riders in Loudoun",
             x="") +scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"), legend.position = "none", 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(plot, tooltip = "y") %>% layout(
        annotations = 
          list(x = 1, y = -.2, text = "Source: Transit Development Plan 2018-2028", 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size= 10))) 
      
      
      }else {
      
      plot <- ggplot() + geom_col(mapping = aes(Type, Number, fill = Type), data = literacy) + 
        labs(title = "Adult Literacy Program", 
             y = "Total TAYs and Adult Residents served in Loudoun", x="")+scale_fill_viridis_d() +theme_minimal() + 
        theme(axis.text.x = element_text(angle = 45, vjust = .5, color = "black"), legend.position = "none", 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank())
      
      ggplotly(plot, tooltip = "y") %>% layout(
        annotations = 
          list(x = 1, y = -.2, text = "Source: Loudoun County Public School 2019-2020", 
               showarrow = F, xref='paper', yref='paper', 
               xanchor='right', yanchor='auto', xshift=0, yshift=0,
               font=list(size= 10))) 
    }
    
    
  }) 
  
  output$mytable = DT::renderDataTable({
    datatable(list, filter = 'top')
  })

  
  
  
  
  
  
}


# Run the application-----------
shinyApp(ui = ui, server = server)
