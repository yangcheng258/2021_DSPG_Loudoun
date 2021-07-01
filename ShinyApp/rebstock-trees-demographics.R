#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(ggplot2)
library(DT)
library(plotly)
library(dplyr)
library(tidyverse)
library(tidycensus)
library(ggbeeswarm)
library(readxl)
library(collapsibleTree)
library(shinycssloaders)
library(leaflet)
library(leaflet.extras)


# Loudoun -----------------------------------------------------------
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
race$Race <- rownames(race)

# education 
male_edu <- c("B15001_003","B15001_004","B15001_005","B15001_006","B15001_007","B15001_008","B15001_009","B15001_010") 
female_edu <- c("B15001_044","B15001_045", "B15001_046", "B15001_047", "B15001_048", "B15001_049","B15001_050","B15001_051") 
education <- get_acs(geography = 'county', variables = c(male_edu, female_edu) , state= 'VA',county='Loudoun')

school <- c("Less than 9th", "9th to 12th (no diploma)", "High school diploma", "Some college (no degree)", "Associate's degree", "Bachelor's degree", "Graduate or professional degree")

medu <- education[2:8,]%>%
  select(variable, estimate)
medu$variable <- school
fedu <- education[10:16,]%>%
  select(estimate)

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
  select(variable, sum)%>%
  mutate(variable = "White")
b_p <- data.frame(poverty[3,], poverty[4,])%>%
  mutate(sum = estimate + estimate.1)%>%
  select(variable, sum)%>%
  mutate(variable = "Black")
i_p <- data.frame(poverty[5,], poverty[6,])%>%
  mutate(sum = estimate + estimate.1)%>%
  select(variable, sum)%>%
  mutate(variable = "Indian")
as_p <- data.frame(poverty[7,], poverty[8,])%>%
  mutate(sum = estimate + estimate.1)%>%
  select(variable, sum)%>%
  mutate(variable = "Asian")
n_p <- data.frame(poverty[9,], poverty[10,])%>%
  mutate(sum = estimate + estimate.1)%>%
  select(variable, sum)%>%
  mutate(variable = "Native")
o_p <- data.frame(poverty[11,], poverty[12,])%>%
  mutate(sum = estimate + estimate.1)%>%
  select(variable, sum)%>%
  mutate(variable = "Other")

# Health Care
covered <- 1473 
private_cov <- 25418
medicare <- 58 
va <- 69

healthcare <- data.frame(rbind(covered, private_cov, medicare, va))
healthcare$Type <- c("Public", "Private", "Medicare", "VA Health")
colnames(healthcare) <- c("Estimate", "Type")

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
jv_race <- intake_race %>% select(RACE, `FY20 %`, CSU) %>% 
  filter(CSU == "20L") %>% 
  rename(Proportion = `FY20 %`) %>% 
  select(RACE, Proportion) %>% 
  rename(Race = RACE)%>%
  mutate(Proportion = readr::parse_number(as.character(Proportion)))


#Eth 
intake_eth <- read_csv(paste0(getwd(), "/data/DJJ-2020-Juvenile_Detention_Locality-Ethnicity_Intake.csv")) 
colnames(intake_eth) <-intake_eth[1,]
intake_eth <- intake_eth[-1,]
jv_eth <- intake_eth %>% select(ETHNICITY, `FY20 %`, CSU) %>% 
  filter(CSU == "20L") %>% 
  rename(Proportion = `FY20 %`, Ethnicity = ETHNICITY) %>%
  select(Ethnicity, Proportion)%>%
  mutate(Proportion = readr::parse_number(as.character(Proportion)))

# Sex
intake_sex <- read_csv(paste0(getwd(),"/data/DJJ-2020-Juvenile_Detention_Locality-Sex_Intake.csv")) 
jv_sex <- intake_sex %>% select(SEX, `FY20 %`, CSU) %>% 
  filter(CSU == "20L") %>% 
  rename(Proportion = `FY20 %`, Sex = SEX) %>%
  select(Sex, Proportion)%>%
  mutate(Proportion = readr::parse_number(as.character(Proportion)))

# Age
intake_age <- read_csv(paste0(getwd(),"/data/DJJ-2020-Juvenile_Detention_Locality-Age_Intake.csv")) 
colnames(intake_age) <-intake_age[1,]
intake_age <- intake_age[-1,]
jv_age <- intake_age %>% select(AGE, `FY20 %`, CSU) %>% 
  filter(CSU == "20L") %>% 
  rename(Proportion = `FY20 %`, Age = AGE) %>%
  select(Age, Proportion) %>% 
  filter(Age != "Total Cases")%>%
  mutate(Proportion = readr::parse_number(as.character(Proportion)))



# Trees -----------------------------------------------------------
Tree <- read_excel(paste0(getwd(),"/data/combined-programs.xlsx")) 
# Maps -----------------------------------------------------------
map <- read_excel(paste0(getwd(),"/data/combined-programs.xlsx")) 

loudoun_locations <- map %>%
  filter(County == "Loudoun") %>% 
  rename(Longitude = Latitude, Latitude = Longitude) %>%
  select(Program, Longitude, Latitude, Pillars, Subpopulation, Qualification, Description, Website) %>%
  filter(Longitude != "Online" & Longitude != "Mulitple locations") %>% drop_na()

loudoun_locations$Longitude <- as.numeric(loudoun_locations$Longitude)
loudoun_locations$Latitude <- as.numeric(loudoun_locations$Latitude)

allegheny_locations <- map%>%
  filter(County == "Allegheny") %>% 
  rename(Longitude = Latitude, Latitude = Longitude) %>%
  select(Program, Longitude, Latitude, Pillars, Subpopulation,  Qualification, Description, Website) %>%
  filter(Longitude != "Online" & Longitude != "Mulitple locations") %>% drop_na()

allegheny_locations$Longitude <- as.numeric(allegheny_locations$Longitude)
allegheny_locations$Latitude <- as.numeric(allegheny_locations$Latitude)


subpop_levels <- c("Foster Care", "Juvenile Detention")
subpop_pal <- colorFactor(pal = c('red', 'green'),
                          levels = subpop_levels)

Pillar_levels <- unique(loudoun_locations$Pillars)
Pillar_pal <- colorFactor(pal = c('red', 'yellow', 'blue', 'orange', 'green', 'pink'), 
                          levels = Pillar_levels)


# sidebar -----------------------------------------------------------
sidebar <- dashboardSidebar(
  sidebarMenu(
      id = "tabs",
      menuItem(
        tabName = "overview",
        text = "Project Overview",
        icon = icon("info circle")),
      menuItem(
        tabName = "intro",
        text = "Introduction to Loudoun",
        icon = icon("database")) ,
      menuItem(
        tabName = "data",
        text = "Data and Methodology",
        icon = icon("database")) ,
      menuItem(
        tabName = "services",
        text = "Service Availability",
        icon = icon("database")),
      menuItem(
        tabName = "locations",
        text = "Locations",
        icon = icon("database"))
  ) 
) 
# body -----------------------------------------------------------
body <- dashboardBody(
  fluidPage(
    tabItems(
      ## Tab Overview--------------------------------------------
      tabItem(tabName = "overview",
              fluidRow(
                box(
                  title = "Project Overview",
                  closable = FALSE,
                  width = NULL,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h1("Service Availability for Transitional Aged Youth in Loudoun County"),
                  h2("Project Description")
                ) 
              ) 
      ),
      
      ## Introduction to Loudoun County--------------------------------------------
      tabItem(tabName = "intro",
               fluidRow(style = "margin: 6px;",
                        h1(strong("Loudoun County Residents' Demographic Characteristics"), align = "center"),
                        h2("Project Description"),
                        br(),
                        p("", style = "padding-top:10px;"),
                        h4(strong("Who does Loudoun County Serve?")),
              p("Loudoun County is located in the northern part of the Commonwealth of Virginia 
                        in the United States. It covers 515.6 square miles ranking 20th-largest county 
                        in Virginia by area. Loudoun County, Virginia is bordered by Jefferson County, West 
                        Virginia, Fauquier County, Virginia, Fairfax County, Virginia, Prince William County,
                        Virginia, Clarke County, Virginia, Washington County, Maryland, Montgomery County, 
                        Maryland, and Frederick County, Maryland.[1] In 2019, the population was estimated at 
                        395,134, making it Virginia’s third-most populous county. Loudoun County is part of 
                        the Washington-Arlington-Alexandria, DC-VA-MD-WV Metro Area."),
              p("n 2015-2019, there were 128,637 households in Loudoun County, Virginia. 
                        The average household size was 3.06 people. In 2015-2019, there were 128,637 
                        households in Loudoun County, Virginia. The average household size was 3.06 people. 
                        In Loudoun County, Virginia, 47.7 percent of all households have one or more
                        people under the age of 18; 19.4 percent of all households have one or more people 
                        65 years and over."), 
              p("The median income of households in Loudoun County, Virginia was $142,299, 
                        the poverty rate is 3.4% in 2019. An estimated 1.6 percent of households had 
                        income below $10,000 a year and 30.0 percent had income over $200,000 or more. 
                        As of 2018, Loudoun County had a median household income of $136,268.[6] Since 2008, 
                        the county has been ranked first in the U.S. in median household income among jurisdictions
                        with a population of 65,000 or more. In 2015-2019, 3.4% of people were 
                        in poverty. An estimated 3.2% of children under 18 were below the poverty 
                        level, compared with 4.5% of people 65 years old and over. An estimated 3.3% 
                        of people 18 to 64 years were below the poverty level.")) ,
              br(),
                      box(
                        title = "Visualizations of Transitional Aged Youth (TAYs)",
                        closable = FALSE,
                        width = NULL,
                        status = "warning",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        selectInput("var1", "Select Variable:", width = "100%", choices = c(
                          "Gender and Age" = "age",
                          "Percentage of TAYs" = "percent", 
                          "Educational Attainment" = "education",
                          "Races" = "race",
                          "Poverty Level" = "poverty",
                          "Healthcare Coverage" = "health")
                        ),
                        plotlyOutput("plot1"),
                        p(tags$small("Data Source: American Community Survey 2019 1-Year Estimates."))), 
                  br(),
                  br(),
                  box(
                    title = "Visualizations of the Subpopulations Demographics",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    tabsetPanel(
                      tabPanel("Foster Care",
                               h3(strong(""), align = "center"),
                               br(),
                               selectInput("var2", "Select Topic:", width = "100%", choices = c(
                                 "Age" = "age",
                                 "Sex" = "sex", 
                                 "Race" = "race",
                                 "Ethnicity" = "eth",
                                 "TAYs" = "tays")
                               ),
                               plotlyOutput("plot2"),
                               p(tags$small("Data source: The Adoption and Foster Care Analysis and Reporting System 2019")) ,
                               br(),
                               p("In the US 2019, 423,997 children were in the foster system with 251,359 
                                 newly entered children and 248,669 exiting. The average age of a child in 
                                 foster care is 8.4 years old and males are the majority by 4%. For the Transitional 
                                 Aged Youth (18-24), they only make up about 4% of the total foster care youth in 
                                 the US. 44% of foster care youth are white and 23% were black. Similar to foster 
                                 care statistics in Virginia alone, the average time in care is 19.6 months [2]. 
                                 According to The AFCARS Report in 2019, only 3,335 (1%) children who entered 
                                 the foster care system were 18+ and it was most likely due to neglect. However, 
                                 there were 20,465 (8%) youths 18+ who exited the system most likely due to
                                 aging out and emancipation."), 
                               p(""),
                               p("According to the The Adoption and Foster Care Analysis and Reporting System, 
                                 in 2020 there were 48 children in foster care in only Loudoun County which was 
                                 .8% in the state of Virginia. As you can see in the pie chart above, over 2/3 
                                 of those children were boys and 1/3 were girls and the minority of them of 
                                 ethnicity of Hispanic. Almost 50% of those children were white, 25% black 
                                 and less than 5% Asian and multi-racial as you can see from the barplot below. 
                                 When we are looking at only transitional aged youth from 18-24 where 21 years 
                                 old is the average time a foster child ages out, there were only 8 children. 
                                 In Loudoun County, it does not seem like there are many foster care youths 
                                 who are aging out of the system but only 9 other counties have greater than 9 
                                 foster care kids over the age of 18 ")
                      
                      ),
                      tabPanel("Juvenille Detention",
                               h3(strong(""), align = "center"),
                               br(),
                               selectInput("var3", "Select Topic:", width = "100%", choices = c(
                                 "Age" = "age",
                                 "Sex" = "sex",
                                 "Race" = "race",
                                 "Ethnicity" = "eth")
                               ),
                               plotlyOutput("plot3"),
                               p(tags$small("Data source: DJJ (Department of Juvenile Justice)")),
                               br(),
                               p("Youth incarceration in Virginia is run by the Virginia Department of Juvenile Justice (DJJ) 
                                 and is split between juvenile detention centers (JDCs), group homes, and youth prisons. 
                                 Black youth are overrepresented among offenders in residential placement, making up 
                                 40.9% of residents, as compared to 33.3%, 20.3%, .98% and 2.1% for whites, hispanics, 
                                 asians and native americans, respectively (OJJDP 2019). In regards to sex, males make up 
                                 the vast majority of offenders in residential placement, at 85.2% of residents. 
                                 Broken down by age, those aged 16-17 years made up the bulk of residents, accounting for 
                                 52.4% of them (OJJDP 2019). With respect to mental health, about 73% of all youth entering 
                                 youth prisons demonstrated significant symptoms of mental disorder and more than 94.9% of
                                 youth who entered Virginia youth detainment facilities showed some symptoms of Attention 
                                 Deficit Hyperactivity Disorder (ADHD), Conduct Disorder (CD), Oppositional Defiant Disorder 
                                 (ODD), Substance Abuse, or Substance Dependence (DJJ 2020)."), 
                               p(""),
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
                               
                               
                               )
                     
                  
                    )
                  )
                
              ),
      
      
      ## Data and Methodology--------------------------------------------
      tabItem(tabName = "data",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Data and Methodology"), align = "center"),
                       h2("Sources"),
                       br(),
                       p("", style = "padding-top:10px;"),
                       h4(strong("")),
                       p("We examined Patrick County population sociodemographic and socioeconomic characteristics to better understand the
                                            residents that the county serves."),
                       p("We retrieved American Community Survey (ACS) data to calculate this information at census block group and census
                                            tract levels. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets. We used
                                            the most recently available 5-year estimates from 2014/18 to compute percent Patrick County residents in a given block group or tract by age, race, ethnicity,
                                            employment, health insurance coverage, and other relevant characteristics."),
                       p("Our interactive plots visualize census block-group level sociodemographic characteristics of Patrick County residents.")),
              br(), 
              br()
              
      ),
      
      
      ## Services--------------------------------------------
      tabItem(tabName = "services", 
              fluidRow(
                box(
                  title = "Service Availability",
                  closable = FALSE,
                  width = NULL,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h1("Service Availability for Transitional Aged Youth in Loudoun County"),
                  h2("Project Description"),
                  p("we need to add description of what we are doing and why we are doing this"), 
                  p("Talk about .... ")) , 
                  br(),
                  box(
                    title = "Loudoun County",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h2("Project Description"),
                    selectInput("pillar1", "Select Pillar:", width = "100%", choices = c(
                      "Education",
                      "Employment",
                      "Housing",
                      "Transportation",
                      "Insurance",
                      "Policy and Funding")), 
                    collapsibleTreeOutput("tree1")), 
                  box(
                    title = "Allegheny County",
                    closable = FALSE,
                    width = NULL,
                    status = "warning",
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    h2("Project Description"),
                    h3(strong(""), align = "center"),
                    selectInput("pillar2", "Select Pillar:", width = "100%", choices = c(
                      "Education",
                      "Employment",
                      "Housing",
                      "Transportation",
                      "Insurance",
                      "Policy and Funding")
                    ),
                    collapsibleTreeOutput("tree2")) 
                  
                ) 
             
      ), 
      ## Locations --------------------------------------------
      tabItem(tabName = "locations", 
               fluidRow(style = "margin: 6px;",
                        h1(strong("Location and accessibility of Programs and Services"), align = "center"),
                        box(
                          title = "Loudoun County",
                          closable = FALSE,
                          width = NULL,
                          status = "warning",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                           p("Internet connection and computing devices are key for access to health information and participation in online health-related services like
                                         telemedicine. Rural areas frequently lack broadband access, experience low internet speeds, and have fewer internet providers available
                                         than urban areas. It is crucial to consider digital connectivity in improving health care access. We examined digital connectivity in Patrick County in two ways to
                                         provide the county with insights on where increasing connectivity would facilitate communicating health information and improve online health service access."),
                           p("We first examined access to computing devices and internet connection types in Patrick County. We used American Community Survey (ACS) data to
                                        obtain this information at census block group level. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households
                                        to compile 1-year and 5-year estimates of population sociodemographic and socioeconomic characteristics. We used the most
                                        recently available 5-year data from 2014/18 to calculate the percentage of the Patrick County residents with access to devices
                                        and internet by census block group."),
                           br(), 
                           tabsetPanel(
                             tabPanel("Subpopulation",
                                      p(""),
                                      p(strong("Map of Programs")), 
                                      leafletOutput("map1")
                             ),
                             tabPanel("Pillars",
                                      p(""),
                                      p(strong("Map of Programs")), 
                                      leafletOutput("map2")
                             )
                           )
                           ) , 
                           br(),
                        br(), 
                        box(
                          title = "Allegheny County",
                          closable = FALSE,
                          width = NULL,
                          status = "warning",
                          solidHeader = TRUE,
                          collapsible = TRUE,
                           h4(strong("Allegheny County")),
                           ## description of what we are doin and why we are mapping them out 
                           p("Internet connection and computing devices are key for access to health information and participation in online health-related services like
                                         telemedicine. Rural areas frequently lack broadband access, experience low internet speeds, and have fewer internet providers available
                                         than urban areas. It is crucial to consider digital connectivity in improving health care access. We examined digital connectivity in Patrick County in two ways to
                                         provide the county with insights on where increasing connectivity would facilitate communicating health information and improve online health service access."),
                           p("We first examined access to computing devices and internet connection types in Patrick County. We used American Community Survey (ACS) data to
                                        obtain this information at census block group level. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households
                                        to compile 1-year and 5-year estimates of population sociodemographic and socioeconomic characteristics. We used the most
                                        recently available 5-year data from 2014/18 to calculate the percentage of the Patrick County residents with access to devices
                                        and internet by census block group."),
                           br(), 
                           tabsetPanel(
                             tabPanel("Subpopulation",
                                      p(""),
                                      p(strong("Map of Programs")),
                                      leafletOutput("map3")
                             ),
                             tabPanel("Pillars",
                                      p(""),
                                      p(strong("Map of Programs")),
                                      leafletOutput("map4")
                             )
                           )
                      )
               
                  )
              ) 
               
          )
      ) 
  ) 
    
  
ui <- dashboardPage(
    dashboardHeader(title = "Service Availability for Transitional Aged Youth in Loudoun County"), 
    sidebar = sidebar, 
    body = body
)
   
server <- function(input, output, session) {
    
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
        
        
        ggplot(l_ages_gender, aes(fill = gender, x = estimate, y = variable)) + 
          geom_bar(position="dodge", stat="identity") + 
          labs(title = "Gender and Age Groups ", 
               y = "", 
               x = "Population Estimate") +coord_flip()
        
        
      } else if(var1() == "percent") {
        
        m_p <- sum(l_ages_gender$estimate[1:4])/195769 * 100 
        f_p <-sum(l_ages_gender$estimate[5:8])/199365 * 100 
        
        percent <- data.frame(rbind(m_p, f_p))
        colnames(percent) <- "Percent"
        percent$Gender <- c("Male", "Female")
        
        
        percent %>%
          ggplot() + geom_col(mapping = aes(x = Percent, y = Gender ), fill = "darkgreen")+
          labs(title = "Percent of TAYs by Gender", 
               y = "", 
               x = "Percent %") + coord_flip() 
       
      }else if(var1() == "race"){
        race %>%
          ggplot() + geom_col(mapping = aes(x = Estimate, y = Race ), fill = "lightblue")+ 
          labs(title = "Racial Demographics of TAYs", 
               y = "Races", 
               x = "Population Estimate") 
      } 
      else if (var1() == "education")  { 
        
        both <- data.frame(medu, fedu)%>%
          mutate(sum = estimate+estimate.1)%>%
          select(variable, sum)
        
        both$variable <- factor(both$variable, levels=unique(both$variable))
        
        both %>%
          ggplot() + geom_col(mapping = aes(x = sum, y = variable ), fill = "indianred1")+ 
          labs(title = "Educational Attainment of TAYs", 
               y = "", 
               x = "Population Estimate") 
        
        
      }
      else if (var1() == "health"){
        healthcare %>%
          ggplot(mapping = aes(x = Estimate, y = Type ))  + geom_col(fill = "gold2")+ 
          labs(title = "Types of Health Care Coverage of TAYs in Loudoun", 
               y = "", 
               x = "Population Estimate") + 
          coord_flip() + 
          theme(legend.position = "none")
      }
      else {
        pov <- rbind(w_p, b_p, i_p, as_p, n_p, o_p)
        pov %>%
          ggplot() + geom_col(mapping = aes(x = sum, y = variable ), fill = "plum2")+ 
          labs(title = "Poverty by Race of TAYs", 
               y = "", 
               x = "Population Estimate")+coord_flip()

      }
    
    })  
    
    
    var2 <- reactive({
      input$var2
    })
    ##Render Plot for Foster Care 
    output$plot2 <- renderPlotly({
      if(var2() == "age") {
        fc_ages$Age.Group <- factor(fc_ages$Age.Group, levels=unique(fc_ages$Age.Group))
        
        ggplot(data = fc_ages, aes(x= Age.Group, y = Value, fill = Age.Group) ) + 
          geom_col(width = .95, alpha = .75) + coord_flip()+
          theme_minimal(base_family = 'Verdana' ) + 
          labs(x = "", 
               y = "Population Estimate",
               title = "Age Groups for Foster Care", 
               fill ="") + theme(legend.position = "none") 
        
      }else if(var2() == "race"){
        ggplot(data = fc_races, aes(x= Race, y = Value, fill = Race) ) + 
          geom_col(width = .95, alpha = .75) + coord_flip()+
          theme_minimal(base_family = 'Verdana') + 
          labs(x = "", 
               y = "Population Estimate",
               title = "Racial Demographics for Foster Care", 
               fill ="") + theme(legend.position = "none")
        
    } else if (var2() == "eth") {
      g <- ggplot(fc_eth, aes(eth, value, fill = eth))
      g + geom_bar(stat = "identity") + 
        labs(title = "Ethinic Demographics of Foster children",
             x = "Ethnicity", 
             y = "Population Estimate") + theme(legend.position = "none")
        
    }else if(var2() =="sex") {
      ggplot(fc_sex, aes(x = Gender, y = Value, fill = Gender)) + 
        geom_bar(stat="identity") + 
        labs(x = "" , y = "Population Estimate", 
             title = "Sex of Youths in Foster Care") + theme(legend.position = "none")
    }else{
        ggplot(fc_tays, aes(x = age_19, y = value, fill = age_19)) + 
          geom_bar(stat="identity") + 
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
          geom_col(fill = "brown1") +
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
          geom_col(fill = "coral") +
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
          geom_col(fill = "darkseagreen2") +
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
          geom_col(fill = "darkslategray2" ) +
          labs(x = "Sex", y = "Relative Frequency",
               title = "Sex Demographics of Loudoun Intakes") + 
          theme_minimal() + 
          theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          coord_flip()
        
        
      }
      
    }) 
    
    
    ## Tree for loudoun
    output$tree1 <- renderCollapsibleTree({
      if(input$pillar1%in%"Education"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Education")%>% 
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          fill = "Category", 
                          width=1800,
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')
      }else if(input$pillar1%in%"Employment"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Employment")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          fill = "Category", 
                          width=1800,
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar1%in%"Housing"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Housing")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          fill = "Category", 
                          width=1800,
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar1%in%"Transportation"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Transportation")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          fill = "Category", 
                          width=1800,
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar1%in%"Insurance"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Insurance")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          fill = "Category", 
                          width=1800,
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')

      }else {
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Funding and Policy")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          fill = "Category", 
                          width=1800,
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')

      }
    })

    ## tree for Allegheny County 
    output$tree2 <- renderCollapsibleTree({
      if(input$pillar2%in%"Education"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Education")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          fill = "Category", 
                          width=1800,
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')
      }else if(input$pillar2%in%"Employment"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Employment")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          fill = "Category", 
                          width=1800,
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar2%in%"Housing"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Housing")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          fill = "Category", 
                          width=1800,
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar2%in%"Transportation"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Transportation")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          fill = "Category", 
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar2%in%"Insurance"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Insurance")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          fill = "Category", 
                          width=1800,
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')

      }else {
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Funding & Policy")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          fill = "Category", 
                          width = 1800,
                          zoomable=F, 
                          collapsed = T, nodeSize = 'leafCount')

      }
    })
    
    # Add maps for locations of programs in Loudoun subpopulation 
    output$map1 <- renderLeaflet({
      
      a_sub <- loudoun_locations %>% 
        leaflet( options = leafletOptions(minzoom = 12)) %>%
        setView(lng = -77.531622, lat = 38, zoom = 7) %>% 
        addProviderTiles("CartoDB") %>%
        addCircleMarkers(lng = ~Longitude,
                         lat = ~Latitude,
                         popup = ~paste0("<b>", loudoun_locations$Program, "</b>", "<br/>", "<b>", "Qualifications: ", "</b>", 
                                         loudoun_locations$Qualification, "<br/>","<b>","Description: ", "</b>", 
                                         loudoun_locations$Description, "<br/>","<b>","Website: ", "</b>", "<a>",
                                         loudoun_locations$Website, "</a>"),
                         group = ~loudoun_locations$Subpopulation, radius = 6, color = ~subpop_pal(Subpopulation)) %>%
        addLayersControl(overlayGroups = c("Foster Care", "Juvenile Detention"),
                         options = layersControlOptions(collapsed = FALSE))
      a_sub


    })
    
    
    # Pillars Loudoun
    output$map2 <- renderLeaflet({
      
      l_pill <- loudoun_locations %>%  
        leaflet(options = leafletOptions(minzoom = 12)) %>% 
        setView(lng = -77.531622, lat = 38, zoom = 7) %>% 
        addProviderTiles("CartoDB") %>% 
        addCircleMarkers(lng = ~Longitude, 
                         lat = ~Latitude, 
                         popup = ~paste0("<b>", loudoun_locations$Program, "</b>", "<br/>", "<b>", "Qualifications: ", "</b>", 
                                         loudoun_locations$Qualification, "<br/>","<b>","Description: ", "</b>", 
                                         loudoun_locations$Description, "<br/>","<b>","Website: ", "</b>", "<a>",
                                         loudoun_locations$Website, "</a>"),
                         radius = 6, 
                         group = ~loudoun_locations$Pillars, 
                         color = ~Pillar_pal(Pillars)) %>%  
        addLayersControl(position = "topright",
                         overlayGroups = Pillar_levels, 
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(title = "Service Type", position = "topleft", pal = Pillar_pal, values = Pillar_levels)
      
      l_pill
    })
    
    
    ## map for locations of program in Allegheny
    output$map3 <- renderLeaflet({
      
      a_sub <- allegheny_locations %>% 
        leaflet( options = leafletOptions(minzoom = 12)) %>%
        setView(lng = -79.997030, lat = 40.5, zoom = 10) %>% 
        addProviderTiles("CartoDB") %>%
        addCircleMarkers(lng = ~Longitude,
                         lat = ~Latitude,
                         popup = ~paste0("<b>", allegheny_locations$Program, "</b>", "<br/>", "<b>", "Qualifications: ", "</b>", 
                                         allegheny_locations$Qualification, "<br/>","<b>","Description: ", "</b>", 
                                         allegheny_locations$Description, "<br/>","<b>","Website: ", "</b>", "<a>",
                                         allegheny_locations$Website, "</a>"),
                         group = ~allegheny_locations$Subpopulation, radius = 6, color = ~subpop_pal(Subpopulation)) %>%
        addLayersControl(overlayGroups = c("Foster Care", "Juvenile Detention"),
                         options = layersControlOptions(collapsed = FALSE))
      a_sub
      
      
    })
    
    
    ## Pillars Allegheny 
    output$map4 <- renderLeaflet({
      
      a_pill <- allegheny_locations %>%  
      leaflet(options = leafletOptions(minzoom = 12)) %>% 
        setView(lng = -79.997030, lat = 40.5, zoom = 10) %>% 
        addProviderTiles("CartoDB") %>% 
        addCircleMarkers(lng = ~Longitude, 
                         lat = ~Latitude, 
                         popup = ~paste0("<b>", allegheny_locations$Program, "</b>", "<br/>", "<b>", "Qualifications: ", "</b>", 
                                         allegheny_locations$Qualification, "<br/>","<b>","Description: ", "</b>", 
                                         allegheny_locations$Description, "<br/>","<b>","Website: ", "</b>", "<a>",
                                         allegheny_locations$Website, "</a>"),
                         radius = 6, 
                         group = ~allegheny_locations$Pillars, 
                         color = ~Pillar_pal(Pillars)) %>%  
        addLayersControl(position = "topright",
                         overlayGroups = Pillar_levels, 
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(title = "Service Type", position = "topleft", pal = Pillar_pal, values = Pillar_levels)
      
      a_pill
      

    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
