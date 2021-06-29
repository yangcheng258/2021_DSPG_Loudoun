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


# Loudoun -----------------------------------------------------------
## sex and age
loudoun <- get_estimates(geography = 'county', state = 'VA', county = 'Loudoun', product = 'characteristics', breakdown = c('SEX', 'AGEGROUP'),
                         breakdown_labels = TRUE,year = 2019)

filtered <- filter(loudoun, str_detect(AGEGROUP, '^Age' ), 
                   SEX != 'Both sexes') %>% 
  mutate(value = ifelse(SEX == 'Male', -value, value))
# race
races <- c(White = "B03002_003", Black = "B03002_004",Native = "B03002_005", Asian = "B03002_006", HIPI = "B03002_007", Hispanic = "B03002_012")
lr <- get_acs(
  geography = 'county', 
  variables = races, 
  state= 'VA',
  county='Loudoun', 
  summary_var = 'B03002_001'
)

## income 
income <- get_acs(geography = "tract", state = "VA", county = "Loudoun", variables = c(White = "B03002_003", Black = "B03002_004",
                 Asian = "B03002_006",Hispanic = "B03002_012"),summary_var = "B19013_001") %>%
  group_by(GEOID) %>%filter(estimate== max(estimate, na.rm = TRUE)) %>% ungroup() %>%filter(estimate != 0)
# education 
schools <- c("Total","No school",  "Nursery School","Kindergarten",  "1st Grade" ,  "2nd Grade",
             "3rd Grade" ,"4th Grade",  "5th Grade",  "6th Grade" , "7th Grade", "8th Grade" ,
             "9th Grade", "10th Grade",  "11th Grade" ,"12th Grade",  "High School Diploma", 
             "GED or alternative credential",  "Some college, less than 1 year", 
             "Some college, 1 or more years, no degree",  "Associate's degree", "Bachelor's degree", 
             "Master's degree" ,  "Professional school degree", "Doctorate degree" 
) 

education <- get_acs(geography = 'county', table = "B15003",year = 2019,  state = 'VA',county = 'loudoun', summary_var = 'B15003_001')

#Languages 
lang <- c("Spanish", 'Other Indo-European languages', "Asian and Pacific Islander languages","Other languages")
lang_p <- c(10.9,9.3,9.2,2.2)
lang_df <- data_frame(lang, lang_p)


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

Pillar_levels <- unique(allegheny_locations$Pillars)
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
                        p("We examined Patrick County population sociodemographic and socioeconomic characteristics to better understand the
                                            residents that the county serves."),
                        p("We retrieved American Community Survey (ACS) data to calculate this information at census block group and census
                                            tract levels. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets. We used
                                            the most recently available 5-year estimates from 2014/18 to compute percent Patrick County residents in a given block group or tract by age, race, ethnicity,
                                            employment, health insurance coverage, and other relevant characteristics."),
                        p("Our interactive plots visualize census block-group level sociodemographic characteristics of Patrick County residents.")),
                        br(), 
                        br(),  
                      box(
                        title = "Visualizations of Loudoun Residents",
                        closable = FALSE,
                        width = NULL,
                        status = "warning",
                        solidHeader = TRUE,
                        collapsible = TRUE,
                        selectInput("var1", "Select Variable:", width = "100%", choices = c(
                          "Age, Sex" = "ageSex",
                          "Race" = "race", 
                          "Household Income by Ethnicity" = "income",
                          "Languages Spoken" = "language",
                          "Level of Education" = "education")
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
                               selectInput("var2", "Select Topic:", width = "100%", choices = c(
                                 "Age" = "age",
                                 "Sex" = "sex", 
                                 "Race" = "race",
                                 "Ethnicity" = "eth",
                                 "TAYs" = "tays")
                               ),
                               plotlyOutput("plot2"),
                               p(tags$small("Data source: The Adoption and Foster Care Analysis and Reporting System 2019")) 
                      
                      ),
                      tabPanel("Juvenille Detention",
                               h3(strong(""), align = "center"),
                               selectInput("var3", "Select Topic:", width = "100%", choices = c(
                                 "Age" = "age",
                                 "Sex" = "sex",
                                 "Race" = "race",
                                 "Ethnicity" = "eth")
                               ),
                               plotlyOutput("plot3"),
                               p(tags$small("Data source: DJJ (Department of Juvenile Justice)")))
                  
                    )
                  )
                
              ),
      
      
      ## Data and Methodology--------------------------------------------
      tabItem(tabName = "data",
              fluidRow(style = "margin: 6px;",
                       h1(strong("Loudoun County Residents' Demographic Characteristics"), align = "center"),
                       h2("Project Description"),
                       br(),
                       p("", style = "padding-top:10px;"),
                       h4(strong("Who does Loudoun County Serve?")),
                       p("We examined Patrick County population sociodemographic and socioeconomic characteristics to better understand the
                                            residents that the county serves."),
                       p("We retrieved American Community Survey (ACS) data to calculate this information at census block group and census
                                            tract levels. ACS is an ongoing yearly survey conducted by the U.S Census Bureau that samples households to compile 1-year and 5-year datasets. We used
                                            the most recently available 5-year estimates from 2014/18 to compute percent Patrick County residents in a given block group or tract by age, race, ethnicity,
                                            employment, health insurance coverage, and other relevant characteristics."),
                       p("Our interactive plots visualize census block-group level sociodemographic characteristics of Patrick County residents.")),
              br(), 
              br(),  
              box(
                title = "Visualizations of Loudoun Residents",
                closable = FALSE,
                width = NULL,
                status = "warning",
                solidHeader = TRUE,
                collapsible = TRUE, 
                leafletOutput("map1"),
                p(tags$small("Data Source: American Community Survey 2019 1-Year Estimates."))), 
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
                        column(6,
                               h4(strong("Loudoun County")),
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
                                 tabPanel("Pillars",
                                          p(""),
                                          p(strong("Map of Programs")),
                                          leafletOutput("map1")
                                 ),
                                 tabPanel("Subpopulation",
                                          p(""),
                                          p(strong("Map of Programs")),
                                          leafletOutput("map2")
                                 )
                               )
                               ) , 
                               br(),
                        column(6,
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
                                 tabPanel("Pillars",
                                          p(""),
                                          p(strong("Map of Programs")),
                                          leafletOutput("map3")
                                 ),
                                 tabPanel("Subpopulation",
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
    dashboardHeader(title = "DSPG 2021"), 
    sidebar = sidebar, 
    body = body
)
   
server <- function(input, output, session) {
    
    var1 <- reactive({
      input$var1
    })
    output$plot1 <- renderPlotly({
      if(var1() == "ageSex") {
        ggplot(filtered, aes(x = value, y = AGEGROUP, fill = SEX)) + geom_col(width = .95, alpha = .75) + 
          theme_minimal(base_family = 'Verdana' ) + 
          scale_x_continuous(labels = function(y) paste0(abs(y)/1000, "k")) + 
          scale_y_discrete(labels = function(x) gsub("Age | years", " ", x)) +
          scale_fill_manual(values = c("darkred", "navy")) +
          labs(x = "", 
               y = "Population Estimates",
               title = "Age and Sex Pyramid Structure for Loudoun County", 
               fill ="")
      } else if(var1() == "race") {
        ggplot(data = lr, aes(x = estimate, y = variable, fill = variable)) + 
          geom_bar(stat = "identity") + 
          labs(title = "Races in Loudoun County", 
               x = "Population estimate", 
               y = "Race", 
               caption = "Data source: American Community Survey")+ 
          theme_minimal(base_family = "Verdana")+ theme(legend.title = element_blank()) 
       
      }else if(var1() == "income"){
        ggplot(income, aes(x = variable, y = summary_est, color = summary_est)) + 
          geom_quasirandom(alpha = .5) + 
          coord_flip() +
          theme_minimal() + 
          scale_color_viridis_c(guide =  FALSE) + 
          scale_y_continuous(labels = scales::dollar) + 
          labs(x = "" , y = "Median Hosuehold Income", 
               title = "Household income distribution by largest ethnic/race group", 
               subtitle = "Census tracts, Loudoun County") + 
          theme(plot.title = element_text(color = "black", size = 9, face = "bold", hjust = 0.5), 
                axis.title.x = element_text(size = 5))
      } 
      else if (var1() == "education")  { 
        rownames(education) <- schools
        education$variable <- rownames(education)
        education$variable <- factor(education$variable, levels=unique(education$variable))
        
        education <- education[-1,]
        
        ggplot(data = education, aes(x=estimate, y=variable, fill = "red")) + geom_bar(stat = "identity") +
          labs(title =  "Education in Loudoun County",
               y = "Level of Education",
               x = "Population") + 
          theme(legend.position = "none", plot.title = element_text(color = "black", size = 7, face = "bold", hjust = 0.5))
        
        
      }
      else {
        ggplot(data = lang_df, aes(x = lang, y = lang_p, fill = lang)) + 
          geom_bar(stat = "identity") + coord_flip() + 
          labs(title  ="Percent of the Population 5+ who Speak a Language other than English",
               x = "Language",
               y = "Percentage") + 
          theme(legend.position = "none", plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5))

      }
    
    })  
    
    
    var2 <- reactive({
      input$var2
    })
    ##Render Plot for Foster Care 
    output$plot2 <- renderPlotly({
      if(var2() == "age") {
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

        jv_age %>% 
          ggplot(aes(x = Age, y = Proportion)) +
          geom_col(fill = "brown1") +
          labs(x = "Age", y = "Quantity",
               title = "Age Groups of Loudoun Intakes") + 
          theme_minimal() + 
          theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          coord_flip()
        }
      else if(var3() == "race"){
        jv_race$Race <- factor(jv_race$Race, levels=unique(jv_race$Race))

        jv_race %>% 
          ggplot(aes(x = Race, y = Proportion)) +
          geom_col(fill = "coral") +
          labs(x = "Race", y = "Quantity",
               title = "Racial Demographics of Loudoun Intakes") + 
          theme_minimal() + 
          theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          coord_flip() 

      }
      else if (var3() == "eth") {
        jv_eth$Ethnicity <- factor(jv_eth$Ethnicity, levels=unique(jv_eth$Ethnicity))

        jv_eth %>% 
          ggplot(aes(x = Ethnicity, y = Proportion))  +
          geom_col(fill = "darkseagreen2") +
          labs(x = "Ethnicity", y = "Relative Frequency",
               title = "Ethnic Demographics of Loudoun Intakes") + 
          theme_minimal() + 
          theme(legend.position = "none", panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
          coord_flip()
        
      }
      else{
        jv_sex$Sex <- factor(jv_sex$Sex, levels=unique(jv_sex$Sex))

        jv_sex %>% 
          ggplot(aes(x = Sex, y = Proportion)) +
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
                          width=1800,
                          zoomable=F, fillByLevel = T, 
                          collapsed = T, nodeSize = 'leafCount')
      }else if(input$pillar1%in%"Employment"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Employment")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar1%in%"Housing"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Housing")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar1%in%"Transportation"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Transportation")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar1%in%"Insurance"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Insurance")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }else {
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Funding and Policy")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
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
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')
      }else if(input$pillar2%in%"Employment"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Employment")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar2%in%"Housing"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Housing")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar2%in%"Transportation"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Transportation")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }else if(input$pillar2%in%"Insurance"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Insurance")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }else {
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Funding & Policy")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }
    })
    
    # Add maps for locations of programs in Loudoun subpopulation 
    output$map1 <- renderLeaflet({
      l_sub <- loudoun_locations %>% 
      leaflet(options = leafletOptions(minzoom = 12)) %>%
        setView(lng = -77.457030, lat = 38.3, zoom = 8) %>%
        addProviderTiles("CartoDB") %>%
        addCircleMarkers(lng = ~loudoun_locations$Longitude,
                         lat = ~loudoun_locations$Latitude,
                         popup = ~paste0("<b>", loudoun_locations$Program, "</b>", "<br/>", "<b>", "Qualifications: ", "</b>", 
                                         loudoun_locations$Qualification, "<br/>","<b>","Description: ", "</b>", 
                                         loudoun_locations$Description, "<br/>","<b>","Website: ", "</b>", "<a>",
                                         loudoun_locations$Website, "</a>"),
                         group = ~loudoun_locations$Subpopulation, radius = 2, color = ~subpop_pal(Subpopulation)) %>%
        addLayersControl(overlayGroups = c("Foster Care", "Juvenile Detention"),
                         options = layersControlOptions(collapsed = FALSE))
      
      l_sub 
      

    })
    
    
    ## Pillars Loudoun 
    output$map2 <- renderLeaflet({ 
      l_pill <- loudoun_locations %>%
        leaflet(options = leafletOptions(minzoom = 12)) %>% 
        setView(lng = -77.457030, lat = 38.3, zoom = 8) %>% 
        addProviderTiles("CartoDB") %>% 
        addCircleMarkers(lng = ~Longitude, 
                         lat = ~Latitude, 
                         popup = ~paste0("<b>", loudoun_locations$Program, "</b>", "<br/>", "<b>", "Qualifications: ", "</b>", 
                                         loudoun_locations$Qualification, "<br/>","<b>","Description: ", "</b>", 
                                         loudoun_locations$Description, "<br/>","<b>","Website: ", "</b>", "<a>",
                                         loudoun_locations$Website, "</a>"),
                         radius = 2, 
                         group = ~loudoun_locations$Pillars, 
                         color = ~Pillar_pal(Pillars)) %>%  
        addLayersControl(position = "bottomleft",
                         overlayGroups = Pillar_levels, 
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(title = "Service Type", position = "topleft", pal = Pillar_pal, values = Pillar_levels)
      
      l_pill
      

    })
    
    
    ## map for locations of program in Allegheny
    output$map3 <- renderLeaflet({
      a_sub <- allegheny_locations %>% 
      leaflet( options = leafletOptions(minzoom = 12)) %>%
        setView(lng = -79.857030, lat = 40.4, zoom = 10) %>% 
        addProviderTiles("CartoDB") %>%
        addCircleMarkers(lng = ~Longitude,
                         lat = ~Latitude,
                         popup = ~paste0("<b>", allegheny_locations$Program, "</b>", "<br/>", "<b>", "Qualifications: ", "</b>", 
                                         allegheny_locations$Qualification, "<br/>","<b>","Description: ", "</b>", 
                                         allegheny_locations$Description, "<br/>","<b>","Website: ", "</b>", "<a>",
                                         allegheny_locations$Website, "</a>"),
                         group = ~allegheny_locations$Subpopulation, radius = 2, color = ~subpop_pal(Subpopulation)) %>%
        addLayersControl(overlayGroups = c("Foster Care", "Juvenile Detention"),
                         options = layersControlOptions(collapsed = FALSE))
      a_sub
      
    })
    
    
    ## Pillars Allegheny 
    output$map4 <- renderLeaflet({
      a_pill <- allegheny_locations %>%  
      leaflet(options = leafletOptions(minzoom = 12)) %>% 
        setView(lng = -79.857030, lat = 40.4, zoom = 10) %>% 
        addProviderTiles("CartoDB") %>% 
        addCircleMarkers(lng = ~Longitude, 
                         lat = ~Latitude, 
                         popup = ~paste0("<b>", allegheny_locations$Program, "</b>", "<br/>", "<b>", "Qualifications: ", "</b>", 
                                         allegheny_locations$Qualification, "<br/>","<b>","Description: ", "</b>", 
                                         allegheny_locations$Description, "<br/>","<b>","Website: ", "</b>", "<a>",
                                         allegheny_locations$Website, "</a>"),
                         radius = 2, 
                         group = ~allegheny_locations$Pillars, 
                         color = ~Pillar_pal(Pillars)) %>%  
        addLayersControl(position = "bottomleft",
                         overlayGroups = Pillar_levels, 
                         options = layersControlOptions(collapsed = FALSE)) %>%
        addLegend(title = "Service Type", position = "topleft", pal = Pillar_pal, values = Pillar_levels)
      
      a_pill
      

    })
    
}


# Run the application 
shinyApp(ui = ui, server = server)
