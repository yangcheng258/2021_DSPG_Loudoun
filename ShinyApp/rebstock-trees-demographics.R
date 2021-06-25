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
fc_virginia<- read_excel("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/data/foster-care-2020.xlsx")
fc_2020 <- read_excel("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/data/foster-care-2020-all.xlsx")
#Age
totals <- t(data.frame(fc_virginia[c(3,143),])) 

colnames(totals) <- c("Age Group", "Value")
groups <- c("<1", "1-5", "6-9", "10-12", "13-15", "16-18", "19+")
fc_ages <-data.frame(totals[c(4,6,8,10,12,14,16),])

## must convert characters to numeric values 
fc_ages$Value <- as.numeric(fc_ages$Value)
#Race and ethnicity 
fc_races <- data.frame(fc_2020[c(8,10,12,14,16,18),])
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
fc_sex <- data.frame(fc_2020[c(2,4),])
colnames(fc_sex) <- c("Gender", "Value")
# Juvenille Detention -----------------------------------------------------------
# Race 
intake_race <- read.csv(file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/data/DJJ-2020-Juvenile_Detention_Locality-Race_Intake.csv", header = T)
intake_race_FY2020 <- intake_race$FY20..
loudoun_intake_race_FY2020 <- intake_race_FY2020[285:288]

#Eth 
intake_ethnicity <- read.csv(file = "/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/data/DJJ-2020-Juvenile_Detention_Locality-Ethnicity_Intake.csv", header = TRUE)
intake_ethnicity_FY2020 <- intake_ethnicity$FY20..
loudoun_intake_ethnicity_FY2020 <- intake_ethnicity_FY2020[214:216]

# Sex
intake_sex <- read.csv("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/data/DJJ-2020-Juvenile_Detention_Locality-Sex_Intake.csv")
intake_sex_2020 <- intake_sex$FY20..
loudoun_intake_sex_FY2020 <- intake_sex_2020[143:144]
sex <- c("Female", "Male")
# Age
intake_age <- read.csv("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/data/DJJ-2020-Juvenile_Detention_Locality-Age_Intake.csv")
intake_age_2020 <- intake_age$FY20..
loudoun_intake_age_FY2020 <- intake_age_2020[640:647]
ages <- c("8-12", "13", "14", "15", "16", "17", "18-20", "Missing")


# Trees -----------------------------------------------------------
Tree <- read_excel("/Users/julierebstock/Desktop/Virginia-Tech/DSPG-2021/combined-programs.xlsx")

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
      tabItem("intro",
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
                      title = "Visualizations of Loudoun Residents ",
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
                             "Ethnicity" = "eth")) 
                           # ) ,
                           # plotOutput(plot3),
                           # p(tags$small("Data source: DJJ (Department of Juvenile Justice)")) 
                )
              )
              )
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
                  p("Talk about .... "), 
                  br(),
                  tabsetPanel(
                    tabPanel("Loudoun County",
                             h3(strong(""), align = "center"),
                             selectInput("pillar1", "Select Pillar:", width = "100%", choices = c(
                               "Education",
                               "Employment",
                               "Housing",
                               "Transportation",
                               "Insurance",
                               "Policy and Funding")
                             ), 
                             collapsibleTreeOutput("tree1")

                    ),
                    tabPanel("Allegheny County",
                             h3(strong(""), align = "center"),
                             selectInput("pillar2", "Select Pillar:", width = "100%", choices = c(
                               "Education",
                               "Employment",
                               "Housing",
                               "Transportation",
                               "Insurance",
                               "Policy and Funding")
                             ),
                             collapsibleTreeOutput("tree2")
                             
                    )
                ) 
              ) 
      )) , 
      ## Locations --------------------------------------------
      tabItem(tabName = "locations",
              fluidRow(
                box(
                  title = "Locations of Services",
                  closable = FALSE,
                  width = NULL,
                  status = "warning",
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  h1("Where are the services located?"),
                  h2("Project Description"), 
                  
                  ## second example of how we can do the trees 
                  selectInput("county", "Select Variable:", width = "100%", choices = c(
                    "Loudoun County",
                    "Allegheny County")
                  ),
                  collapsibleTreeOutput("exTree"),
                  p(tags$small("Data Source: American Community Survey 2019 1-Year Estimates."))
                  ) 
                ) 
              ) 
      
      
      
      ## Data and Methodology--------------------------------------------
      
               
        )
      ) 
  ) 
    
  
      


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Julie Rebstock"), 
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
               title = "Age, Race, Sex for Loudoun County", 
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
        ggplot(loudoun_race_income, aes(x = variable, y = summary_est, color = summary_est)) + 
          geom_quasirandom(alpha = .5) + 
          coord_flip() +
          theme_minimal() + 
          scale_color_viridis_c(guide =  FALSE) + 
          scale_y_continuous(labels = scales::dollar) + 
          labs(x = "" , y = "Median Hosuehold Income", 
               title = "Household income distribution by largest ethnic/race group", 
               subtitle = "Census tracts, Loudoun County") + 
          theme(plot.title = element_text(color = "black", size = 7, face = "bold", hjust = 0.5), 
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
               title = "Races for Foster Care", 
               fill ="") + theme(legend.position = "none")
        
    } else if (var2() == "eth") {
      g <- ggplot(fc_eth, aes(eth, value, fill = eth))
      g + geom_bar(stat = "identity") + 
        labs(title = "Ethinicity of Foster children",
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
               title = "Transitional Aged Youth vs Children in Foster Care") + theme(legend.position = "none")}
      
    }) 
    
    
    var3 <- reactive({
      input$var3
    })
    ##Render Plot for Juvenille Detention 
    output$plot3 <- renderPlotly({
      if(var3() == "age") {
        # barplot(height = loudoun_intake_age_FY2020, names.arg = ages, col = "brown1", xlab = "Age",
        #         main = "Age Demographics of Loudoun Intakes",
        #         ylab = "Relative Frequency", ylim = c(0, 0.35)) 
        
      }else if(var3() == "race"){
        # barplot(height = loudoun_intake_race_FY2020, names.arg = c("Asian", "Black", "White", "Other/Unlisted"), col = "coral", ylim = c(0,0.8), xlab = "Race", ylab = "Relative Frequency",
        #         main = "Racial Demographics of Loudoun Intakes")
        
      } else if (var3() == "eth") {
        # barplot(height = loudoun_intake_ethnicity_FY2020, names = c("Hispanic", "Non-Hispanic", "Unkown/Missing"), col = "darkseagreen2", xlab = "Ethnicity",
        #         main = "Ethnic Demographics of Loudoun Intakes",
        #         ylab = "Relative Frequency", ylim = c(0, 0.8))
        
      }
      else{
        # barplot(height = loudoun_intake_sex_FY2020, names.arg = sex, col = c("darkslategray2", "firebrick2"), xlab = "Sex",
        #         main = "Sex Demographics of Loudoun Intakes",
        #         ylab = "Relative Frequency", ylim = c(0, 0.9))
        
        
      }
      
    }) 
    
    
    ## Tree for loudoun
    output$tree1 <- renderCollapsibleTree({
      if(input$pillar1%in%"Education"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Education")%>% 
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T, 
                          collapsed = T, nodeSize = 'leafCount')
      }


      else if(input$pillar1%in%"Employment"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Employment")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }
      else if(input$pillar1%in%"Housing"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Housing")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }
      else if(input$pillar1%in%"Transportation"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Transportation")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }
      else if(input$pillar1%in%"Insurance"){
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Insurance")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }
      else {
        Tree%>%filter(County == "Loudoun")%>%
          filter(Pillars == "Funding and Policy")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }
    })


    output$tree2 <- renderCollapsibleTree({
      if(input$pillar2%in%"Education"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Education")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')
      }


      else if(input$pillar2%in%"Employment"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Employment")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }
      else if(input$pillar2%in%"Housing"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Housing")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }
      else if(input$pillar2%in%"Transportation"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Transportation")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }
      else if(input$pillar2%in%"Insurance"){
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Insurance")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }
      else {
        Tree%>%filter(County == "Allegheny")%>%
          filter(Pillars == "Funding & Policy")%>%
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("County","Pillars","Subpopulation", "Program", "Age_range"),
                          root="County",
                          attribute = "County",
                          width=1800,
                          zoomable=F, fillByLevel = T,
                          collapsed = T, nodeSize = 'leafCount')

      }
    })
    
    ## example of another option 
    output$exTree <- renderCollapsibleTree({
      if(input$county%in%"Loudoun County"){
        Tree%>%
          filter(County == "Loudoun")%>% 
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Subpopulation","Pillars","Program", "Age_range"),
                          root="County",
                          attribute = "Pillars",
                          width=1800,
                          zoomable=F, fillByLevel = T, 
                          collapsed = T, nodeSize = 'leafCount')
      }
      else {
        Tree%>%
          filter(County == "Allegheny")%>% 
          group_by(Pillars)%>%
          collapsibleTree(hierarchy = c("Subpopulation","Pillars","Program", "Age_range"),
                          root="County",
                          attribute = "Pillars",
                          width=1800,
                          zoomable=F, fillByLevel = T, 
                          collapsed = T, nodeSize = 'leafCount')

      }
    })
    
}

##DT table is fancier 
## tableOutput is less fancier 

# Run the application 
shinyApp(ui = ui, server = server)
