library(shiny)
library(shinyWidgets)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(dashboardthemes)
library(readr)
library(collapsibleTree)
library(tidyverse)
library(viridis)
library(sf)
library(mapview)
library(dplyr)
library(tidycensus)
library(sp)
library(readxl)
library(tigris)
library(shinyjs)
#library(RColorBrewer)
#library(osmdata)
#library(purrr)
#library(osrm)
#library(rmapzen)
#library(rgdal)
#library(ggplot2)
#library(scales)
#library(nycflights13)

#install.packages("rsconnect")
#library(rsconnect)
#rsconnect::deployApp('~/git/WytheFinalDash/ShinyFinalPresentation', account = "wythecountydash")
#rsconnect::deployApp('path/to/your/app')



# source("theme.R")
# #Get Data










# UI ------------------------------------------------------------------ 
ui <-  dashboardPage(
    dashboardHeader(title = "Loudoun County"),
    title = "Dashboard Page",
    
    ## SIDEBAR (LEFT) ----------------------------------------------------------

    
    
    
    useShinyjs(),
    #customTheme(),
    fluidPage(
            tabItems(
                tabItem(tabName = "overview",
                        fluidRow(
                            boxPlus(
                                title = "Project Overview",
                                closable = FALSE,
                                width = NULL,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                h1("Industry and Workforce Attraction and Retention in Wythe County"),
                                h2("Project Description"),
                                p("Wythe County is a rural community in Southwest Virginia with a population of 28,684 (2019 estimate). It was founded in 1790 and sits at the confluence of two major highways, Interstates 77 and 81, which facilitate easy access to major markets and population centers along the Eastern Seaboard and in Midwestern and Southern states. In recent years, Wythe County has had success in attracting manufacturing facilities and travel-related businesses, and it has worked to expand the variety of tourism and hospitality options available. Like other communities in Appalachia, it has faced a declining population over the past decade. As a result, county leaders are seeking new ways to attract and retain companies, workers and residents. "),
                                br(),
                                boxPlus(
                                    title = "Wythe County",
                                    closable = FALSE,
                                    status = "warning",
                                    solidHeader = TRUE,
                                    collapsible = TRUE,
                                    width = NULL,
                                    #enable_dropdown = TRUE,
                                    #dropdown_icon = "",
                                    #dropdown_menu = tagList(selectInput("var","Select a Variable",choices = c("Level of Education","Industry","Home Values","Household Income","Household Size"))),
                                    leafletOutput("wythe")
                                ),
                                
                                h2("Project Goals"),
                                p("We partnered with Virginia Cooperative Extension to contextualize industry and workforce factors at levels that are actionable for stakeholders and that promote informed policy and investment in Wythe County amenities and infrastructure. We identified industries and particular jobs within those industries that are expected to grow rapidly in the future. We visualized these jobs by both the skill set and education-level necessary for vocational success. We then created measures to help stakeholders assess the ability of Wythe County and the surrounding region to train the workforce of tomorrow."),
                                
                                h2("Project Approach"),
                                p("Acemoglu and Autor (2012) demonstrate that investments in human capital have large effects on both the labor market and the economy. They build on work by Goldin and Katz (2007), who also argue that investments in human capital, particularly in the area of education, have far reaching effects on the economy, public policy, and society. Our approach to studying future industry growth potential in Wythe  focused predominantly on human capital; to that end, we examined Wythe’s comparative ability to develop its population of workers to meet  workforce needs for the jobs of tomorrow, i.e., those jobs with the most promising outlook for employment and income growth."),
                                p("In their article “Developing High Growth Businesses in Rural Areas: A study of Four States,” William and Lamb (2010) note the key characteristics that a county can use to attract high growth businesses (HGBs). We will focus on three of them: (1) having a skilled and educated workforce, (2) access to research institutions, and (3), access to broadband. We expanded their idea of “access to research institutions” to include community colleges, which provide suitable education and job skills for many high-paying occupations. Job skill enhancements can also be made through workforce training sites.  Additionally, high schools can provide important training certificates in computer science, cyber security, and so on."),
                                p("The US rural economy is diverse and changing. Increasingly, the share of workers in service jobs is overtaking traditional industries, e.g., manufacturing and agriculture (Laughlin, 2016). As a result, it is important for counties to be nimble in their approach to workforce training.  This also demonstrates to HGBs that counties have the capacity to train a skilled workforce for new opportunities in high-growth areas, like information technology."),
                                p("Rural areas face many unique challenges when trying to attract and retain industry. Their natural amenities, affordable and less dense housing, and access to education and training services make counties like Wythe desirable places to live.  Our approach to studying Wythe was to examine two main drivers of industry attraction: people and business amenities."),
                                p("In our project we provide a visual overview of the built/physical capital available in Wythe County.  Based on this framework, our team combined publicly available demographic, infrastructure, and labor information related to the current and potential workforce in Wythe County. The data were used to identify the human capital in Wythe, assess the potential of Wythe County to train its workforce in various industries, and construct a spatial measure of accessibility to education and workforce training centers."),
                                
                                h3("References:"),
                                
                                p("Acemoglu, D. (2012). 'What does human capital do? A review of Goldin and Katz's The race between education and technology.' Journal of Economic Literature 50(2), 426-63."),
                                p("Goldin, Claudia Dale and Katz, Lawrence F. 2009. The race between education and technology. Harvard University Press."),
                                p("Lamb, William B and Sherman, Hugh (2010). 'Developing high-growth businesses in rural areas: A study of four US States.' New England Journal of Entrepreneurship 12(2).")
                                
                            )
                        )),
                tabItem(tabName = "data",
                        fluidRow(
                            boxPlus(
                                title = "Data & Methodology",
                                closable = FALSE,
                                width = NULL,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                h2("Data and Data Sources"),
                                img(src="ACS.jpg", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                                br(),
                                p("We used data from the American Community Survey to visualize labor, housing and infrastructure data to identify built and human capital currently in Wythe County."),
                                br(),
                                br(),
                                br(),
                                br(),
                                img(src="careerWorks.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                                br(),
                                p("We used Virginia Careerworks Data to identify industries, colleges and universities, and workforce training facilities in Wythe county."),
                                br(),
                                br(),
                                br(),
                                img(src="VEC.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                                br(),
                                p("VirginiaWorks is a collaboration of agencies run by the Virginia Employment Commission."),
                                br(),
                                br(),
                                br(),
                                img(src="onet.png", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                                p("We used O*Net to gathered industry/occupation data for occupations that have a “bright outlook” which is defined as having an expectation of growth over the next several years. We also gather the skill set, education level, work experience, and training needed for these occupations."),
                                br(),
                                br(),
                                br(),
                                img(src="OSM.jpg", height="100", width="200", alt="Image", style="float: left; margin: 3px 12px 3px 0px; border: 1px solid #000000;"),
                                br(),
                                p("We used OpenStreetMap and the VirginiaWorks data to construct time-to-travel measures for access to education and workforce training. With this data, we were able to map floating catchment areas for various time windows in around Wythe County. "),
                                br(),
                                h2("Methodology"),
                                p("One of the central aims of this project is to create comparable measures of accessibility to educational facilities in the county.  In rural areas, metrics such as distance do not have the same meaning as they do in urban areas. Large distances in rural areas can often be traversed quickly due to lower traffic density and a lack of other traffic impediments. To create our accessibility measures, we rely on travel time, which accounts for both distance and traffic flow. We construct accessibility measures for each county in the region by estimating the travel time between the county population weighted centroid and foci for educational and workforce training (Waldorf and Chen, 2010). We use a floating catchment area around each county centroid and vary the size of the travel time window to include 30, 45, and 60 min windows for each catchment area. We then count the services by type within each of the travel time windows. This allows us, for each county, to construct:"),
                                p("1. No. of workforce training sites within 30 (45, 60) minute driving distance of the county"),
                                p("2. No. of community colleges within 30 (45, 60) minute driving distance of the county"),
                                p("3. No. of four-year colleges and universities within 30 (45, 60) minute driving distance of the county"),
                                h3("References:"),
                                
                                p("Luo, Wei and Wang, Fahui (2003). 'Measures of spatial accessibility to health care in a GIS environment: synthesis and a case study in the Chicago region.' Environment and Planning B: Planning and Design 30(6):865—884."),
                                p("Waldorf, B. S., & Chen, S. E. (2010). 'Spatial models of health outcomes and health behaviors: the role of health care accessibility and availability.' In Progress in spatial analysis (339-362). Springer: Berlin, Heidelberg.")
                                
                            )
                        )),
                
                tabItem(tabName = "builtcapital",
                        fluidRow(
                            h2("Transportation and Access to Major Markets for Firms"),
                            p("To understand the full suite of amenities available to HGBs in Wythe, we used publicly available demographic and infrastructure data to provide an overview of the built capital amenities in Wythe."),
                            p("In many respects, Wythe County is uniquely endowed with built amenities attractive to businesses (William and Lamb, 2010).  It is situated at the intersection of two major interstates, and it is within a six-to-eight-hour drive of most of the population in the United States. As the map shows, it also has easy access to rail and other supporting infrastructure (e.g., powerplants) for commerce and manufacturing. From an “access to major markets” perspective, Wythe is an attractive location for both light and heavy industry."),
                            boxPlus(
                                title = "Wythe County Infrastructure",
                                closable = FALSE,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = NULL,
                                #enable_dropdown = TRUE,
                                #dropdown_icon = "",
                                #dropdown_menu = tagList(selectInput("var","Select a Variable",choices = c("Level of Education","Industry","Home Values","Household Income","Household Size"))),
                                leafletOutput("wythe_infrastructure")
                            ),
                            h2("Land & Housing"),
                            p("One of the attractive features of rural America is the cost of land and housing.  Technology companies, for example, which often require land intensive sites for data centers, may find rural locations increasingly appealing. The cost of housing, the low time travel cost, and attractive recreational and natural amenities may also be important attributes to attract and retain employers and employees alike. The cost of housing in Wythe is very reasonable; a large proportion of homes are valued below $150,000. The housing stock in Wythe is, however, aging and limited (see graphs in dashboard below)."),
                            boxPlus(
                                title = "Built Capital in Wythe County",
                                closable = FALSE,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = NULL,
                                enable_dropdown = TRUE,
                                dropdown_icon = "",
                                dropdown_menu = tagList(selectInput("var","Select a Variable",choices = c("Home Age", "Home Values", "Building Permit Trend"))),
                                plotOutput("builtCapital")
                            ),
                            br()
                        )),
                tabItem(tabName = "humancapital",
                        fluidRow(
                            h2("Human Capital in Wythe County"),
                            p("Investing in human capital is yet another way of attracting and retaining HGBs. It can also provide an engine for entrepreneurship. Wythe has a total population of 29,000 with 13,000 workers. The median age of the population in Wythe county is 44, and approximately 42 percent of the population is under the age of 34. Because of its size and rural location, Wythe’s actual labor market pool is much larger than its population; it is within a 60-minute drive of most adjoining counties. The white cloud on the map surrounding Wythe represents all parts of the region that are within a 60-minute drive from the center of the county (the red dot)."),
                            br(),
                            boxPlus(
                                title = "Wythe:60-Minutes Isochrone",
                                closable = FALSE,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = NULL,
                                #enable_dropdown = TRUE,
                                #dropdown_icon = "",
                                #dropdown_menu = tagList(selectInput("var","Select a Variable",choices = c("Level of Education","Industry","Home Values","Household Income","Household Size"))),
                                leafletOutput("isochrones")
                            ),
                            br(),
                            h2("Characteristics of the Wythe Labor Market"),
                            br(),
                            boxPlus(
                                title = "Human Capital in Wythe County",
                                closable = FALSE,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = NULL,
                                enable_dropdown = TRUE,
                                dropdown_icon = "",
                                dropdown_menu = tagList(selectInput("vari","Select a Variable",choices = c("Industry","Level of Education","Household Income","Household Size"))),
                                plotOutput("myplot")
                            ),
                            br(),
                            p("The dashboard above shows important features of Wythe’s labor market with regard to industry concentration, salary, and household size."),
                            p("First, the workforce in Wythe County is concentrated in the health, education, manufacturing, and retail industries. Outside of education and healthcare, many workers in Wythe are in the retail sector.  While these types of service jobs are important, it should be recognized that these are not jobs that are typically associated with high earning potential. Moreover, e-commerce is continuing to crowd out many of these types of businesses. From an economic growth perspective, it is therefore important to continue diversifying the industrial base. "),
                            
                            p("Second, the majority of households in Wythe have incomes between $25,000 to $150,000, with the median household income between $50,000 and $75,000."),
                            p("Third, households in Wythe County are relatively small. The majority of households have one or two people. Given the median age and proportion of the population under the age of 34, the population of Wythe does not appear to be overly skewed toward older workers and retired individuals (as the prevalence of one-person and two-person households might suggest). "),
                            h3("References:"),
                            p("[1] https://datausa.io/profile/geo/wythe-county-va#:~:text=In%202017%2C%20the%20median%20age,County%2C%20VA%20residents%20was%2044."),
                            br()
                        )),
                tabItem(tabName = "jobs",
                        fluidRow(
                            p("One of the key tasks for the VT-DSPG team was to identify some of the best jobs available over the next several years. To accomplish this, we mined data from the ONet project. ONet databases list categories of occupations by sector and industry and rate them according to future job growth.  The ratings are a standardized index (0-100) created from reported importance and scale factors for each occupation. We call ONet high-growth occupations the “jobs of tomorrow.” To represent these high-growth occupation categories, we use a collapsible tree diagram, which allows us to compactly present large quantities of data. In this way, we can map careers from broad industries to particular occupation to the skill set and education level typically needed for each occupation. For comparison purposes, the ratings are listed next to each reported education level, job training time period, and experience time period."),
                            h3("Highlighted Industry"),
                            p("As an example, consider jobs within the information technology (IT) industry. The user can choose between careers in Information Systems, Network Systems, Programming/Software Development, and Web and Digital Communications. For each career path, the viewer can select specific occupations and find information about the skill set necessary for that occupation. A similar collapsible tree for education, which is also available in the dropdown dashboard, demonstrates the typical education credential necessary for someone in a particular occupation."),
                            p("The occupations listed in the tree were selected because they are careers with bright futures; as such, they represent the jobs of tomorrow. "),
                            p("The trees provided actionable information for implementing programs. They also suggest curricula that might be developed to train the future workforce in a desired industry. "),
                            boxPlus(
                                title = "Information Technology",
                                closable = FALSE,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = "100%",
                                enable_sidebar = FALSE,
                                enable_dropdown = TRUE,
                                dropdown_icon = "",
                                dropdown_menu = tagList(selectInput("varIT","Select a Variable",choices = c("Skills","Education", "Experience Needed", "On-Site Training", "On-the-Job Training"))),
                                collapsibleTreeOutput("myITtree",width = "100%")
                            ),
                            # br(),
                            # #p("Similar trees are included below for the Agricultural and Manufacturing industries."),
                            # boxPlus(
                            #   title = "Agriculture",
                            #   closable = FALSE,
                            #   status = "warning",
                            #   solidHeader = TRUE,
                            #   collapsible = TRUE,
                            #   width = "100%",
                            #   enable_sidebar = FALSE,
                            #   enable_dropdown = TRUE,
                            #   dropdown_icon = "",
                            #   dropdown_menu = tagList(selectInput("varAg","Select a Variable",choices = c("Skills","Education", "Experience Needed", "On-Site Training", "On-the-Job Training"))),
                            #   collapsibleTreeOutput("myAgtree",width = "100%")
                            # ),
                            # br(),
                            # boxPlus(
                            #   title = "Manufacturing",
                            #   closable = FALSE,
                            #   status = "warning",
                            #   solidHeader = TRUE,
                            #   collapsible = TRUE,
                            #   width = "100%",
                            #   enable_sidebar = FALSE,
                            #   enable_dropdown = TRUE,
                            #   dropdown_icon = "",
                            #   dropdown_menu = tagList(selectInput("varMan","Select a Variable",choices = c("Skills","Education", "Experience Needed", "On-Site Training", "On-the-Job Training"))),
                            #   collapsibleTreeOutput("myMantree",width = "100%")
                            # ),
                            br(),
                            h3("Jobs of Tomorrow"),
                            
                            br(),
                            boxPlus(
                                title = "Jobs of Tomorrow.",
                                closable = FALSE,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                width = "100%",
                                enable_sidebar = FALSE,
                                enable_dropdown = TRUE,
                                dropdown_icon = "",
                                dropdown_menu = tagList(selectInput("var1","Select a Variable",choices = c("Skills","Education", "Experience Needed", "On-Site Training", "On-the-Job Training"))),
                                collapsibleTreeOutput("mytree",width = "100%")
                            ),
                            p("Given the sizeable role investments in human capital have on economic growth, detailed knowledge of the industries, careers paths and occupations that are growing in the U.S. is valuable when determining how to allocate resources. This knowledge provides actionable information for preparing, teaching, and training the workforce of tomorrow. "),
                            p("The collapsible trees (above) map ONet career and occupation data. Each tree maps industry (the first level) to career pathways (second level) to occupations (third level). This data represents industries, careers and occupations with the highest projected growth or brightest outlook. In addition, the dropdown menu allows for a fourth level. As a whole, the trees map Skills, Education, Experience, On-Site-Training and On-Job-Training needed for each occupation, career and industry."),
                            p("Desired industries, as indicated by Wythe County officials, include manufacturing, agriculture, information systems and technology and food and beverage production. These industries are available in the visualizations above."),
                            br()
                        )),
                tabItem(tabName = "access",
                        fluidRow(
                            h2("A Regional Comparison of Educational Access"),
                            p("An analysis of the trees (see previous tab) shows that many of the high growth careers are in information technology. These jobs often require training in a host of critical areas, from customer support and client satisfaction to computer science and systems and electrical engineering. Aside from the heavy STEM focus in information technology, there is also a large need for people with skills in communication, education, and training. Additionally, there are many high-paying jobs in the IT sector that do not require a college degree. IT jobs such as web developer, computer programmer, systems analyst, cybersecurity analyst, graphic designer, digital marketer, and telecommunications technician can be filled by individuals that have relevant skills but not necessarily a four-year degree. In such cases, local community colleges and high schools can help with IT skill development by providing courses and certification programs that are readily accessible to residents in Wythe County."),
                            
                            tabBox(
                                title = NULL , width = 16,
                                # The id lets us use input$tabset1 on the server to find the current tab
                                id = "tabset1", height = "250px",
                                tabPanel("Educational Institutions", sidebarLayout(
                                    sidebarPanel(
                                        selectInput("spatial_variable", "Spatial Variable:",
                                                    c("Colleges and Universities",
                                                      "Community Colleges",
                                                      "Workforce Development Centers",
                                                      "Colleges - All types")),
                                        selectInput("time_variable", "Time Variable:",
                                                    c("60 minutes" = "60",
                                                      "45 minutes" = "45",
                                                      "30 minutes" = "30"))
                                    ),
                                    
                                    # Show a plot of the generated distribution
                                    mainPanel(
                                        tableOutput("label_1"),
                                        leafletOutput("mapplot_1"),
                                        #mapview:::plainViewOutput("test")
                                    )
                                )
                                
                                ),
                                tabPanel("Broadband and High School",
                                         sidebarPanel(
                                             selectInput("variable", "Comparison Variable:",
                                                         c("Number of High Schools",
                                                           "Percentage of Broadband Access",
                                                           "Percentage of People having Computer"))
                                             
                                         ),
                                         
                                         # Show a plot of the generated distribution
                                         mainPanel(
                                             tableOutput("label_2"),
                                             leafletOutput("mapplot_2")
                                             #mapview:::plainViewOutput("test")
                                         )
                                         
                                )
                            ),  
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            br(),
                            h3("Colleges and Universities"),
                            p("The foregoing analysis motivated the team to examine Wythe County’s access to institutions that provide high-quality, high-impact training. The dashboard above shows access measures for education and workforce training for Wythe County and the surrounding region. The workforce training, community college, and four-year college and university measures are counts of these locations within a chosen travel time from the population weighted county centroid."),
                            p("For example, Wythe County has three colleges and universities located within a 60-minute drive of most of the population. If, however, one narrows the time window to 30 minutes, there is only one. Further exploration of the dashboard shows that this institution is a community college, which may play an important role in providing workforce training. In summary, Wythe is well positioned in terms of access to continuing and higher education facilities."),
                            
                            br(),
                            h3("Broadband and High Schools"),
                            p("High schools also play a large role in training future workers. Trade and vocational programs can offer future career paths for many students who do not pursue additional education beyond a high school diploma. This second dashboard shows that Wythe County has three high schools."),
                            p("In this dashboard, we also display the percentage of the population who have access to broadband internet and who have a computer in the home. Online learning is an increasingly important and popular way to acquire additional skills and education. These visualizations allow us to assess Wythe’s ability to train the current and future workforce from home and to serve as a site for telework.  This is particularly important given that so many aspects of education and work have moved online due to Covid-19."),
                            br()
                        )),
                tabItem(tabName = "conclusions",
                        fluidRow(
                            boxPlus(
                                title = "Conclusions",
                                closable = FALSE,
                                width = NULL,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                
                                p("Wythe, like many of its counterparts in Southwest Virginia, has quality secondary schools and access to outdoor recreational amenities. These factors are often cited as important to millennials as they think about potential occupations and locations. To capture and retain workers, we recommend that Wythe emphasize tech-related jobs in addition to its current industrial strengths.  These jobs include cybersecurity, data centers, and so on. There are many high-paying jobs in these fields that DO NOT require a college degree. Additionally, this could allow Wythe to take better advantage of Virginia's Tech Talent Pipeline Initiative. "),
                                p("Wythe has many unique features that make it an attractive place to live: low population density, reasonably low rates of violent crime, and great natural amenities. If county leaders can demonstrate through programs at their high schools and community colleges that the local workforce is prepared for IT-related jobs and industries, then the county will be in a strong position to promote itself as a vital component of Southwest Virginia's Technology Corridor. Community colleges and local high schools (resources that are already available in Wythe) can help to develop workforce talent.")
                            )
                        )),
                tabItem(tabName = "team",
                        fluidRow(
                            boxPlus(
                                title = "Team",
                                closable = FALSE,
                                width = NULL,
                                status = "warning",
                                solidHeader = TRUE,
                                collapsible = TRUE,
                                h2("Data Science for the Public Good Program"),
                                p("The Data Science for the Public Good (DSPG) Young Scholars program is a summer immersive program held at the Biocomplexity Institute’s Social and Decision Analytics Division (SDAD). In its seventh year, the program engages students from across the country to work together on projects that address state, federal, and local government challenges around critical social issues relevant in the world today. DSPG young scholars conduct research at the intersection of statistics, computation, and the social sciences to determine how information generated within every community can be leveraged to improve quality of life and inform public policy. For more information on program highlights, how to apply, and our annual symposium, please visit the official Biocomplexity DSPG website."),
                                h2("2020 Wythe County Summer Project"),
                                p("Our project goal was to identify industries and the jobs that are expected to grow rapidly in the future. We visualized these jobs by the skills, education, experience and training needed to do them. We then created measures to visualize and assess the ability of Wythe County and the surrounding region to respond to training the workers of tomorrow. Our team is comprised of talented individuals with a broad range of skills and experience."),
                                h2("DSPG Team Members"),
                                img(src = 'Josh.Beverly.VT.jpg', height = "150", width = "140", align = "center"),
                                img(src = 'Dylan.Glover.VT.jpg', height = "150", width = "140", align = "center"),
                                img(src = 'Afrina.Tabassum.VT.jpg', height = "150", width = "140", align = "center"),
                                img(src = 'Adam.Wells.VT.jpg', height = "150", width = "140", align = "center"),
                                br(),
                                br(),
                                p("Josh Beverly, Fellow (Ph.D. Student at Virginia Tech, Agricultural and Applied Economics)"),
                                p("Dylan Glover, Intern (Undergraduate Student at Virginia Tech, Mathematics)"),
                                p("Afrina Tabassum, Intern (Ph.D. Student at Virginia Tech, Computer Science)"),
                                p("Adam Wells, Intern (Master Student at Virginia Tech, Data Analysis and Applied Statistics)"),
                                h2("Virginia Tech Faculty Team Members"),
                                img(src = 'Susan.Chen.VT.jpg', height = "150", width = "140", align = "center"),
                                img(src = 'Conaway.Haskins.VT.jpg', height = "150", width = "140", align = "center"),
                                img(src = 'Matt.Holt.VT.jpg', height = "150", width = "140", align = "center"),
                                img(src = 'Ford.Ramsey.VT.jpg', height = "150", width = "140", align = "center"),
                                br(),
                                br(),
                                p("Susan Chen (Associate Professor, Food and Health Economics, DSPG Project Co-Lead)"),
                                p("Conaway Haskins (Extensions Specialist, Rural & Regional Development)"),
                                p("Matt Holt (Department Head, Professor, Agribusiness, Applied Econometrics, Principal Investigator)"),
                                p("Ford Ramsey (Assistant Professor, Agribusiness, DSPG Project Co-Lead)"),
                                h2("Project Sponsors"),
                                img(src = 'VCE.Logo.png', height = "150", width = "200", align = "center", style="display: block; margin-left: auto; margin-right: auto;"),
                                p("Matthew Miller (Unit Coordinator and Extension Agent, Agriculture and Natural Resources - Farm Business Management)"),
                                
                                h2("Acknowledgements"),
                                p("We would like to thank:"),
                                p("Stephen Bear (Wythe County Administrator),"),
                                p("David Manely (Joint Industrial Development Authority of Wythe County)"),
                                p("John Matthews (Economic Developer & Associate Director at the Joint IDA of Wythe County)")
                            )
                        ))
            )
        ))
)





# SERVER ------------------------------------------------------------------ 
server <- function(input, output) {
    
    runjs(jscode)
    
    ## Render Trees ----------------------------------------------------------
    output$mytree <- renderCollapsibleTree({
        if(input$var1%in%"Skills"){
            Tree%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88)%>%
                group_by(Occupation)%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Element_Name"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1800,
                                zoomable=F)
        }
        
        
        else if(input$var1%in%"Education"){
            Tree_Ed%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88)%>%
                group_by(Occupation)%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1800,
                                zoomable=F)
            
        }
        else if(input$var1%in%"Experience Needed"){
            Tree_Ex%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88)%>%
                group_by(Occupation)%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1800,
                                zoomable=F)
            
        }
        else if(input$var1%in%"On-Site Training"){
            Tree_Site%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88)%>%
                group_by(Occupation)%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1800,
                                zoomable=F)
            
        }
        else {
            Tree_Job%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88)%>%
                group_by(Occupation)%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1800,
                                zoomable=F)
            
        }
    })
    ### Manufacturing Tree ----------------------------------------------------------
    output$myMantree <- renderCollapsibleTree({
        if(input$varMan%in%"Skills"){
            Tree_Man_skills%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Manufacturing")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Element_Name"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
        }
        
        
        else if(input$varMan%in%"Education"){
            Tree_Man_Ed%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Manufacturing")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
        else if(input$varMan%in%"Experience Needed"){
            Tree_Man_Ex%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Manufacturing")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
        else if(input$varMan%in%"On-Site Training"){
            Tree_Man_Site%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Manufacturing")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
        else {
            Tree_Man_Job%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Manufacturing")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
    })
    ## IT Tree
    output$myITtree <- renderCollapsibleTree({
        if(input$varIT%in%"Skills"){
            Tree_IT_skills%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Information Technology")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Element_Name"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
        }
        
        
        else if(input$varIT%in%"Education"){
            Tree_IT_Ed%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Information Technology")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
        else if(input$varIT%in%"Experience Needed"){
            Tree_IT_Ex%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Information Technology")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
        else if(input$varIT%in%"On-Site Training"){
            Tree_IT_Site%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Information Technology")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
        else {
            Tree_IT_Job%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Information Technology")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
    })
    ## Ag Tree
    output$myAgtree <- renderCollapsibleTree({
        if(input$varAg%in%"Skills"){
            Tree_Ag_skills%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Ag., Food, & Nat. Resources")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Element_Name"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
        }
        
        
        else if(input$varAg%in%"Education"){
            Tree_Ag_Ed%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Ag., Food, & Nat. Resources")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
        else if(input$varAg%in%"Experience Needed"){
            Tree_Ag_Ex%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Ag., Food, & Nat. Resources")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
        else if(input$varAg%in%"On-Site Training"){
            Tree_Ag_Site%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Ag., Food, & Nat. Resources")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
        else {
            Tree_Ag_Job%>%
                filter(Job_Openings>5300)%>%
                mutate_if(is.numeric, ~replace(., is.na(.), 0))%>%
                filter(Importance>=2.88 & Career_Cluster=="Ag., Food, & Nat. Resources")%>%
                collapsibleTree(hierarchy = c("Career_Cluster","Career_Pathway","Occupation","Cat_Val"),
                                root="Industries",
                                attribute = "Job_Openings",
                                width=1500,
                                zoomable=F 
                )
            
        }
    })
    # Render Plot Built Cap
    output$builtCapital <- renderPlot({
        if(input$var%in%"Building Permit Trend"){
            bps_final_Wythe %>%
                ggplot( aes(x=year, y=tot_units)) +
                geom_line( color="navy") +
                geom_point(shape=21, color="red", fill="#0066ff", size=2) +
                theme(legend.title = element_blank(), axis.ticks.y = element_blank(),
                      plot.title = element_blank(), axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), plot.subtitle = element_text(color="black", size=9, hjust = 0),
                      panel.background = element_blank()
                ) + 
                ggtitle("Trend of Building Unit Permits by Year") + 
                labs(title = "Building Permits in Wythe County", x = "Year", y = "Total Housing Units") + 
                scale_x_continuous(breaks = seq(1990, 2017, by = 5)) 
        }
        else if (input$var%in%"Home Age"){
            ggplot(f, aes(x=`Median Housing Stock Age`, y=`Proportion of Housing`, fill=as.factor(`Median Housing Stock Age`))) + 
                geom_bar(stat = "identity") +
                coord_flip() + theme(legend.title = element_blank(),axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"), plot.title = element_blank(), plot.subtitle = element_text(color="black", size=9, hjust = 0), axis.text.y = element_blank(), axis.ticks.y = element_blank(), panel.background = element_blank()) + 
                scale_fill_viridis_d(name = "Age of Housing", labels = c("0-6 years", "7-10 years", "11-20 years", "21-30 years","30+ years")) 
        }
        
        else {
            Wythe_long%>%filter(Variable%in%input$var)%>%
                ggplot(mapping=aes(x=fct_inorder(name),y=value,fill=fct_inorder(name)))+
                geom_bar(stat = "identity")+
                scale_fill_viridis(discrete = T)+
                labs (title="Home Values",y="Percent of Homes",x="")+
                theme_minimal()+
                theme(legend.position = "none", axis.title.y = element_text(size=14, face="bold"), plot.title = element_text(size = 18, face = "bold"), axis.text.x =element_text(angle=45, size = 14, hjust=1,vjust=1, face = "bold"))
        }
        
    })
    
    #Render Human Capital Plots
    output$myplot <- renderPlot({
        if(input$vari%in%"Household Size"){
            Wythe_long%>%filter(Variable%in%input$vari)%>%
                ggplot(mapping=aes(x=name,y=value,fill=name))+
                geom_col()+
                scale_fill_viridis(discrete = T)+
                labs (title="Household Size",y="Percent of Households",x="")+
                theme_minimal()+
                theme(legend.position = "none", axis.text.x =element_text(size = 12, face = "bold"), plot.title = element_text(size=18, face="bold"), axis.title.x = element_text(size=14, face="bold"), axis.title.y = element_text(size=14, face="bold"))
        }
        else if (input$vari%in%"Level of Education"){
            Wythe_long%>%filter(Variable%in%input$vari)%>%
                ggplot(mapping=aes(x=fct_inorder(name),y=value,fill=fct_inorder(name)))+
                geom_col()+
                scale_fill_viridis(discrete = T)+
                labs (title="Highest Level of Education (Age > 25)",y="Percent of Population",x="")+
                theme_minimal()+
                theme(legend.position = "none", plot.title = element_text(size=18, face="bold"), axis.title.x = element_text(size=14, face="bold"), axis.text.x =element_text(size = 12, face = "bold"), axis.title.y = element_text(size=14, face="bold"))
        }
        else if (input$vari%in%"Household Income"){
            Wythe_long%>%filter(Variable%in%input$vari)%>%
                ggplot(mapping=aes(x=fct_inorder(name),y=value,fill=fct_inorder(name)))+
                geom_bar(stat = "identity")+
                scale_fill_viridis(discrete = T)+
                labs (title="Household Income",y="Percent of Households",x="")+
                theme_minimal()+
                theme(legend.position = "none",plot.title = element_text(size=18, face="bold"), axis.title.y = element_text(size=14, face="bold"),axis.text.x =element_text(angle=45,hjust=1,vjust=1, size = 14, face = "bold"))
        }
        else{
            Wythe_long%>%filter(Variable%in%input$vari)%>%
                ggplot(mapping=aes(x=name,y=value,fill=name))+
                geom_bar(stat = "identity")+
                scale_fill_viridis(discrete = T)+
                labs (title="Employment by Industry",y="Percent of Population",x="")+
                theme_minimal()+
                theme(legend.position = "none",axis.text.x =element_text(angle=65,hjust=1,vjust=1,size=12), plot.title = element_text(size=18, face="bold"), axis.title.y = element_text(size=14, face="bold"))
        }
    })
    #Accesibility Maps
    # Create Map Points 1
    points <- eventReactive(input$recalc, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    # Render Access Measures
    output$mymap <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers(data = points())
    })
    
    # Create Map Points 2
    points2 <- eventReactive(input$recalc2, {
        cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
    }, ignoreNULL = FALSE)
    
    # Render Map 2
    output$mymap2 <- renderLeaflet({
        leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)) %>%
            addMarkers(data = points2())
    })
    #Spatial Variable Maps
    output$label_1 <- renderText({
        paste( input$spatial_variable, input$time_variable, " minute radius")
    })
    output$mapplot_1 <- renderLeaflet({
        
        distance_measure = NULL
        leg= NULL
        labels = NULL
        colors= NULL
        if(input$spatial_variable == "Colleges and Universities"){
            distance_measure = dis_clg_uni
        }else if(input$spatial_variable == "Community Colleges"){
            distance_measure = dis_community
        }else if(input$spatial_variable == "Workforce Development Centers"){
            distance_measure = dis_workforce
            
        }else{
            distance_measure= merge(dis_clg_uni,dis_community, by = "X1")
        }
        num_col <- ncol(distance_measure)
        filtered_distance_measure <- distance_measure <=as.numeric(input$time_variable)
        distance_measure$count <- rowSums(filtered_distance_measure[, 2:num_col])
        
        selected_distance_measure <- distance_measure %>% rename ( COUNTYFP = X1) %>% select(COUNTYFP, count)
        
        map_and_data <- inner_join(mapVA_county, selected_distance_measure, by = "COUNTYFP")
        
        
        if(input$spatial_variable == "Colleges and Universities"){
            if(input$time_variable == "60" ){
                leg = c(0,1,2,3,4,5,14)
                labels = c("0","1","2","3","4","5 or more")
                mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
                colors <- mypalette(map_and_data$count)
            }
            if(input$time_variable == "45" ){
                leg = c(0,1,2,3,4,5,11)
                labels = c("0","1","2","3","4","5 or more")
                mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
                colors <- mypalette(map_and_data$count)
            }
            if(input$time_variable == "30" ){
                leg = c(0,1,2,3,4)
                labels =c("0","1","2","3","4")
                colors <- c("#440154", "#414487", "#2A788E", "#22A884", "#7AD151")
                mypalette <- colorFactor(colors,leg)
                
            }
        }else if(input$spatial_variable == "Community Colleges"){
            if(input$time_variable == "30" ){
                leg = c(0,1)
                labels =c("0","1")
                colors <- c("#440154", "#21908D")
                mypalette <- colorFactor(colors,leg)
                
            }
            if(input$time_variable == "45" ){
                leg = c(0,1,2,3)
                labels =c("0","1","2")
                mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
                colors <- mypalette(map_and_data$count)
                
            }
            if(input$time_variable == "60" ){
                leg = c(0,1,2,3)
                labels =c("0","1","2")
                mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
                colors <- mypalette(map_and_data$count)
                
            }
        }else if(input$spatial_variable == "Workforce Development Centers"){
            if(input$time_variable == "30" ){
                leg = c(0,1,2,3,4,5,8)
                labels = c("0","1","2","3","4","5 or more")
                mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
                colors <- mypalette(map_and_data$count)
                
            }
            if(input$time_variable == "45" ){
                leg = c(0,1,2,3,4,5,11)
                labels = c("0","1","2","3","4","5 or more")
                mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
                colors <- mypalette(map_and_data$count)
                
            }
            if(input$time_variable == "60" ){
                leg = c(0,1,2,3,4,5,11)
                labels = c("0","1","2","3","4","5 or more")
                mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
                colors <- mypalette(map_and_data$count)
                
            }
        }else{
            if(input$time_variable == "60" ){
                leg = c(0,1,2,3,4,5,19)
                labels = c("0","1","2","3","4","5 or more")
                mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
                colors <- mypalette(map_and_data$count)
            }
            if(input$time_variable == "45" ){
                leg = c(0,1,2,3,4,5,16)
                labels = c("0","1","2","3","4","5 or more")
                mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
                colors <- mypalette(map_and_data$count)
            }
            if(input$time_variable == "30" ){
                leg = c(0,1,2,3,4,5,8)
                labels =c("0","1","2","3","4","5 or more")
                mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
                colors <- mypalette(map_and_data$count)
            }
            
        }
        
        #mypalette <- colorQuantile(palette="viridis", leg,n= length(leg)-1)
        
        #mypalette <- colorQuantile(palette="viridis", c(0,max_eviction),n=12)
        
        #construct map
        leaflet() %>%
            addTiles() %>%
            addPolygons(data=map_and_data,color = mypalette(map_and_data$count),
                        smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$count))%>%
            addLegend(pal = mypalette,position = "topright",values = leg,labels = labels,
                      labFormat = function(type, cuts, p) {  # Here's the trick
                          paste0(labels)
                      },
                      opacity = .6,title= paste("Number of ", input$spatial_variable)) %>%
            
            addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1)       %>%
            addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 )
        
        
    })
    
    
    #Spatial Map 2
    output$label_2 <- renderText({
        paste( input$variable)
    })
    
    
    output$mapplot_2 <- renderLeaflet({
        if(input$variable == "Number of High Schools"){
            
            dataset <- High_schools
            map_and_data <- inner_join(mapVA_county, dataset, by = "COUNTYFP")
            
            leg <- c(1,2,3,4,5)
            mypalette <- colorQuantile(palette="viridis", leg,n= 4)
            labels <- c("1","2","3","4")
            leaflet() %>%
                addTiles() %>%
                addPolygons(data=map_and_data,color = mypalette(map_and_data$count),
                            smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$count))%>%
                addLegend(pal = mypalette,position = "topright",values = leg,labels = labels,
                          labFormat = function(type, cuts, p) {  # Here's the trick
                              n = length(cuts)
                              paste0(labels)
                          },
                          opacity = .6,title= paste("Number of High School")) %>%
                
                addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1)       %>%
                addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 )
        }
        else if(input$variable == "Percentage of Broadband Access"){
            dataset <- Internet_Education
            map_and_data <- inner_join(mapVA_county, Internet_Education, by = "COUNTYFP")
            
            mypalette <- colorNumeric(palette="viridis", map_and_data$PerBroadband)
            leaflet() %>%
                addTiles() %>%
                addPolygons(data=map_and_data,color = mypalette(map_and_data$PerBroadband),
                            smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$PerBroadband))%>%
                addLegend(pal = mypalette,position = "topright",values = map_and_data$PerBroadband,
                          
                          opacity = .6,title= paste("Percentage of Broadband Access")) %>%
                
                addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1)       %>%
                addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 )
            
            
        }else{
            dataset <- Internet_Education
            map_and_data <- inner_join(mapVA_county, Internet_Education, by = "COUNTYFP")
            leg <- c(70,75,80,85,90,95,100)
            #mypalette <- colorNumeric(palette="viridis", map_and_data$PerHasComputer)
            mypalette <- colorNumeric(palette="viridis", leg)
            
            leaflet() %>%
                addTiles() %>%
                addPolygons(data=map_and_data,color = mypalette(map_and_data$PerHasComputer),
                            smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$PerHasComputer))%>%
                addLegend(pal = mypalette,position = "topright",values = leg,
                          
                          opacity = .6,title= paste("Percentage of People having Computer ")) %>%
                
                addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1)       %>%
                addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 )
        }
    })
    
    #Render Isochrone map
    output$isochrones <- renderLeaflet({
        mapVA  <- st_read("isochrones/VirginiaShapeFiles2/tl_2019_51_tract.shp",
                          stringsAsFactors = FALSE)
        
        map_and_data <- inner_join(mapVA, acs_Wythe_area, by = "GEOID")
        
        Colleges_and_Universities <- read_csv("isochrones/Colleges_and_Universities.csv")
        
        filtered_college_university <- Colleges_and_Universities %>% filter( STATE == "VA") %>%
            filter(COUNTY %in% c("WYTHE", "CARROLL", "PULASKI", "BLAND", "SMYTH",
                                 "GRAYSON","PITTSYLVANIA", "WASHINGTON", "FLOYD", "PATRICK","FRANKLIN",
                                 "HENRY", "MARTINSVILLE", "DANVILLE", "GALAX")) %>% filter( ZIP != 23851)
        
        
        
        Workforce_Development_Centers <- read_csv("isochrones/Workforce_Development_Centers.csv")
        colnames(Workforce_Development_Centers)[1] <- "Longitude"
        colnames(Workforce_Development_Centers)[2] <- "Latitude"
        
        workforce_dev_center_data <- Workforce_Development_Centers %>%
            filter(Zip %in% c("24312" , "24313" , "24322" , "24323" , "24324" , "24350" , "24360" ,
                              "24368" , "24374" , "24382", # wythe
                              "24053" , "24120" , "24312" , "24317" , "24325" , "24328" , "24333" , "24343" ,
                              "24350" , "24351" , "24352" , "24380" , "24381", # Carroll
                              "24292", "24326", "24330", "24333", "24348", "24350", "24363", "24378", # Grayson
                              "24084", "24124", "24134", "24314", "24315", "24318", "24366", # Bland
                              "24084", "24141", "24301", "24324", "24347", # Pulaski
                              "24054", "24069", "24137", "24139", "24161", "24527", "24530", "24531", "24540", "24541",
                              "24549", "24557", "24563", "24565", "24566", "24569", "24586", "24592",
                              "24594", "24597",#Pittslvania
                              "24311", "24316", "24318", "24319", "24354", "24368", "24370", "24375", "24378", #Smyth
                              "24201", "24202", "24210", "24211", "24236", "24258", "24270", "24319", "24340",
                              "24361", "24370", #Washington
                              "24059", "24072", "24079", "24091", "24105", "24120", "24138", "24141", "24149", "24352",
                              "24380", #Floyd
                              "24076", "24082", "24133", "24171", "24185", #Patrick
                              "24055", "24065", "24067", "24088", "24092", "24101", "24102", "24112" , "24121", "24151",
                              "24176", "24184", #Franklin
                              "24078", "24089", "24148", "24165", "24168", #Henry
                              "24112", # Martinsville
                              "24541", #Danville
                              "24333" #Galax
            ))
        
        
        hospitals <- st_read("isochrones/Hospitals__Virginia_shp/Hospitals__Virginia_.shp")
        
        hospitals <- hospitals %>% rename(Longitude = POINT_X)
        hospitals <- hospitals %>% rename(Latitude = POINT_Y)
        
        filtered_hospitals <- hospitals  %>%
            filter(Zip %in% c("24312" , "24313" , "24322" , "24323" , "24324" , "24350" , "24360" ,
                              "24368" , "24374" , "24382", # wythe
                              "24053" , "24120" , "24312" , "24317" , "24325" , "24328" , "24333" , "24343" ,
                              "24350" , "24351" , "24352" , "24380" , "24381", # Carroll
                              "24292", "24326", "24330", "24333", "24348", "24350", "24363", "24378", # Grayson
                              "24084", "24124", "24134", "24314", "24315", "24318", "24366", # Bland
                              "24084", "24141", "24301", "24324", "24347", # Pulaski
                              "24054", "24069", "24137", "24139", "24161", "24527", "24530", "24531", "24540", "24541",
                              "24549", "24557", "24563", "24565", "24566", "24569", "24586", "24592",
                              "24594", "24597",#Pittslvania
                              "24311", "24316", "24318", "24319", "24354", "24368", "24370", "24375", "24378", #Smyth
                              "24201", "24202", "24210", "24211", "24236", "24258", "24270", "24319", "24340",
                              "24361", "24370", #Washington
                              "24059", "24072", "24079", "24091", "24105", "24120", "24138", "24141", "24149", "24352",
                              "24380", #Floyd
                              "24076", "24082", "24133", "24171", "24185", #Patrick
                              "24055", "24065", "24067", "24088", "24092", "24101", "24102", "24112" , "24121", "24151",
                              "24176", "24184", #Franklin
                              "24078", "24089", "24148", "24165", "24168", #Henry
                              "24112", # Martinsville
                              "24541", #Danville
                              "24333" #Galax
            ))
        
        CenterPop_county <- read.csv("isochrones/CenPop2010_Mean_CO51.txt", header =T)
        
        centerPop_wythe <- CenterPop_county %>% filter(COUNAME %in% c("Wythe")
        ) %>% filter(COUNTYFP != 620)
        
        
        
        pop_centroid_wythe_iso_60 <- readRDS("isochrones/pop_centroid_wythe_iso_60.RDS")
        st_crs(pop_centroid_wythe_iso_60) = 4326
        
        nearby_airport <- read_excel("isochrones/Virginia airport.xlsx")
        nearby_airport <- nearby_airport[c(1,3,4),]
        
        
        mypalette <- colorNumeric(palette="viridis", map_and_data$Total_Population)
        
        map_with_all_point <- leaflet() %>%
            addTiles() %>%
            addPolygons(data=map_and_data,color = mypalette(map_and_data$Total_Population),
                        smoothFactor = 0.2, fillOpacity=.6, weight = 1,stroke = F, label=paste(" county name ", map_and_data$NAME ,", Value: ",map_and_data$Total_Population))%>%
            addLegend(pal = mypalette,position = "topright",values = map_and_data$Total_Population,
                      
                      opacity = .6,title= paste("Total Population")) %>%
            
            addPolylines(data = Wythe_area_outline, color = "black", opacity = 1, weight = 1)       %>%
            addPolylines(data = Wythe_outline, color = "red", opacity = 2, weight = 1 ) %>%
            addPolygons(data = pop_centroid_wythe_iso_60 , color = "white",
                        opacity = 1, weight = 1, fillColor = "white",fillOpacity = .6)%>%
            addCircleMarkers(centerPop_wythe,lat = centerPop_wythe$LATITUDE, lng= centerPop_wythe$LONGITUDE,
                             radius =  4,
                             color = "red",
                             stroke = FALSE, fillOpacity = 1
            ) %>%
            addLegend(colors = "red", labels = "Wythe Population Centroid") %>%
            addCircleMarkers(nearby_airport,lat = nearby_airport$Lattitude, lng= nearby_airport$Longitude,
                             radius =  4,
                             color = "#55DDE0",
                             stroke = FALSE, fillOpacity = 1
            ) %>%
            addLegend(colors = "#55DDE0", labels = "Airports") %>%
            
            addCircleMarkers(filtered_college_university,lat = filtered_college_university$LATITUDE ,
                             lng = filtered_college_university$LONGITUDE,
                             radius =  3,
                             color = "orange",
                             stroke = FALSE, fillOpacity = 1
            ) %>%
            addLegend(colors = "orange", labels = "College and University") %>%
            addCircleMarkers(workforce_dev_center_data,lat = workforce_dev_center_data$Latitude ,
                             lng = workforce_dev_center_data$Longitude,
                             radius =  3,
                             color = "pink",
                             stroke = FALSE, fillOpacity = 1
            ) %>%
            addLegend(colors = "pink", labels = "Workforce Development")%>%
            addCircleMarkers(filtered_hospitals,lat = filtered_hospitals$Latitude ,
                             lng = filtered_hospitals$Longitude,
                             radius =  3,
                             color = "yellow",
                             stroke = FALSE, fillOpacity = 1
            ) %>%
            addLegend(colors = "yellow", labels = "Hospitals") %>% 
            setView(lat = 36.927813,lng = -80.6372317, zoom =9)
        map_with_all_point 
        
        
        
    })
    
    output$wythe<-renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolylines(data = tigris::counties(state="51",cb=T,class="sf"), color = "black", weight = 1)%>%
            addPolygons(data = Wythe_outline, color = "red", fillColor="red", fillOpacity = .6, weight = 1)
    })
    
    output$wythe_infrastructure<-renderLeaflet({
        leaflet() %>%
            addTiles() %>%
            addPolylines(data = tigris::counties(state="51",cb=T,class="sf"), color = "black", weight = 1)%>%
            addPolygons(data = Wythe_outline, color = "grey", weight = 1)%>%
            addCircles(lat=Power_Plants$LATITUDE, lng=Power_Plants$LONGITUDE, color="blue",radius = 50)%>%
            addCircles(lat=Cities_and_Towns$latitude, lng=Cities_and_Towns$longitude, color="red",radius = 50)%>%
            setView(lat=36.9541936,lng=-81.101218,zoom = 9)%>%
            addLegend(colors = c("blue","red"),labels=c("Power Plants", "Cities and Towns"))
    })
    
}


# Shiny APP ------------------------------------------------------------------ 
shinyApp(  ui = ui, server = server )
