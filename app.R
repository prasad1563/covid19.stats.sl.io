library(shiny)
library(shinydashboard)
library(plotly)
library(shinythemes)
library(data.table)
library(shinyWidgets)
library(tmap)
library(readxl)
library(sf)
library(dplyr)
library(shinycssloaders)
library(tidyverse)
library(lubridate)
library(leaflet)
library(shinyjs)


######################## Functions ##################
source("functions/districtMapSL.R")
source("functions/clusterPieChart.R")
source("functions/cumulative_plot.R")
source("functions/districtLinePlot.R")

############################################# Import the datasets and rearrange #################################################
cumulativeData <- read.csv("Data/Cumulative Data/SL_cum_counts.csv")
cumulativeData$Date <- as.Date(cumulativeData$Date, format = "%m/%d/%y")

clusterData <- read.csv("Data/Cluster_classified_data/cluster_data.csv")
clusterData$Date <- as.Date(clusterData$Date, format = "%m/%d/%y")
colnames(clusterData) <- c("Date", "infectedCount", "districtSum", "importedCase", "otherCluster")

date_seq <- seq.Date(from = as.Date("2020-11-13"), to = as.Date("2021-04-21"), by = "day")

##############################################################################
covid19DistrictCases <- read_csv(file = "Data/District_confirmed_data/District_data_Nov14_Apr21_transpose.csv")

colnames(covid19DistrictCases)
colnames(covid19DistrictCases)[19] <- "Monaragala"
covid19DistrictCases %>%
    gather("district", "count", -Date) -> covid19DistrictCasesLong

mapSL <- st_read("Data/Shape_files/lka_admbnda_adm2_slsd_20200305.shp") 

mapSL <- mapSL %>%
    select(ADM2_EN) %>%
    filter(ADM2_EN != "[unknown]")



mapSLCovid19 <- left_join(mapSL, covid19DistrictCasesLong, by = c("ADM2_EN" = "district"))

mapSLCovid19 %>% mutate(Date = mdy(Date)) -> mapSLCovid19

colnames(mapSLCovid19) <- c("District", "Date", "Count", "geometry")

covidDistrictsSL <- mapSLCovid19

districtLineData <- read_excel("Data/District_confirmed_data/District_data_Nov14_Apr21_transpose.xlsx",
                               col_names = TRUE)
districtLineData$Date <- as.Date(districtLineData$Date, format = "%y-%m-%d")

############################################### Data preprocessing ######################################
clusterData_min_date = as.Date(min(clusterData$Date), "%y-%m-%d")
clusterData_max_date = as.Date(max(clusterData$Date), "%y-%m-%d")

############################################## CSS themes ###############################################
options(spinner.color = "#0275D8", spinner.color.background = "#ffffff", spinner.size = 2)

####################################### SHINY APP FUNCTIONS ###############################################
# Define UI for application 
ui <- dashboardPage(
    
    dashboardHeader(
        
        title = tags$span("COVID-19 Stats - SL",
            style = "font-family: 'Palatino Linotype', 'Book Antiqua', Palatino, serif;font-size: 20px;"
            
        )
    ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Cumulative Figures", tabName = "cumulative", icon = icon("dashboard")),
            menuItem("District Distribution", tabName = "district_distribution", icon = icon("map")),
            menuItem("Cluster Distribution", tabName = "cluster", icon = icon("bar-chart-o")),
            menuItem("About Us", tabName = "about", icon = icon("info-circle"))
        )
    ),
 
    
    
    dashboardBody(
        includeCSS("Styles.css"), 
        tabItems(
            tabItem(
                tabName = "cumulative",
               # h2("Cumulative count content"),
                fluidRow(
                    valueBoxOutput("infected", width = 4),
                    valueBoxOutput("recovered", width = 4),
                    valueBoxOutput("deaths", width = 4)
                ),
               h1("Distribution Of Cumulative Counts", style = "font-family: Lato; color: #333399;font-weight: bold;margin-bottom: 30px;"),
                fluidRow(
                    column(width = 6, plotlyOutput("graph", width = "100%")),
                    column(width = 6, plotlyOutput("deathCurve", width = "100%"))
                ),
               # Add a footer to this page
               div(class = "container",
                   tags$footer(HTML("COVID-19 Stats Dashboard &copy; 2021. All rights reserved."), class = "footer"))
            ),

            ####district distribution page
            tabItem(
                tabName = "district_distribution",
                h1("District-Specific Insights",style = "font-family: Lato; color: #333399;font-weight: bold;margin-bottom: 30px;"),
                sidebarLayout(
                    sidebarPanel(
                        width = 3,
                        sliderInput(inputId = "page4plotDate", "Date Filter:",
                                    min = as.Date("2020-11-14", "%Y-%m-%d"),
                                    max = as.Date("2021-04-21", "%Y-%m-%d"),
                                    value = as.Date("2020-11-14"),
                                    step = 1),
                                    
                        verbatimTextOutput(outputId = "Dates"),
                        selectInput(inputId = "districtSelect",
                                    label = h5("Select a District to display the trend line:", style = "font-weight: bold;"),
                                    choices = unique(mapSLCovid19$District))
                    ),
                    mainPanel(
                        tabsetPanel(
                            id = "myTabsetPanel",
                            tabPanel("District Hot Spots", tmapOutput("district_map")),
                            tabPanel("Trend Line", plotlyOutput("districtLineGraph"))
                        )
                    )
                    # Add a footer to this page
                    
                ),div(class = "container",
                      tags$footer(HTML("COVID-19 Stats Dashboard &copy; 2021. All rights reserved."), class = "footer"))
                
            ),
            tabItem(
                tabName = "cluster",
                h2("COVID-19 Case Summary",style = "font-family: Lato; color: #333399;font-weight: bold;margin-bottom: 30px;"),
                sidebarLayout(
                    sidebarPanel(
                        sliderInput(inputId = "plotDate", "Date Filter:",
                                    min = as.Date(min(clusterData$Date), "%Y-%m-%d"),
                                    max = as.Date(max(clusterData$Date), "%Y-%m-%d"),
                                    value = as.Date("2020-11-14"),
                                    step = 1,
                                    animate = animationOptions(interval = 500, loop = TRUE))
                    ),
                    mainPanel(
                        tabsetPanel(
                           tabPanel("Case Distribution-Cumulative", box(plotlyOutput("cumulativeGraph"),width = 12,style = "margin-top: 50px;")),
                           tabPanel("Case Distribution-Selected Date", box(plotlyOutput("lastDayGraph"),width = 12,style = "margin-top: 50px;"))
                        )
                    )
                    
                    
                    
                    # Add a footer to this page
                    
                ),div(class = "container",
                      tags$footer(HTML("COVID-19 Stats Dashboard &copy; 2021. All rights reserved."), class = "footer"))
            ),
            
            
            ###################################        
            tabItem(
                tabName = "about",
                h2("Summary Dashboard for COVID-19 Statistics in Sri Lanka"),
                
                p("The aim of this project is to develop a comprehensive summary dashboard that provides 
                key statistics related to the impact of the SARS-CoV-2 pandemic in Sri Lanka. The dashboard 
                focuses on reporting data on infections, recoveries, and deaths during a specific period. 
                To achieve this, we have utilized a combination of open-source tools, including RShiny for visualization, 
                 as well as Python libraries and R for data extraction and preprocessing. "),
                
                h3("Key Features:"),
                
                # List of Key Features
                p(HTML("<ul>
                 <li>Infection Statistics: The dashboard prominently displays the total number of individuals infected by the virus during the specified time frame.</li>
                 <li>Recovery Statistics: It also provides data on the total count of individuals who have successfully recovered from COVID-19.</li>
                 <li>Mortality Statistics: The dashboard includes information about the total number of reported deaths attributed to the pandemic.</li>
               </ul>")),
                
                #data sources
                h3("Data Sources:"),
                
                # Collaboration and Acknowledgment Paragraph
                HTML("<p>To ensure the accuracy and reliability of the information presented, we collected data from authoritative sources,
                     including <a href='https://www.epid.gov.lk/situation-report' target='https://www.epid.gov.lk'>official Epidemiology Unit website</a>.</p>"),
    
                
                
                h3("Data Processing:"),
                
                # Data Processing Paragraph
                p("To ensure the accuracy and reliability of the information presented, we leveraged Python 
                libraries and R for data extraction for data preprocessing. 
                  This involved organizing and structuring the raw data to generate meaningful insights."),
                
                h3("Visualization Tool:"),
                
                # Visualization Tool Paragraph
                p("The primary tool used to create this informative dashboard is RShiny. RShiny is an open-source data visualization tool that enables the development of interactive and user-friendly interfaces. It allows us to present the COVID-19 statistics in an engaging and accessible manner."),
                
                h3("Collaboration and Acknowledgment:"),
                
                # Collaboration and Acknowledgment Paragraph
                HTML("<p>This dashboard project was a collaborative effort led by R.M.Prasad Priyadarshana and 
                     U.S.R.D.Vilochana Bandara. We would like to extend our deepest gratitude to 
                     <b>Dr. Mahasen Dehideniya</b> for their valuable supervision, support, and contributions to this initiative. 
                     If you have any questions or feedback, please do not hesitate to reach out to us at 
                     <a href='mailto:prasadpriyadarshana4@gmail.com'>prasadpriyadarshana4@gmail.com</a>, 
                     <a href='mailto:vilochanasmc@gmail.com'>vilochanasmc@gmail.com</a>.</p>"),
                
                # Add a footer to this page
                div(class = "container",
                    tags$footer(HTML("COVID-19 Stats Dashboard &copy; 2021. All rights reserved."), class = "footer"))
            )
        )
    ),
    
    title = "COVID-19 Stats-SL"
)

# Define server logic required 
server <- function(input, output) {
    
    shinyjs::useShinyjs()
    
    observe({
        active_tab <- input$myTabsetPanel  
        if (active_tab == "district map") {
            shinyjs::hide("districtSelect")  # Hide the radioGroupButtons
        } else if (active_tab == "districtLineGraph") {
            shinyjs::show("districtSelect")  # Show the radioGroupButtons
        }
    })
    ## Value for valuebox-1 page2
    output$infected <- renderValueBox(
        {
          
            
            valueBox(value = cumulativeData[nrow(cumulativeData), 2],
                     subtitle = "Total Infected Count",
                     color = "yellow")
                
        }
    )
    
    ## Value for valuebox-2, page2
    output$recovered <- renderValueBox(
        {
            valueBox(value = cumulativeData[nrow(cumulativeData), 3],
                     subtitle = "Total Recovered Count",
                     color = "green")
        }
    )
    
    ## Value for valuebox-3
    output$deaths <- renderValueBox(
        {
            valueBox(value = cumulativeData[nrow(cumulativeData), 4],
                     subtitle = "Total Deaths",
                     color = "red")
        }
    )
    
    ## Plotly graph page2
    output$graph <- renderPlotly({
        cumulativeDistribution <- plot_ly(cumulativeData, x = ~Date, y = ~Infected, name = "Infected Count", type = 'scatter', mode = 'lines')
        cumulativeDistribution <- cumulativeDistribution %>% add_trace(y = ~Recovered, name = "Recovered Count", type = 'scatter', mode = 'lines')
        
        cumulativeDistribution <- cumulativeDistribution %>% layout(title = "Distribution of Infected and Recovered Cases",
                                                                      xaxis = list(title = "Month"),
                                                                      yaxis = list(title = "Count of individuals"),
                                                                      titlefont = list(family = "Arial", weight = "bold"))
    })
    
    output$deathCurve <- renderPlotly({
        deathCurve <- plot_ly(cumulativeData, x = ~Date, y = ~Deaths, name = "Total reported", mode = "lines")
        
        deathCurve <- deathCurve %>% layout(title = "Distribution Of Deaths",
                                            xaxis = list(title = "Month"),
                                            yaxis = list(title = "Count of individuals"),
                                            titlefont = list(family = "Arial", weight = "bold"))
    })
    
    ### Graphs for page 3
    
    ##################################Line graph of cumulative tab
    output$cumulativeGraph <- renderPlotly({
        cumulative_plot(clusterData, as.Date(input$plotDate, "%y-%m-%d"))
    })
    
    output$result <- renderPrint({
        paste0("Selected Date: ", as.Date(input$plotDate, "%y-%m-%d"))
    })
    
    ################################Pie chart for selected last day
    
    output$lastDayGraph <- renderPlotly({
        clusterPieChart(clusterData, as.Date(input$plotDate, "%y-%m-%d"))
    })
    
    #########################Graph for page 4 ##########
    output$Dates <- renderPrint({
        paste0("Selected Date: ", input$page4plotDate)
    })
    
    output$district_map <- renderTmap({
        districtMapSL(mapSLCovid19,input$page4plotDate)
    })
    
    output$districtLineGraph <- renderPlotly({
        districtLinePlot(mapSLCovid19, input$districtSelect, input$page4plotDate)
    })
    
    output$Dates_linegraph <- renderPrint({
        paste0("Date selected: ", input$page4plotDate_linegraph)
    })
    
    

}


# Run the application
shinyApp(ui = ui, server = server)
