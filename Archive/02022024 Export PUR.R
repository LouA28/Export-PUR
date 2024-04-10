#Load libraries     

library(shiny)
library(readxl)
library(ggplot2)
library(plotly)
library(treemap)
library(treemapify)
library(data.table)
library(DT)
library(dplyr)
library(shinydashboard)
library(openxlsx)
library(tidyr)
library(stringr)
library(png)
library(scales)
library(purrr)

## Importing PUR calculations

PUR_exportdata <- readRDS("final_exportPUR2024-01-25.RDS")

## Importing PUR pref data

Preftype_data <- readRDS("PUR_type of export preference2024-01-25.RDS")

##################### Creating dataframes and functions that will link to different inputs of the app #############

## 1. PUR rates by chapter

## PUR rates by chapter 
HS2_df_2 <- PUR_exportdata %>%
  group_by(HS2,HS2_desc, year) %>%
  summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100, .groups = "drop") %>%
  na.omit(PUR)

## 2. PUR rates by chapter & country (excluding any PUR rates that are not calculated)
## This dataset is used to calculate the PUR rate by each HS chapter AND country (excluding any NAs in the original dataset)
## This is used to create country list & calculate total number of countries by HS chapter

HS2_df_country <- PUR_exportdata %>%
  group_by(HS2, HS2_desc, country_name) %>%
  summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100,.groups = "drop")


## 3. Create dropdown of all countries available in the dataset
## The country names in the dataset is extracted by displaying the unique values 
## The country names in country_choice are reordered alphabetically (A-Z)

country_choice <- unique(HS2_df_country$country_name)
country_choice <- country_choice[order(country_choice)]


## The country_choice is now converted into a dataframe called 'country' which can be used to create the dropdown for the user to select the country of their choice in the app
country <- data.frame(country_choice)

## The number_country dataset is used to calculate the total number of countries in the dataset which will be displayed in one of the blue boxes in the app. 

number_country <- data.frame(country_choice) %>%
  summarise(total = length(country_choice))

## 4. PUR rates by CN8 

## This dataset calculates the PUR rates at CN8 level omitting any NAs (not calculated PURS)
## This dataset is used to create the All_CN8() function that will calculate the lowest PUR for selected country & compare to total average by CN8
## This will effect the reactive text for the CN8 graph that is displayed in the PUR app 


CN8_df <- PUR_exportdata %>%
  group_by(CN8,CN8_desc,year)%>%
  summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100,
            .groups="drop")  %>%
  na.omit(PUR)

##################################### functions ################################################

## function 1 - to create the function for the HS2 graph

calculateHS2PurData <- function(data, selectedCountry, selectedYear) {
  if (selectedCountry != "EU") {
    result <- data %>%
      filter(country_name %in% selectedCountry, year %in% selectedYear) %>%
      group_by(HS2, HS2_desc) %>%
      summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100, NonPUR = 100-PUR, .groups = "drop")
  } else {
    result <- data %>%
      group_by(HS2, HS2_desc) %>%
      filter(year %in% selectedYear) %>%
      summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100, NonPUR = 100-PUR, .groups = "drop")
  }
  return(result)
}

## function 2 - used to calculate the number of cn8 lines eligible for preference for the specific country selected

calculatenumberCN8country<- function(data, selectedCountry, selectedYear) {
  result <- data %>%
    filter(country_name %in% selectedCountry, year %in% selectedYear, Eligible_Trade > 0) %>%
    group_by(CN8) %>%
    mutate(seq = row_number()) %>%
    distinct(CN8, .keep_all = TRUE) %>%
    ungroup() %>%
    summarise(total = sum(seq), .groups = "drop")
  
}
## function 3 - created to calculate the average HS2 PUR available for the selected country

calculatenumberHS2country<- function(data, selectedCountry, selectedYear) {
  result <-  data %>%
    filter(country_name %in% selectedCountry, year %in% selectedYear) %>%
    ungroup() %>%
    summarise(agri_PUR = round(sum(Pref_Trade)/sum(Eligible_Trade)*100,1), .groups = "drop")
  
}

## function 4 - number of average PUR for all agrifood chapters (1-23)

calculatenumberHS2all<- function(data,selectedYear) {
  result <- data %>%
    filter(year %in% selectedYear) %>%
    ungroup() %>%
    summarise(agri_PUR = round(sum(Pref_Trade)/sum(Eligible_Trade)*100,1), .groups = "drop")
  
}

## function 5 - calculated to show the number of cn8 codes that have 100% PUR rates by country

calculateCN8Pur100 <- function(data, selectedCountry, selectedYear) {
  df <- data %>%
    filter(country_name %in% selectedCountry, year %in% selectedYear, Eligible_Trade > 0) %>%
    group_by(CN8, CN8_desc) %>%
    summarise(PUR = round(sum(Pref_Trade)/sum(Eligible_Trade)*100, 2), .groups = "drop")
  
  Percent_df <- df %>%
    filter(PUR == 100) %>%
    summarise(Percentage = round(n()/ nrow(df)*100, 1), .groups = "drop")
  
  return(Percent_df)
}

## function 6 - calculate the total number of CN8 lines that have 100% PUR rates

calculateCN8all100 <- function(data, selectedCountry, selectedYear) {
  result <-  data %>%
    filter(year %in% selectedYear, Eligible_Trade>0) %>%
    group_by(CN8, CN8_desc) %>%
    summarise(PUR = round(sum(Pref_Trade)/sum(Eligible_Trade)*100,2), .groups = "drop") 
  
  Percent_dfEU<- result %>%
    filter(PUR == 100) %>%
    summarise(Percentage = round(n()/ nrow(result)*100,1), .groups = "drop")
  
  return(Percent_dfEU)
}

## function 7- create a HS2 graph function that creates the interactive HS2 graph that shows the PUR/Non PUR rates by chapter

createHS2Plot <- function(HS2_PUR) {
  plot <- ggplotly(ggplot(HS2_PUR, aes(fill = PUR_Type, y = PUR_rate, x = HS2, text = paste0(HS2_desc))) +
                     geom_bar(position = "stack", stat = "identity") +
                     scale_fill_manual(values = c("#A9A9A9", "#159ecc")) +
                     theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90),
                           axis.title.y = element_blank(),
                           legend.title = element_text(size = 10),
                           panel.grid.major = element_blank(),
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank(),
                           axis.line = element_line(colour = "black"),
                           legend.position = "bottom"), tooltip = c("text")) %>%
    layout(legend = list(orientation = "h", x = 0.2, y = -0.1, text = "Total Eligible trade", "Trade utilised"))
  
  return(plot)
}

## function 8 - calculate the bottom CN8 PURs for the specific country selected

getBottomCN8Country <- function(data, selectedCountry, selectedYear) {
  CN8_df_2 <- data %>%
    filter(country_name %in% selectedCountry, year %in% selectedYear) %>%
    group_by(CN8, CN8_desc) %>%
    summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100, .groups = "drop")   %>%
    na.omit(PUR)
  
  Bottom_CN8_country <- CN8_df_2 %>%
    filter(PUR != 0) %>%
    arrange(desc(PUR)) %>%
    tail(10)
  
  return(Bottom_CN8_country)
  
}

## function 9 - calculate the bottom CN8 PURs for all countries

getBottomCN8all <- function(data,selectedYear) {
  CN8_df <- data %>%
    filter(year %in% selectedYear) %>%
    group_by(CN8,CN8_desc)%>%
    summarise(PUR = sum(Pref_Trade)/sum(Eligible_Trade)*100,
              .groups="drop")  %>%
    na.omit(PUR)
  
  Bottom_CN8 <- CN8_df %>%
    filter(PUR != 0) %>%
    arrange(desc(PUR)) %>%
    tail(10)
  return(Bottom_CN8)
  
}

## function 10  - creates the monthly agri PUR data for the monthly graph in summary page

getAgriGraphData <- function(data, selectedCountry, selectedYear) {
  if (selectedCountry != "EU") {
    av_agri_graph <- data %>%
      filter(country_name %in% selectedCountry, year %in% selectedYear) %>%
      group_by(month, country_name) %>%
      summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")
    return(av_agri_graph)
    
  } else {
    av_agri_graph <- data %>%
      filter(year %in% selectedYear) %>%
      group_by(month) %>%
      summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop")
    return(av_agri_graph)
  }
}

## function 11 - creates the Monthly data for the Monthly trends graph

MonthlyHS2graphdata <- function(data, selectedYear, selectedHSCode, selectedCountry) {
  monthly_HS2 <- data %>%
    filter(year %in% selectedYear, HS2 %in% selectedHSCode) %>%
    group_by(month, country_name) %>%
    summarise(agri_PUR = round(sum(Pref_Trade)/sum(Eligible_Trade)*100,1),
              Country = if_else(c(country_name %in% selectedCountry | country_name == "EU"), country_name, "All"),.groups = "drop") %>%
    distinct() %>%
    na.omit()
}

Monthlygraphdata <- function(data, selectedCountry, selectedYear, selectedHSCode) {
  summary_data <- data %>%
    filter(country_name %in% selectedCountry, year %in% selectedYear, HS2 %in% selectedHSCode) %>%
    group_by(month, country_name) %>%
    summarise(agri_PUR = round(sum(Pref_Trade) / sum(Eligible_Trade) * 100, 1), .groups = "drop") %>%
    na.omit()
  
}

## function 12 - creates the monthly graph in the monthly trends tab

MonthlytrendsPlot <- function(data, data_choice, data_EU) {
  gg <- ggplot(data, aes(x = month, y = agri_PUR, group = 1, text = paste0(country_name, " ", agri_PUR, "%"))) +
    geom_point(color = "grey") +
    geom_line(data = data_choice, aes(x = month, y = agri_PUR, group = 1), color = "red") +
    geom_line(data = data_EU, aes(x = month, y = agri_PUR, group = 1), color = "black") +
    theme_classic() +
    theme(axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 90),
          axis.title.y = element_blank()) +
    scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
  
  return(ggplotly(gg, tooltip = c("text")))
}

## function 13 - filters the HS codes for the HS tables

filterPURData <- function(data, country, year1, HSCode) {
  if (length(country) > 1) {
    filtered_data <- data %>%
      filter(HS2 %in% HSCode, country_name %in% country, year %in% year1)
  } else if (country %in% "All") {
    filtered_data <- data %>%
      filter(HS2 %in% HSCode)
  } else {
    filtered_data <- data %>%
      filter(HS2 %in% HSCode, country_name %in% country, year %in% year1)
  }
}

## function 14 - Creates the CN8 graph for the bottom CN8 graph in the summary page

createCN8Plot <- function(data) {
  ggplotly(
    ggplot(data, aes(x = reorder(CN8, PUR), y = PUR,
                     text = paste0(CN8_desc, "<br>", PUR, "%")))
    + geom_bar(stat = "identity", fill = "#159ecc") +
      theme_classic() +
      theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90),
            axis.title.y = element_blank(),
            plot.title = element_text(size = 12, hjust = 0.01)), tooltip = c("text")
  )
}

## function 15 - creates the graph for the monthly trens graph in the summary page

createMonthlyPlot <- function(data) {
  ggplotly(
    ggplot(data, aes(x = month, y = agri_PUR, group = 1, text = paste0(agri_PUR, "%")))
    + geom_line(linetype = "dashed", color = "blue")
    + geom_point(color = "blue")
    + theme_bw()
    + theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90),
            axis.title.y = element_blank())
    + scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
    + scale_y_continuous(limits = c(0, max(data$agri_PUR) + 10)), tooltip = c("text")
  )
}

## function 17 - Total import value for all agrifood products 

## this dataset will display the total imports for all agrifood products (All)

createtotal_exportAll <- function(data,yeartype1, countryEU) {
  
  result <- data %>%
    filter(year %in% yeartype1,country_name %in% countryEU ) %>%
    group_by(year) %>%
    summarise(Total = round(sum(Total_ex)/1000000000,1))
  
}

## function 18 - Total import value for agrifood products by country 

## this dataset will display the total imports for agrifood products by country

createtotal_export_country <- function(data,year1,country1){
  
  result <- data %>%
    filter(year %in% year1, country_name %in% country1) %>%
    group_by(year) %>%
    summarise(Total = round(sum(Total_ex)/1000000000,2))
  
}

##################### Creating a list for the app, adding "All" to my list of HS2 codes for dropdowns #############

## This dataframe is the list of HS2 codes used in the dropdown of the app that the user will select from, side by side each country in the dataset.

HS2_code <- subset(PUR_exportdata, country_name %in% country_choice) %>%
  select(country_name,HS2)

## This is a dataset with the list of HS2 codes

HS2 <- unique(PUR_exportdata$HS2) 

HS2_code <- HS2_code[!duplicated(HS2_code),]

####################################### The app  #################################################

## This creates the header for the PUR app

header <- dashboardHeader(title = "PUR app: EU imports from UK")

sidebar <- dashboardSidebar(
  
  
  ##This is the sidebar in the app, The user can select what page they want to view from here
  
  sidebarMenu(
    
    ##The names of each page (the overview page and the app page)
    
    menuItem("Overview", 
             tabName = "Overview"),
    
    menuItem("The App",
             tabName = "PUR")
    
  ))

body <- dashboardBody(
  ## This is the body of the App - this is where most of the interface is built.
  ## The tab items function will allow you to section out what will be placed in each tab/page of the app - this is for the "Overview' page.
  ## This is the text to explain the app (the user guide for the app)
  tabItems(
    
    tabItem(tabName = "Overview",
            h3(strong("Welcome to the EU imports from UK Preference Utilisation Rate (PUR) app")),
            
            h5("This tool was produced by the Trade Analysis Project Delivery and Support (TAPS) team to present Preference Utilisation Rate (PUR) data for EU imports from the UK."),
            ## This box creates the overline for the box of the text
            box(title = "User Guide", status='primary', solidHeader=TRUE, width="100%", height="100%",
                h5(strong("PUR Information:")),
                
                h5("Although the UK and EU have agreed tariff-free, quota-free access under the TCA, this only applies where the goods meet the relevant Rules of Origin and therefore not all trade between the two parties will qualify for zero tariffs."),
                
                h5("Understanding where preferences are being used (and where they are not), through Preference Utilisation Rate data can inform HMG's efforts to increase trade and inform the work to revisit the TCA."), 
                
                h5("A Preference Utilisation Rate (PUR) reflects the value of goods entering under trade preferences as a share of the total value of goods that were eligible for preference."),
                
                
                
                h5(strong("PUR methodology")),
                br(),
                
                ## This displays the image for the PUR calculation - this is only compatible with the browser - this will not work in the tester page
                withMathJax(
                  helpText("$$PUR =\\frac{Value\\;of\\;preferential\\;imports}
                       {Value\\;of\\;the\\;imports\\;eligible\\;for\\;preferences} x100$$")),
                
                br(),
                
                h5("The data presented in this app covers EU imports from the UK, so the PURs measures the total value of EU imports from UK that entered under a preferential tariff regime, as a proportion of the total value of EU imports from UK that were eligible for preferential tariffs."),
                h5("Imports are considered eligible for a preference (i.e. the denominator) if there is one or more preferential tariffs available for that good from the specified partner country in the month of reporting, and that preferential rate is lower than the MFN tariff that would otherwise apply."),
                h5("Imports are recorded as using their preference (i.e. the numerator) if they were imported under a preferential regime."),
                h5("Imports are excluded from the eligibility total if they entered under conditions where a preferential tariff wouldn't reasonably be used - this includes: "),
                h5("- Imports entering under special processing procedures that would permit goods to enter duty-free or under a reduced rate (i.e. inward or outward processing)."), 
                h5("- Imports where a preference is eligible but entered duty-free under MFN terms due to a measure such as suspensions or non-preferential TRQs."),
                h5("- Imports where the regime under which the good entered the UK is unknown (e.g. due to insufficient information provided on the customs declaration)."),
                
                h5(strong("How to use:")),
                h5(strong("1)"),"Choose to view the PUR rates for the EU as a bloc or individual member states, and the year of interest."),
                h5(strong("2)"), "An overview of the average PUR data for the chosen country is presented."),
                h5(strong("3)"), "If you'd like more detail, select the HS2 Chapter of interest."),
                h5(strong("4)"), "Select the tab for the aggregation level of interest (HS4, HS6 or CN8)."),
                
                h5(strong("Limitations & caveats")),
                h5("Currently, the tool contains EU import from UK data for the period of",strong(em("Jan-Dec 2022 and Jan-Nov 2023."))),
                h5("This tool includes data for agrifood products (HS Chapters 1-23) only."),
                br(),
                
                h5("App built by: Louise Anokye, October 2023"),
                h5("Quality Assured: Katie Earl, January 2024"),
                
                h5(strong(em("If you have any questions or queries, contact Louise Anokye (louise.anokye@defra.gov.uk), Katie Earl (katie.earl@defra.gov.uk) or Molly O'brien (Molly.o'brien@defra.gov.uk) 
             "))))
            
            
    ),
    
    
    ## This tab is the PUR app interface
    tabItem(tabName = "PUR",
            ## This creates the mini side bar who allows the user to select their options in the drop bar
            ## the fluidpage sets the width and height limits of the App page
            
            fluidPage(width = "100%", height = "100%",
                      
                      ## this column sets  out the dropdowns and the dropdown options for the user
                      
                      column(width = 2,
                             
                             selectInput(inputId = "Multiple",
                                         label = "How many countries are you interested in?",
                                         choices = c("One", "Multiple"), selected = "One"),  
                             
                             ## this is dependent on whether the user uses one country or more! - the second dropdown can change from 'choose a country' to 'select countries'. This is dependent on the interactive UI
                             uiOutput(outputId = "Countries"),
                             
                             selectInput(inputId = "year",
                                         label = "Please select a year",
                                         choices = c("2022", "2023"), selected = "2022"),
                             
                             ## HS2 dropdown only appears if you select the tabs that need it
                             conditionalPanel(
                               condition = "input.One == 'HS4' || input.One == 'HS6' || input.One == 'CN8'|| input.One == 'Monthly Trends'",
                               selectInput(inputId = "HSCode",
                                           label = "Select HS2 Code",
                                           choices = NULL,
                                           multiple = T),
                               
                             ),
                             conditionalPanel(
                               condition = "input.One === 'HS4' || input.One === 'HS6' || input.One === 'CN8'",
                               downloadButton("download", "Download"))
                             
                      ),
                      
                      column(width = 10,
                             
                             uiOutput("tabs")
                             
                      )
                      
            ) #close fluid page
            
            
    ) #close tab item PUR bracket
    
    
  ) # close tab item overall bracket
) #close dashboard body




server <- function(input, output, session){
  
  ####################################### Interactive UI  #################################################
  
  
  ### This section changes if you can select one or multiple countries 
  ## This is the reactive function that changes the dropdown once the user has selected one country or multiple. If they choose one country then they will 'choose a country', if multiple is selected then they will 'select multiple'.
  
  observeEvent(input$Multiple,{
    output$Countries <- renderUI({
      
      if(input$Multiple == "One"){
        selectInput(inputId = "Country",
                    label = "Choose a country",
                    choices = c("EU", setdiff(country_choice, "EU")), selected = "EU", ## default selected
                    multiple = F)
        
      }else{  
        
        ## This dropdown appears if the user selects the 'multiple' option in the first dropdown
        selectInput(inputId = "Country",
                    label = "Choose the countries",
                    choices = unique(country_choice), selected = "Austria", ## default selected
                    multiple = T)
        
      }
    })
    
  }) #close observe event
  
  
  ## This selects available HS codes dependent on country/countries selected - 
  ## This picks up the right input country variable to use
  
  HS2_codes <- reactive({
    HS2_code  %>%
      filter(country_name %in% input$Country)
  })
  
  
  ## So user can only choose from the available PUR rates by HS2 for the specific country selected. 
  
  observeEvent(HS2_codes(),{
    
    choice <- unique(HS2_codes()$HS2)
    choice <- choice[order(choice)]
    updateSelectInput(inputId = "HSCode", choices = choice)
    
    
  })
  
  ## Type of tab displayed dependent on whether individual selects one country or more
  output$tabs <- renderUI({
    
    validate(
      need(input$Country, "Please select a country"))
    
    ## If the user selects one country option, then the summary stats page will appear    
    if(input$Multiple =="One"){
      tabsetPanel(id = "One",
                  type = "tabs",
                  
                  
                  
                  ##  Summary Information                         
                  ############################################################################################################## 
                  
                  tabPanel(paste0(input$Country),
                           
                           box(status='primary', solidHeader=TRUE, width="100%", height="100%",
                               
                               fluidRow(width = "100%",
                                        
                                        column(width = 12,
                                               uiOutput("observations"),
                                               valueBoxOutput("agri_value", width = 3),
                                               valueBoxOutput("max_CN8", width = 3),
                                               valueBoxOutput("total_exports", width = 3)
                                               
                                        ),
                                        
                                        column(width = 6,
                                               h4(strong(textOutput("HS2_text_title"))),
                                               textOutput("HS2_text"),
                                               plotlyOutput("HS2_graph")
                                               
                                        ),
                                        
                                        column(width = 6,
                                               h4(strong(textOutput("CN8_text_title"))),
                                               textOutput("CN8_text"),
                                               plotlyOutput("CN8_graph")
                                               
                                        ),
                                        column(width = 12,
                                               h4(strong(textOutput("Monthly_text_title"))),
                                               textOutput("Monthly_text"),
                                               plotlyOutput("Monthly_graph")
                                               
                                        ) 
                                        
                               )## End of fluid Row
                           )## End of main box
                           
                  ),## End of tab
                  
                  ## The 'Treemap' tab will also appear only if the user selects one country in the dropdown options
                  
                  ################# TreeMap #############
                  
                  if(input$Country != "EU"){
                    
                    tabPanel("Treemap",
                             
                             fluidRow(width="100%",
                                      
                                      box(status='info', solidHeader=TRUE, width="100%", height="100%",        
                                          column(width = 7,
                                                 plotOutput("elig_graph")
                                          ),
                                          column(width = 2.5, 
                                                 textOutput("elig_text_1"),
                                                 textOutput("elig_text_2")
                                          ))),
                             fluidRow(width="100%",
                                      box(status='info', solidHeader=TRUE, width="100%", height="100%",
                                          column(width = 7,
                                                 plotOutput("use_graph")
                                          ),
                                          column(width = 2.5, 
                                                 textOutput("use_text_1"),
                                                 textOutput("use_text_2")
                                          ))),
                             fluidRow(width="100%",
                                      box(status='info', solidHeader=TRUE, width="100%", height="100%",
                                          column(width = 7,
                                                 plotOutput("combo_graph")
                                          ),
                                          column(width = 2.5, 
                                                 textOutput("combo_text_1"),
                                                 textOutput("combo_text_2")
                                                 
                                          ))))
                  }else{
                    
                  },
                  
                  
                  ## This displays the HS tables for the PUR rates dependent on the HS2 code the user selects
                  
                  ##  HS4 Information                        
                  ############################################################################################################## 
                  
                  tabPanel("HS4",id="HS4",
                           fluidPage(width = "100%", height = "100%",
                                     
                                     DT::dataTableOutput("HS4_table")
                           )
                  ),
                  
                  
                  ##HS6 Information                        
                  ##############################################################################################################                        
                  
                  tabPanel("HS6",DT::dataTableOutput("HS6_table")),
                  
                  
                  ## CN8 Information                        
                  ############################################################################################################## 
                  
                  tabPanel("CN8",DT::dataTableOutput("CN8_table")),
                  
                  ## Monthly trends tab - EDIT!
                  ############################################################################################################## 
                  tabPanel("Monthly Trends", 
                           box(status='primary', solidHeader=TRUE, width="100%", height="100%",
                               
                               fluidRow(width = "100%",
                                        
                                        column(width = 12,
                                               h4(strong(textOutput("trends_title"))),
                                               textOutput("trends_text"),
                                               plotlyOutput("trends_graph")
                                               
                                        ) # end column       
                               ) # end fluidRow
                           )# end box
                  )# end tabpanel                
      )# end tabset panel
      
    }else{tabsetPanel(id = "One",
                      type = "tabs",
                      
                      
                      ## If the user selects 'multiple' in the first dropdown, then only the HS tables will appear (the summary page will hidden) 
                      
                      ## First Tab - HS4 Information                        
                      ############################################################################################################## 
                      
                      tabPanel("HS4",
                               
                               DT::dataTableOutput("HS4_table")),
                      
                      ## First Tab - HS6 Information                        
                      ##############################################################################################################                        
                      
                      tabPanel("HS6",DT::dataTableOutput("HS6_table")),
                      
                      
                      ## First Tab - CN8 Information                        
                      ############################################################################################################## 
                      
                      tabPanel("CN8",DT::dataTableOutput("CN8_table"))
                      
                      
    )}
    
  })
  
  ####################################### Data functions for summary tab display  #################################################
  
  ## HS2 Graph
  
  ## This is the function that will be used to create the HS graph and will be included in the reactive text that summarises the HS graph in the summary tab
  
  
  HS2_data <- reactive({
    
    HS2_df <- calculateHS2PurData(PUR_exportdata, input$Country, input$year) 
    
  })
  
  all_HS2 <- reactive({
    
    
    All_HS2 <- HS2_df_2 %>%
      filter(ifelse(HS2 == 0,0, HS2 == HS2_data()$HS2[which.min(HS2_data()$PUR)]))
    
    
  })
  
  
  ## This function will calculate the lowest PUR for selected country & compare it to the total average by CN8 
  ## This will be used in the reactive text for the CN8 graph.
  
  all_CN8 <- reactive({
    
    all_CN8 <- CN8_df %>%
      filter(CN8 == CN8_graph_data()$CN8[which.min(CN8_graph_data()$PUR)] )
    
  })
  
  ## This generates the values in the blue boxes in the summary page
  
  ## First Value box showing number of countries in dataset or number of CN8 products available for the selected country
  
  output$observations <- renderUI({
    
    ## if the user selects 'All', then the first blue value box will show the number of countries in the dataset
    
    req(length(input$Country) >0)
    
    if(input$Country == "EU"){
      
      valueBox(value = number_country$total,
               subtitle = "Countries in the dataset (including EU bloc)",
               color = "light-blue",
               width = 3)
      
    }else{
      
      ## If the user selects a specific country, then the first blue value box will show the number of CN8 products available for the selected country
      
      valueBox(value = {
        result <- calculatenumberCN8country(PUR_exportdata, input$Country, input$year)
        
        if(result$total==0) {
          "N/A"
        }else{
          result$total
        }
      },
      subtitle = paste0("of ", input$Country,"'s", " CN8 lines had preference eligible imports from the UK"),
      color = "light-blue",
      width = 3)
      
    }
    
  })
  
  ## Second value box showing the average HS2 PUR for all countries or the selected country 
  
  ## If the user selects a specific country, then the second blue value box will show the average HS2 PUR available for the selected country
  
  output$agri_value <- renderValueBox({
    
    req(length(input$Country) >0)
    
    if(input$Country != "EU"){
      
      valueBox(value = {
        result <- calculatenumberHS2country(PUR_exportdata, input$Country, input$year)
        
        if(is.na(result$agri_PUR)) {
          "N/A"
        }else{
          result$agri_PUR
          
        }
      },              
      subtitle = paste0("average PUR from eligible ", input$Country, 
                        " agri-food products"),
      color = "light-blue")
      
    }else{
      
      ## if the user selects 'EU', then the second blue value box will show the average PUR rate for all agrifood products in the dataset
      
      valueBox(value = {
        
        result <- calculatenumberHS2all(PUR_exportdata,input$year)
        
        result$agri_PUR
      }, 
      
      subtitle = paste0("Average preference utilisation rate across agri-food products"),
      color = "light-blue")
      
    }
    
  }) 
  
  ## Third value box showing number of CN8 codes that have 100% PUR
  
  ## If the user selects a specific country, then the third blue value box will show the number of cn8 codes that have 100% PUR rates by country
  
  output$max_CN8 <- renderValueBox({
    
    req(length(input$Country) >0)
    
    if(input$Country != "EU"){
      
      valueBox(value = {
        
        Percent_df <- calculateCN8Pur100(PUR_exportdata, input$Country, input$year)
        
        if(is.na(Percent_df$Percentage)){
          "N/A"
        }else{
          Percent_df$Percentage
        }
      },
      
      subtitle = paste0("Percent of the lines with a PUR with ",input$Country, " have full utilisation (e.g. a PUR of 100%)"),
      color = "light-blue")
      
      
    }else{
      
      ## if the user selects 'All', then the third blue value box will show the total number of CN8 lines that have 100% PUR rates
      
      valueBox(value = {
        result <-  calculateCN8all100(PUR_exportdata, input$Country, input$year)
        
        
        result$Percentage
        
      },
      subtitle = paste0("Percent of the lines with a PUR have full utilisation (e.g. a PUR of 100%)"),
      color = "light-blue")
      
    }
    
  })
  
  ## fourth green box - total imports for all countries and individual country
  
  output$total_exports <- renderValueBox({
    
    req(length(input$Country) >0)
    
    if(input$Country != "EU"){
      
      valueBox(value = {
        result <- createtotal_export_country(PUR_exportdata,input$year,input$Country)
        
        if(is.na(result$Total)) {
          "N/A"
        }else{
          result$Total
          
        }
      },              
      subtitle = paste0("Total imports value of ", input$Country, 
                        " agri-food products (€bn)"),
      color = "light-blue",
      )
      
    }else{
      
      ## if the user selects 'All', then the second green value box will show the average PUR rate for all agrifood products in the dataset
      
      valueBox(value = {
        
        result <- createtotal_exportAll(PUR_exportdata,input$year, "EU")
        
        result$Total
      }, 
      
      subtitle = paste0("Total import value of agri-food products (€bn)"),
      color = "light-blue",
      )
      
    }
    
  }) 
  
  ####################################### Plotted graphs + text for summary tab display  #################################################
  
  ## Text for CN8 graph
  
  ## If the user selects 'EU', then the following title will appear for the CN8 graph 
  
  output$CN8_text_title <- renderText({
    req(length(input$Country) >0)
    
    if(input$Country == "EU"){
      
      paste0(
        
        "Lowest PUR by CN8 (%)")
      
    }else{
      ## If the user selects a specific country , then the following title will appear for the CN8 graph 
      
      paste0("The lowest PURs for ", input$Country, " by CN8 (%)")
      
    }
    
  })
  
  
  ## If the user selects 'EU', then the following title will appear for the HS2 graph
  
  output$HS2_text_title <- renderText({
    req(length(input$Country) >0)
    
    if(input$Country == "EU"){
      
      paste0(
        
        "Average PUR by HS2 (%)")
      
    }else{
      
      ## If the user selects a specific country , then the following title will appear for the HS2 graph
      
      paste0(
        input$Country, "'s PUR by HS2 (%)")
      
    }  
  })
  
  output$trends_title <- renderText({
    
    req(length(input$Country) >0)
    
    if(input$Country == "EU"){
      
      paste0(
        
        "Average Monthly PURs by HS2 (%) ")
      
    }else{
      
      ## If the user selects a specific country , then the following title will appear for the HS2 monthly graph
      
      paste0(
        input$Country, "'s Average Monthly PURs by HS2 (%)")
      
    }
  })
  
  output$trends_text <- renderText({
    
    req(length(input$Country) >0)
    
    if(input$Country == "EU"){
      
      paste0(
        
        "This graph below represents the monthly average Agri-food PURs for the EU over time.",
        " The BLACK line represents the monthly average PURs for the EU.",
        " The GREY points represent the monthly average PURs for each individual member state.")
      
    }else{
      ## If the user selects a specific country , then the following title will appear for the CN8 graph 
      
      paste0("This graph below represents the monthly average Agri-food PURs for ", input$Country, " over time (%).",
             " The BLACK line represents the monthly average PURs for the EU.",
             " The GREY points represent the monthly average PURs for each individual member state.",
             " The RED line represents the monthly average PURs for the selected individual country.")
      
    }
    
  }) 
  
  output$Monthly_text_title <- renderText({
    
    req(length(input$Country) >0)
    
    if(input$Country == "EU"){
      
      paste0(
        
        "Average PUR rate over time (%)")
    }else{
      ## If the user selects a specific country , then the following title will appear for the HS2 graph 
      
      paste0("Average PUR rate over time by ", input$Country, " over time (%)")
      
    }
    
  }) 
  
  output$Monthly_text <- renderText({
    
    req(length(input$Country) >0)
    
    if(input$Country == "EU"){
      
      paste0(
        
        "The graph below shows the average PUR rate for EU agrifood imports from the UK over the chosen year.")
      
    }else{
      ## If the user selects a specific country , then the following title will appear for the CN8 graph 
      
      paste0("The graph below shows the average PUR rate for agri-food products ", input$Country, " imported from the UK over the chosen year.")
      
    }
    
  }) 
  output$HS2_text <- renderText({
    
    ## If the user selects 'EU', then the following text will accompany the HS2 graph
    ## The reactive text is dependent on the HS2 functions created
    
    req(length(input$Country) >0)
    
    if(input$Country == "EU"){
      
      paste0(
        "The graph below presents the average PUR for for the chosen country by chapter (HS2). ",
        HS2_data()$HS2_desc[which.min(HS2_data()$PUR)], " (Chapter ",  HS2_data()$HS2[which.min(HS2_data()$PUR)], ") has the lowest PUR of ",
        round(min(HS2_data()$PUR, na.rm = T),2),"%. Note: this graph shows the import preference used (blue) as a share of the imports eligible for preference (grey). There are some HS Chapters which are all MFN zero and therefore no imports would be eligible for a preference."
      )
      
    }else{
      ## If the user selects a specific country, then the following text will accompany the HS2 graph
      ## The reactive text is dependent on the HS2 functions created 
      if(nrow(all_HS2())>0){
        
        paste0(
          "The graph below examines ", input$Country, " 's PUR for each available HS2 code. 
             Chapter ", HS2_data()$HS2[which.min(HS2_data()$PUR)], " has the lowest PUR with ",
          round(min(HS2_data()$PUR, na.rm = T),2),"%. ", input$Country, " is ", 
          ifelse(min(HS2_data()$PUR, na.rm = T) < all_HS2()$PUR[all_HS2()$year == input$year], " below ", " above "), 
          "the average PUR in this chapter, which is ", round(all_HS2()$PUR[all_HS2()$year == input$year],2),"%.")
        
      }else{
        paste0("No data available, please view the HS4-CN8 tables to see if there were any imports (regardless of preference)")
      }
    }
    
  })
  
  ## Text for CN8 graph
  
  ## If the user selects 'EU', then the following text will accompany the CN8 graph
  
  output$CN8_text <- renderText({
    req(length(input$Country) >0)
    
    
    if(input$Country == "EU"){
      
      "The graph below presents the CN8 codes with the lowest average PURs (assuming eligible trade exists and the PUR is non-zero)."
      
    }else{
      ## If the user selects 'EU', then the following text will accompany the CN8 graph
      ## The reactive text is dependent on the HS2 functions created  
      
      
      if (nrow(CN8_graph_data())>0){
        ## However, the text will only display if a CN8 graph can be generated - there are examples of a country that does not have any trade reported, therefore the cn8 graph will not generate any data
        
        paste0("The graph below displays the most underutilised preference at the CN8 level for ", 
               input$Country, " (assuming eligible trade exists and the PUR is non-zero). ","CN8 code (", CN8_graph_data()$CN8[which.min(CN8_graph_data()$PUR)],")", " has
             the lowest PUR for ", input$Country," at ", round(min(CN8_graph_data()$PUR),2), "%.", " The average PUR for
             this CN8 code is ", round(all_CN8()$PUR[all_CN8()$year == input$year],2), "%.")  
        
      }else{
        ## so if there is no data plotted in the cn8 graph, then this message will appear        
        paste0("No data available")  
      }  
    }
    
  })
  
  ## This creates the interactive HS2 graph that shows the PUR/Non PUR rates by chapter
  ## this graph is dependent on the HS2 functions created
  ## "HS2_graph" will be displayed in UI interface
  
  output$HS2_graph <- renderPlotly({
    
    req(length(input$Country) > 0)
    
    HS2_PUR <- HS2_data() %>%
      pivot_longer(cols = c("PUR", "NonPUR"), names_to = "PUR_Type", values_to = "PUR_rate")
    
    if (input$Country == "EU") {
      x <- createHS2Plot(HS2_PUR)
      
    } else {
      
      if (all(is.na(HS2_PUR$PUR_rate))) {
        
        return(NULL)  # Return NULL to hide the graph when all values are NA
      } else {
        x <- createHS2Plot(HS2_PUR)
      }
    }
  })
  
  ## CN8 Graph
  ## This creates the function which is used to create the interactive CN8 graph that shows the lowest CN8
  
  CN8_graph_data <- reactive({
    
    req(length(input$Country) >0)
    
    ## If the user selects a specific country, then the CN8 function will calculate the bottom CN8 PURs for that specific country
    
    if(input$Country != "EU"){
      
      getBottomCN8Country(PUR_exportdata,input$Country,input$year)
      
    }else{
      
      ## If the user selects "All", then the CN8 function will calculate the bottom CN8 PURs for all countries
      
      
      getBottomCN8all(PUR_exportdata,input$year)
      
    }
    
  })
  
  ## This creates the interactive CN8 graph  
  output$CN8_graph <- renderPlotly({
    
    ## If there is no data to plot into the graph, then nothing will be displayed in the interface
    ## However, if there is then it will generate the interactive graph
    
    if (nrow(CN8_graph_data())>0){
      
      createCN8Plot(CN8_graph_data()) 
      
    }else{
      
    }
    
  })
  
  ## Create monthly agri-food PUR graphs
  # This extracts the average agri-food PUR over 12 months
  ## create reactive text
  
  Monthly_graph_data <- reactive({
    
    getAgriGraphData(PUR_exportdata,input$Country,input$year)
    
  })
  
  #this creates the interactive graph for the monthly agri-food graph in summary page
  
  output$Monthly_graph <- renderPlotly({
    
    createMonthlyPlot(Monthly_graph_data())
    
  })
  
  ## Create the monthly PUR graphs for the monthly trends tab
  # This extracts the average agri-food PUR by HS code over 12 months
  ## create reactive text
  
  filtered_graph_data <- reactive({
    
    monthly_HS2 <- MonthlyHS2graphdata(PUR_exportdata,input$year,input$HSCode,input$Country)
    Monthly_countrychoice <- Monthlygraphdata(PUR_exportdata, input$Country, input$year, input$HSCode)
    Monthly_countryEU <- Monthlygraphdata(PUR_exportdata, "EU", input$year, input$HSCode)
    
    list(monthly_HS2 = monthly_HS2, Monthly_countrychoice = Monthly_countrychoice, Monthly_countryEU = Monthly_countryEU)
  })
  
  ## graph for monthly PUR by HS code and country
  
  output$trends_graph <- renderPlotly({
    if (nrow(filtered_graph_data()$monthly_HS2) == 0 || nrow(filtered_graph_data()$Monthly_countrychoice) == 0 || nrow(filtered_graph_data()$Monthly_countryEU) == 0) {
      
      return(plotly::plot_ly(x = NULL, y = NULL, type = "scatter", mode = "markers"))
      
    } else {
      
      MonthlytrendsPlot(data = filtered_graph_data()$monthly_HS2,
                        data_choice = filtered_graph_data()$Monthly_countrychoice,
                        data_EU = filtered_graph_data()$Monthly_countryEU)
    }
  })
  
  # Treemap graphs
  
  ## A function is created for each treemap, which will be used to generate the treemaps
  
  itemvar <- function(myval){
    paste0("€",round(myval/1000000,1),"m")
  }
  
  
  treemapdata <- function (mydat1,mycountry1,myyear1,myitem1) {
    myitem1 <- sym(myitem1)
    t <- mydat1 %>%
      filter(country_name %in% mycountry1 & year %in% myyear1) %>%
      #~ the {{}} allows it to group by the variable that you have sent to the function
      group_by(!!myitem1) %>%
      summarise(Value = sum(statvalue),.groups = "drop") %>%
      #~ by renaming the column to a general name (here I've called it item), you can 
      rename("item" = !!myitem1)
    return(t)
  }
  
  output$test <- renderTable({
    treemapdata(Preftype_data,input$Country,input$year,"eligibility_name")
  })
  
  treemap_plots <- function (mydat,mycountry,myyear,myitem) {
    toplot <- treemapdata(mydat,mycountry,myyear,myitem)
    toplot$mylabel <- paste(toplot$item,itemvar(toplot$Value), sep = "\n")
    ggplot(toplot, aes(area = Value, fill= Value, label = mylabel)) +
      geom_treemap()+
      geom_treemap_text(colour ="black", place = "centre", size = 15) +
      scale_fill_gradient(low = "white", high = "lightblue") +
      theme(legend.position = "none")
  }
  
  # Eligibility graph
  output$elig_graph <- renderPlot({ 
    treemap_plots(Preftype_data,input$Country,input$year,"eligibility_name")+
      ggtitle(str_wrap(paste0("Eligibility graph for ", input$Country),60))
  })
  
  # Use data
  output$use_graph <- renderPlot({ 
    treemap_plots(Preftype_data,input$Country,input$year,"use_name")+
      ggtitle(str_wrap(paste0("Use graph for ", input$Country),60))
  })
  
  # Combination data
  output$combo_graph <- renderPlot({ 
    treemap_plots(Preftype_data,input$Country,input$year,"combination_code")+
      ggtitle(str_wrap(paste0("Combo graph for ", input$Country),60))
  })
  
  
  # Eligibility data
  ## This function will create a dataframe for based on the selected option and list all the preferences they are eligible for
  
  elig_data <- reactive({
    
    Country_pref <- Preftype_data %>% 
      filter(country_name %in% input$Country & year %in% input$year)
    
    Country_elig <- Country_pref %>%
      group_by(eligibility_name) %>%
      summarise(Value = sum(statvalue), .groups = "drop") 
    
    return(Country_elig)
    
  })
  
  # Text for treemaps
  
  ## This generates the text for all the different treemaps
  ## The 'text 2' for all treemaps will print out multiple versions depending on how many values are in the data set
  ## e.g. if country A has 3 different trade preferences they are eligible for, then the text will repeat line 1039, three times but with each preference 
  
  #elig text 1
  
  output$elig_text_1 <- renderText({
    
    paste0("The treemap shows which import preferences ", input$Country, " offer for which the UK is eligible.")
    
  })
  
  #elig text 2
  
  output$elig_text_2 <- renderText({
    
    paste0("From ", input$Country,", the UK is eligible for ", treemapdata(Preftype_data,input$Country,input$year,"eligibility_name")$item ,". ", input$Country,"'s", " imports from the UK under this preference was €", format(treemapdata(Preftype_data,input$Country,input$year,"eligibility_name")$Value, big.mark = ","), ".")
  })
  
  
  #use text 1
  
  output$use_text_1 <- renderText({
    
    paste0("The treemap shows the import preferences ", input$Country," used when importing from the UK.")
    
  })
  
  #use text 2
  
  output$use_text_2 <- renderText({
    
    paste0(input$Country, " used ", treemapdata(Preftype_data,input$Country,input$year,"use_name")$item ," when importing from the UK. ", input$Country,"'s imports from the UK under this preference was €", format(treemapdata(Preftype_data,input$Country,input$year,"use_name")$Value, big.mark = ","), ".")
    
  })
  
  # Combo text 1
  
  output$combo_text_1 <- renderText({
    
    paste0("The treemap shows the combination of import preferences offered and used by ", input$Country, " when importing from the UK.")
    
  })
  
  #Combo text 2
  
  output$combo_text_2 <- renderText({
    paste0(input$Country, " used ", treemapdata(Preftype_data,input$Country,input$year,"combination_code")$item ,". ",input$Country,"'s", " imports from the UK under this preference was € ", format(treemapdata(Preftype_data,input$Country,input$year,"combination_code")$Value, big.mark = ","), ".")
    
  })
  
  ## HS4 Tab 
  ########################################################################################################
  
  ## This is for the tabs that display the PUR rates in data tables
  
  ## This is a function that will be used to generate the Hs4 tables + to used for the final download sheets
  
  HS4_data <- reactive({
    ## If the user selects "multiple", the summary tab is hidden and they are asked to select a country
    
    validate(
      need(input$Country, "Please select a country")
      
    )
    ## If the user selects "All" and HS codes of choice, then it will display the following data from this dataset
    ## This will display all the PUR rates for the HS codes selects and each individual country
    
    HS4_df <- filterPURData(PUR_exportdata, input$Country, input$year, input$HSCode)
    
    HS4_df <- HS4_df %>%
      group_by(HS4, country_name) %>%
      mutate(Pref_Trade = sum(Pref_Trade, na.rm = T),
             Eligible_Trade = sum(Eligible_Trade), na.rm =T,
             Total_ex = sum(Total_ex), na.rm = T,
             PUR = round((Pref_Trade/Eligible_Trade)*100,0)) %>%
      
      
      ## This selects the following columns that will be displayed in the tables and excludes any duplications     
      select (country_name,HS4, HS4_desc,Pref_Trade,Eligible_Trade,PUR,Total_ex)
    
    HS4_df <- HS4_df[!duplicated(HS4_df),]
    
  })
  
  ## HS4 producing table 
  ## This creates the tables generated in the tabs  
  
  output$HS4_table <- DT::renderDataTable({
    
    data_HS4 <- HS4_data()
    
    data_HS4$Pref_Trade <- format(data_HS4$Pref_Trade, 
                                  big.mark = ",", big.interval = 3)
    
    data_HS4$Eligible_Trade <- format(data_HS4$Eligible_Trade, 
                                      big.mark = ",", big.interval = 3)
    
    data_HS4$Total_ex <- format(data_HS4$Total_ex, 
                                big.mark = ",", big.interval = 3)
    
    names(data_HS4)[names(data_HS4) == "country_name"] <- "Country Name"
    names(data_HS4)[names(data_HS4) == "HS4_desc"] <- "HS4 Description"
    names(data_HS4)[names(data_HS4) == "Pref_Trade"] <- "Preferential Imports €"
    names(data_HS4)[names(data_HS4) == "Eligible_Trade"] <- "Eligible Imports €"
    names(data_HS4)[names(data_HS4) == "Total_ex"] <- "Total Imports €"
    names(data_HS4)[names(data_HS4) == "PUR"] <- "PUR (%)"
    
    
    datatable(data_HS4, 
              rownames = F, filter ="top",
              options = exprToFunction(list(searching = T, paging = F, dom = "Bfrtip",
                                            scrollX = "100%", scrollY = "100%")), caption = "'Total Imports' reflects the EU's imports of these codes from the UK, regardless of the eligibility or use of a preference regime, e.g. all imports.
                                            'Eligible Imports' reflects the value of EU imports from the UK that were eligible for a tariff preference.
                                            'Preferential Imports' reflects the value of EU imports from the UK that were actually imported under a tariff preference (e.g. used the preference).
                                            'PUR (%)' reflects the Preference Utilisation rate, e.g. value of EU imports from the UK using preference divided by the value of EU imports from the UK that was eligible for a preference.
                                             It is important to note that a) if the product has a tariff of MFN zero, then no preference exists and therefore there will be no eligible trade, and b) these values are presented in Euros.") %>%
      formatStyle(columns = c(4:7), textAlign = "right")
    
    
  })
  
  ## HS6 Tab
  #####################################################################################################
  
  ## HS6 Table filtering data
  
  ## This is a function that will be used to generate the Hs6 tables + to used for the final download sheets
  
  
  HS6_data <- reactive({
    
    ## If the user selects "multiple", the summary tab is hidden and they are asked to select a country
    
    validate(
      need(input$Country, "Please select a country")
      
    )
    
    ## If the user selects "All" and HS codes of choice, then it will display the following data from this dataset
    ## This will display all the PUR rates for the HS codes selects and each individual country
    
    HS6_df <- filterPURData(PUR_exportdata, input$Country, input$year, input$HSCode)
    
    HS6_df <- HS6_df %>%
      group_by(HS6, country_name) %>%
      mutate(Pref_Trade = sum(Pref_Trade, na.rm = T),
             Eligible_Trade = sum(Eligible_Trade), na.rm =T,
             Total_ex = sum(Total_ex), na.rm = T,
             PUR = round((Pref_Trade/Eligible_Trade)*100,0)) %>%
      
      
      ## This selects the following columns that will be displayed in the tables and excludes any duplications     
      
      select (country_name,HS6, HS6_desc,Pref_Trade,Eligible_Trade,PUR,Total_ex)
    
    HS6_df <- HS6_df[!duplicated(HS6_df),]
    
    
    ## If they select a specific country and HS code, then the table will only display PUR rates for that country/HS code
    ## the same process as above but filtered for the specific country of choice
    
    
  })
  
  
  ## HS6 producing table
  
  ## This creates the tables generated in the tabs
  
  output$HS6_table <- DT::renderDataTable({
    
    data_HS6 <- HS6_data()
    
    data_HS6$Pref_Trade <- format(data_HS6$Pref_Trade, 
                                  big.mark = ",", big.interval = 3)
    
    data_HS6$Eligible_Trade <- format(data_HS6$Eligible_Trade, 
                                      big.mark = ",", big.interval = 3)
    
    data_HS6$Total_ex <- format(data_HS6$Total_ex, 
                                big.mark = ",", big.interval = 3)
    
    names(data_HS6)[names(data_HS6) == "country_name"] <- "Country Name"
    names(data_HS6)[names(data_HS6) == "HS6_desc"] <- "HS6 Description"
    names(data_HS6)[names(data_HS6) == "Pref_Trade"] <- "Preferential Imports €"
    names(data_HS6)[names(data_HS6) == "Eligible_Trade"] <- "Eligible Imports €"
    names(data_HS6)[names(data_HS6) == "Total_ex"] <- "Total Imports €"
    names(data_HS6)[names(data_HS6) == "PUR"] <- "PUR (%)"
    
    
    datatable(data_HS6, 
              rownames = F, filter ="top",
              options = exprToFunction(list(searching = T, paging = F, dom = "Bfrtip",
                                            scrollX = "100%", scrollY = "100%")),caption = "'Total Imports' reflects the EU's imports of these codes from the UK, regardless of the eligibility or use of a preference regime, e.g. all imports.
                                            'Eligible Imports' reflects the value of EU imports from the UK that were eligible for a tariff preference.
                                            'Preferential Imports' reflects the value of EU imports from the UK that were actually imported under a tariff preference (e.g. used the preference).
                                            'PUR (%)' reflects the Preference Utilisation rate, e.g. value of EU imports from the UK using preference divided by the value of EU imports from the UK that was eligible for a preference.
                                              It is important to note that a) if the product has a tariff of MFN zero, then no preference exists and therefore there will be no eligible trade, and b) these values are presented in Euros.") %>%
      formatStyle(columns = c(4:7), textAlign = "right")
    
    
  })
  
  ## CN8 Tab
  ###############################################################################################
  
  ## CN8 Table 
  
  ## This is a function that will be used to generate the Hs6 tables + to used for the final download sheets
  
  CN8_data <- reactive({
    
    ## If the user selects "multiple", the summary tab is hidden and they are asked to select a country
    
    validate(
      need(input$Country, "Please select a country")
    )
    
    ## If the user selects "All" and HS codes of choice, then it will display the following data from this dataset
    ## This will display all the PUR rates for the HS codes selects and each individual country    
    
    CN8_df_table <- filterPURData(PUR_exportdata, input$Country, input$year, input$HSCode)
    
    
    CN8_df_table <- CN8_df_table %>%    
      group_by(CN8, country_name)%>%
      mutate(Pref_Trade = sum(Pref_Trade),
             Eligible_Trade = sum(Eligible_Trade),
             Total_ex = sum(Total_ex),
             PUR = round(Pref_Trade/Eligible_Trade*100,0)) %>%
      
      ## This selects the following columns that will be displayed in the tables and excludes any duplications     
      
      select (country_name,CN8, CN8_desc, Pref_Trade,Eligible_Trade,PUR,Total_ex)
    
    CN8_df_table <- CN8_df_table[!duplicated(CN8_df_table),]
    
    
    ## If they select a specific country and HS code, then the table will only display PUR rates for that country/HS code
    ## the same process as above but filtered for the specific country of choice
    
  })
  
  ## CN8 Output
  
  output$CN8_table <- DT::renderDataTable({
    
    data_CN8 <- CN8_data()
    
    data_CN8$Pref_Trade <- format(data_CN8$Pref_Trade, 
                                  big.mark = ",", big.interval = 3)
    
    data_CN8$Eligible_Trade <- format(data_CN8$Eligible_Trade, 
                                      big.mark = ",", big.interval = 3)
    
    data_CN8$Total_ex <- format(data_CN8$Total_ex, 
                                big.mark = ",", big.interval = 3)
    
    names(data_CN8)[names(data_CN8) == "country_name"] <- "Country Name"
    names(data_CN8)[names(data_CN8) == "Pref_Trade"] <- "Preferential Imports € "
    names(data_CN8)[names(data_CN8) == "Eligible_Trade"] <- "Eligible Imports €"
    names(data_CN8)[names(data_CN8) == "Total_ex"] <- "Total Imports €"
    names(data_CN8)[names(data_CN8) == "PUR"] <- "PUR (%)"
    names(data_CN8)[names(data_CN8) == "CN8_desc"] <- "CN8 Description"
    
    datatable(data_CN8, 
              rownames = F, filter ="top",
              options = exprToFunction(list(searching = T, paging = F, dom = "Bfrtip",
                                            scrollX = "100%", scrollY = "100%")),caption = "'Total Imports' reflects the EU's imports of these codes from the UK, regardless of the eligibility or use of a preference regime, e.g. all imports.
                                            'Eligible Imports' reflects the value of EU imports from the UK that were eligible for a tariff preference.
                                            'Preferential Imports' reflects the value of EU imports from the UK that were actually imported under a tariff preference (e.g. used the preference).
                                            'PUR (%)' reflects the Preference Utilisation rate, e.g. value of EU imports from the UK using preference divided by the value of EU imports from the UK that was eligible for a preference.
                                              It is important to note that a) if the product has a tariff of MFN zero, then no preference exists and therefore there will be no eligible trade, and b) these values are presented in Euros.") %>%
      formatStyle(columns = c(4:7), textAlign = "right")
    
  }) 
  
  ## Download button
  
  ## This creates the download button + allows the user to download their generated tables
  
  output$download <- downloadHandler(
    
    filename = function(){
      
      if(input$Multiple == "One"){
        
        paste0(input$Country,"_PUR_",Sys.Date(),".xlsx")
        
      }else{
        
        paste0(input$Multiple,"_PUR_",Sys.Date(),".xlsx")
        
      }
      
    },
    content = function(file){
      
      wb <- createWorkbook()
      addWorksheet(wb,"HS4")
      writeData(wb, sheet = "HS4", x = HS4_data())
      
      addWorksheet(wb, "HS6")
      writeData(wb, sheet = "HS6", x = HS6_data())
      
      addWorksheet(wb, "CN8")
      writeData(wb, sheet = "CN8", x = CN8_data())
      
      saveWorkbook(wb, file = file)
      
    }
    
  )
  
}


ui <- dashboardPage(skin = "blue",
                    
                    header, sidebar, body)    

shinyApp(ui = ui, server = server)
