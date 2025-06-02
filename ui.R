# Advanced Programming in R [2400-DS1APR] Final Project by:
# I Putu Agastya Harta Pratama - 472876 - i.pratama@student.uw.edu.pl
# Tsoi Kwan Ma - 476914 - t.ma@student.uw.edu.pl



ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "minty", "navbar-brand-color" = "#78c2ad", "nav-tabs-link-active-bg" = "#78c2ad", "nav-tabs-border-color" = "#78c2ad", "nav-tabs-link-active-color" = "white"),
  br(),
  titlePanel("Europe Carbon Emissions Overview"),
  br(),
  tabsetPanel(
  
    # 1. Overview tab
    tabPanel("Overview",
             br(),
             h2("General Overview"),
             p(" "),
             p("This interactive dashboard provides a comprehensive analysis of carbon emissions across European countries from 2010 to 2022. 
               It is designed to help explore emissions trends across countries, sectors, and over time, enabling deeper insights into how different regions and industries contribute to overall greenhouse gas emissions. 
               It would be useful for policymakers, researchers, or environmentally conscious citizens, specifically offering valuable data visualization to inform decisions and discussions around climate change mitigation.
               The data is sourced from verified emission records and covers 29 European countries, across 22 distinct economic sectors, for instance agriculture, manufacturing, construction, electricity and gas, households activities, financial and insurance activities, education, and so on. 
               It is allowed to interact with the dashboard through multiple filters and visual tools to investigate both general trends and granular breakdowns of carbon emissions.", style = "text-align: justify;"),
             
             p("Additionally, in order to support future analysis on Europe carbon smissions, it is allowed to upload specific carbon emissions statistics from Eurostat when new data available, 
               and it is not limited to the original data source used in this interactive dashboard. It is hoped that this dynamic carbon emissions analysis tool can generate meaningful insights 
               by visualizations and customise the plugged in data based on needs. However, it has to be in mind that the raw and pure data must be downloaded from Eurostat database called ", 
               tags$a(
                 href = "https://ec.europa.eu/eurostat/databrowser/view/env_ac_co2fp/default/table?lang=en", "Carbon Dioxide Emission Footprints", target = "_blank"),
               '. Further details and requirements can be found on the tab "New Data Upload".',
               style = "text-align: justify;"),
             
             hr(),
             p("Below provides a high-level overview of emission trends. The total carbon emissions have changed over time for either the entire region or for a selected country. It is significant to identify the macro trends, 
               such as whether emissions are increasing, decreasing, or remaining constant.", style = "text-align: justify;"),
             hr(),
             selectInput("overviewCountry", "Select Country or Total:", choices = NULL),
             plotOutput("overviewPlot"),
             
             p(" "),
             hr(),
             p("Sometimes, each country has different major contributors to carbon emissions in a given year, and each of them contributes relatively to the others. Therefore, 
               it is especially crucial to aware the composition of emissions by country for a selected year. It is also beneficial to get a clear geographical distribution of carbon emission intensity.", style = "text-align: justify;"),
             hr(),
             fluidRow(
               column(6,
                      h4("Overview Map of Carbon Emission in European Countries",
                         style = "margin-top:0; margin-bottom:5px; font-size:1.2em;"),
                      leafletOutput("overviewMap", width = "100%", height = "500px"),
               ),
               
               column(6,
                      h4("Emission Composition by Country",style = "margin-top:0; margin-bottom:5px; font-size:1.2em;"),
                      uiOutput("pieYearUI"),
                      plotlyOutput("piePlotly", height = "400px")
               )
             ),
             hr(),
             p("Carbon emissions are closely linked to environmental policy instruments such as carbon taxes and Emissions Trading Systems (ETS). These mechanisms are designed to internalize the environmental cost of greenhouse gas emissions by placing a price on carbon, 
               thereby encouraging emitters to reduce their carbon footprint. A carbon tax sets a direct price on carbon by defining a tax rate on greenhouse gas emissions or on the carbon content of fossil fuels. In contrast, ETS — also known as cap-and-trade systems — 
               set a cap on total emissions and allow industries to buy and trade emission allowances within that cap. Several European countries have implemented one or both of these strategies to meet climate goals and reduce overall emissions. If interested, and for more 
               information on global carbon pricing initiatives, including those in Europe, can be refered and explored to the ", 
               tags$a(
                 href = "https://carbonpricingdashboard.worldbank.org/", "World Bank's Carbon Pricing Dashboard", target = "_blank"),
               ".",
               style = "text-align: justify;")
    ),
    
    # 2. Carbon Emission Details tab
    tabPanel("Carbon Emission Details",
             br(),
             h2("Emissions Details"),
             p(" "),
             p("Here provides more detailed insights into emissions, focusing on economic sectors and specific countries. 
               There are trends over time by sector. With a specific country and either view all sectors collectively or focus on a particular sector, it is easier to understand which sectors drive emissions in a country and how their contributions have changed over time.", 
               style = "text-align: justify;"),
             p(" On the other hand, by comparing the carbon emissions by sector between two selected years, the absolute changes in emissions being the immediate cues to the performance of each sector, 
               it is not difficult to spot which sectors have made progress in reducing emissions and which have seen increases.", style = "text-align: justify;"),
             hr(),
             fluidRow(
               column(4,
                      # no wellPanel here
                        selectInput("country_sector_tab", "Select Country:", choices = NULL, width = "100%"),
                        selectInput("sector_select", "Select Sector:", choices = NULL, width = "100%"),
                        sliderInput("yearRangeSector", "Year Range:", min = 2010, max = 2022, value = c(2010, 2022), step = 1, sep = "", width = "100%")
                      
               ),
               column(8,
                      plotOutput("linePlotSector")
               )
             ),
             
             br(),
             hr(),
             fluidRow(
               column(8,
                      plotOutput("barPlotSector")
               ),
               column(4,
                      # no wellPanel here
                        selectInput("country_sector_tab_2", "Select Country:", choices = NULL, width = "100%"),
                        sliderInput("yearRangeSector_2", "Year Range:",
                                    min = 2010, max = 2022, value = c(2010, 2022), step = 1, sep = "", width = "100%")
                      
               )
             )
    ),
    
    # 3. Raw Data Access tab
    tabPanel("Raw Data Access",
             br(),
             h2("Explore & Download Filtered Emissions Data"),
             p(" "),
             p("Please use the sidebar to filter and download the raw dataset that powers the dashboard. It is able to filter the dataset by selecting specific or interested sector(s), country(ies), and year(s). 
               The matching data can be downloaded as CSV file. It is important to note that the unit measure for carbon emission is by ", strong("thousand tonnes"), ".", style = "text-align: justify;"),
             hr(),
             sidebarLayout(
               sidebarPanel(
                 selectInput("filter_sector", "Select Sector(s):", choices = NULL, multiple = TRUE),
                 selectInput("filter_country", "Select Country(ies):", choices = NULL, multiple = TRUE),
                 selectInput("filter_year", "Select Year(s):", choices = 2010:2022, multiple = TRUE),
                 textInput("file_name", "Filename to save:"),
                 actionButton("save", "Save CSV", class = "btn btn-outline-primary")
               ),
               mainPanel(
                 h4("Filtered Data Preview"),
                 DT::dataTableOutput("tab")
               )
             )),
    
    
    # 4. Upload New User Data
    tabPanel("Upload New Data",
             br(),
             h2("New Emissions Data Upload"),
             p(" "),
             p("It is able to upload new carbon emissions dataset and explore it interactively using this dashboard. The data ", strong("must"), " be downloaded 
             from Eurostat database called ", 
               tags$a(
                 href = "https://ec.europa.eu/eurostat/databrowser/view/env_ac_co2fp/default/table?lang=en", "Carbon Dioxide Emission Footprints", target = "_blank"), ".", 
               "To ensure the data works correctly and successfully with the dashboard, there are several requirements ", strong("must"), " be satisfied:", style = "text-align: justify;"),
             p("1. The file must be a CSV file (maximum 70 MB);"),
             p('2. There are 5 columns must be included in the data: "National accounts indicator (ESA 2010)", "Statistical classification of economic activities in the European Community (NACE Rev. 2)", "Country of origin", 
               "TIME_PERIOD", and "OBS_VALUE";', style = "text-align: justify;"),
             p("3. The European countries are limited to Austria, Belgium, Bulgaria, Switzerland, Cyprus, Czechia, Germany, Denmark, 
               Estonia, Greece, Spain, Finland, France, Croatia, Hungary, Ireland, Italy, Lithuania, Luxembourg, Latvia, Malta, Netherlands, Norway, Poland, Portugal, Romania, Sweden, Slovenia, Slovakia, and the United Kingdom.", 
               style = "text-align: justify;"),
             p('After selecting the CSV file using the upload field, pleace click "Validate & Preview" to check the format and contents. 
               If the data passes validation check, please click "Apply Data" to load it into the dashboard and ultimately all charts, maps, and filters will be reflected.', style = "text-align: justify;"),
             hr(),
             fileInput("uploadFile", "Select CSV File",
                       accept = c(".csv")),
             actionButton("validateUpload", "Validate & Preview", icon = icon("check")),
             uiOutput("applyDataUI"), 
             br(), br(),
             uiOutput("uploadMeta"),
             uiOutput("uploadMessage"),        # for success or error messages
             DT::dataTableOutput("uploadPreview")
    )
  )
)
