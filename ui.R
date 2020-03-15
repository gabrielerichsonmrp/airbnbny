# HEADER ---------------------------------------------------------------------------------------
header <- dashboardHeader(
    title = span(img(src = "icon_yellow_100.png", height = 35), "AirbnbNY"),
    titleWidth = 260
)


# SIDEBAR ---------------------------------------------------------------------------------------
sidebar <- dashboardSidebar(
    width = 260,
    
    sidebarMenu(
        menuItem("Airbnb Newyork!", tabName = "menu_map_airbnb", icon = icon("search")),
        menuItem("Distribution & Growth", tabName = "menu_exploratory", icon = icon("dashboard")),
        menuItem("Datatable", tabName = "menu_datatable", icon = icon("list")),
        menuItem("About", tabName = "menu_about", icon=icon("question"))
        
    )
)


# BODY -------------------------------------------------------------------------------------------------------
body <- dashboardBody(
    tags$head(
        
        includeCSS(file.path('www', path = "mycss.css")),
        includeCSS(file.path('www', path = "map_styles.css")),

        tags$head(tags$style(HTML('.info-box {min-height: 60px;}
                                  .info-box-icon {
                                        height: 60px; 
                                        width: 60px; line-height: 60px; 
                                        font-size: 24px;
                                  } 
                                  .info-box-content {
                                        font-size: 50%; 
                                        padding-top: 10px; 
                                        padding-bottom: 4px; 
                                        margin-left: 60px;
                                  }
                                  .small-box{
                                    min-height: 60px;
                                    border-radius: 2px;
                                    margin-bottom: 10px;
                                    background-color: #4a4a4a!important;
                                    
                                   }
                                  .small-box>.inner {padding: 5px;}
                                   .small-box h3 {
                                        font-size: 20px;
                                        font-weight: 700;
                                        margin: 0 0 4px 0;
                                        white-space: nowrap;
                                        padding: 0;
                                        text-align: center;
                                   }
                                    .small-box p {
                                        font-size: 14px;
                                        margin: 0 0 2px;
                                        text-align: center;
                                    }
                                  '
                                  
                                  
        )),
        tags$link(rel = "icon", 
                  type = "image/png", 
                  href = "icon_yellow_100.png")
        )
    ),
    useShinyjs(),
    tabItems(
        tabItem(
            tabName = "menu_map_airbnb",
            div(
                class="outer",
                # If not using custom CSS, set height of leafletOutput to a number instead of percent
                leafletOutput("map_airbnb", width="100%", height="100%"),
                tags$div(id="cite",
                         'Compiled for ', tags$em('ALGORITMA-Capstone Data Visualization'), ' by Gabriel Erichson (2020).'
                )
            ),
            div(
                class="controls",
                style="margin-left:40px; max-width: 200px"
                
            ),
            
            absolutePanel(
                id = "controlspanel", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 80, left = "auto", right = 20, bottom = "auto",
                width = 330, height = "auto",
                br(),
                p(class="title","Airbnb NY Distribution"),
                selectInput("inp_map_neighgroup", "Neighbourhood Group:", airbnb.neighbourhood_group, selected = "All"),
                sliderInput(
                    inputId = "inp_map_price",
                    label = "Prices in $:",
                    min= 0,
                    max= max(airbnb$price_per_night),
                    value=c(min(airbnb$price_per_night),max(airbnb$price_per_night)),
                    step = 50 
                ),
                numericInput("inp_map_night", "Minimum Night (Max:1250):",value=1,min = 1,max = 1250),
                tags$div(
                    id="my-paragraph",
                    textOutput("out_map_description")
                ),
                fluidRow(
                    valueBoxOutput(
                        outputId = "out_map_max_price"
                    ),
                    valueBoxOutput(
                        outputId = "out_map_avg_price"
                        
                    ),
                    valueBoxOutput(
                        outputId = "out_map_min_price"
                    )
                ),
                fluidRow(
                    plotlyOutput(outputId ="out_map_plot_room",height = 175)
                )
            )
            
        ),
        tabItem(
            tabName = "menu_exploratory",
            fluidRow(
                column(
                    width=6,
                    selectInput(
                        inputId = "inp_neighbourhood_group_growth",
                        label = "Neighbourhood Group:",
                        choices = levels(airbnb$neighbourhood_group),
                        selected = c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
                        multiple = T,
                        width = 600
                    )
                )
            ),
            highchartOutput("out_hc_room",height = 420),
            br(),
            plotlyOutput("out_room_growth", height = 270),
            fluidRow(
               
                    height = 330,
                
                    column(
                   
                        width=2,
                        selectInput("inp_popular_group", "Neighbourhood Group:", airbnb.neighbourhood_group, selected = "Bronx"),
                        dateRangeInput('date_range_review',
                                       label = 'Date range:',
                                       start = min.date$last_review, end = max.date$last_review
                        )
                    ),
                    column(
                        width=5,
                        plotlyOutput("out_popular_group",height = 300)
                    ),
                    column(
                        width=5,
                        plotlyOutput("out_popular_neighbourhood",height = 300)
                    )
                
            )
          
            
        ),
        tabItem(
            tabName = "menu_datatable",
            h3("Airbnb Newyork Datatable"),
            fluidRow(
                column(
                    width=5,
                    selectInput(
                        inputId = "inp_neighbourhood_group",
                        label = "Choose Neighbourhood group:",
                        choices = levels(airbnb$neighbourhood_group),
                        selected = c("Bronx","Brooklyn","Manhattan","Queens","Staten Island"),
                        multiple = T,
                        
                        width = 600
                    )
                ),
                column(
                    width=5,
                    radioButtons(
                        inputId = "rb_room_type",
                        label = "Room Type:",
                        choices = c("All" = "All", "Entire home" = "Entire home",
                                    "Private room" = "Private room", "Shared room"="Shared room"),
                        inline = TRUE,
                        selected = "All"
                        
                    )
                ),
                
                column(
                    width=2,
                    br(),
                    downloadButton(
                        "download_data", 
                        "Download Data"
                    )
                )
                
            ),
            
            DT::dataTableOutput("table_airbnb_ny", height = 600)
        ),
        tabItem(
            tabName = "menu_about",
            box(
                includeMarkdown("README.md")
            )
        )
        
    )
)


# LOAD UI -----------------------------------------------------------------------------------------
ui <- tagList(
    dashboardPage(
        skin = "black",
        title = "AirbnbNY",
        header,
        sidebar,
        body
    )
)

