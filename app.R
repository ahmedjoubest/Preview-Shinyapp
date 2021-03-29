
# 1.1 PACKAGES ----------------------------------------------------------

#detach("package:plyr", unload=TRUE) must be unloaded : unwanted interference with {dplyr}

library(scroller) # to install : remotes::install_github("lgnbhl/scroller")

library(shiny)
library(shinydashboard)

library(shinyBS)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)

library(stringr)
library(lubridate)
library(dplyr)
library(highcharter)
library(data.table)
library(DT)

# feather : Faster than RDS if not compromised by storage
# More info : https://appsilon.com/fast-data-loading-from-files-to-r/
library(feather)

library(sp)
library(leaflet)

# 1.2 Load Data + functions  --------------------------------------------

source("helpers.R")
Data <- read_feather("Data-Decathlon.feather")
  
# 2.1 HEADER ------------------------------------------------------------

header <- 
  dashboardHeader(title = "Sales Analysis")

# 2.2 SIDEBAR -----------------------------------------------------------

sidebar <- 
  dashboardSidebar(
    
    sidebarMenu(
      id = "sidebar",
      menuItem("Main Dashboard", tabName = 'dashboard', icon = icon('dashboard')),
      menuItem("Filtered data", tabName = 'Filtered_Data', icon = icon('table'))
    ),
    
    br(),
    
    h4(tags$u("Filter Data:"),align = "center"),
    
    ### Remark
    h6(tags$u("note"),": please filter Data everytime from top to bottom, hover 
       filters fore more info.", style='margin-left: 1.4em; color:red;'),
    
    ### Stores filter
    tipify(
      pickerInput(
        inputId = "store_names",label = "Stores",
        choices = unique(Data$store_name),
        multiple = T,
        selected = unique(Data$store_name),
        options = pickerOptions(actionsBox = T, liveSearch = T, size = 5,
                                dropdownAlignRight = T)
        ),
      "Dependence info: this filter acts on {Items} filter",
      "right"
      ),
    
    ### Prices range filter
    tipify(
      sliderInput(
        inputId = "prices_range",
        label = "Range of item price in Euro",
        min = round(min(Data$prices))-1,
        max = round(max(Data$prices))+1,
        value = c(30,760)
        ),
      "Dependence info: this filter acts on {Items} filter",
      "right"
      ),
    
    ### Items filter
    tipify(
      pickerInput(
        inputId = "item_names",
        label = "Items",
        choices = unique(Data$item_name),
        selected = unique(Data$item_name)[c(1:10)],
        multiple = T,
        options = list(`actions-box` = T,`live-search` = T,size = 5)
        ),
      "Dependence info: this filter does NOT act on any filter! And get updated each time the first two filters are modified to make sure that you do not miss items",
      "right"
      ),
    
    ### Dates range filter
    tipify(
      dateRangeInput(
        'dates_range',
        'Date range (2012-2022)',
        start = min(Data$the_date_transaction),
        end = ymd("2012-04-30")
        ),
      "Dependence info: this filter is independent",
      "right"),
    
    br(),
    
    div(
      align = 'center',
      
      ### Submit button
      tipify(
        actionBttn(
          inputId = "submit",
          label = "Filter",style = "stretch",
          color = "primary",
          icon = icon("sliders")
          ),
        "Apply filters",
        "top"),
        
        br(),br(),br(),br(),br(),
        
        ### Scroll down button
        a(tags$b("Scroll Down",style="color: black;"),
          type="button",class = "btn btn-primary",
          href = "#.down", icon("arrow-down",class = "arrow_down"))
    )
  )


# 2.3 DASHBOARD BODY ----------------------------------------------------

body <- dashboardBody( 

  ### Set scrolling options
  scroller::use_scroller(animationLength = 265),
  
  ### Import styling CSS file
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "css4.css") 
  ),
  
  ### Set logo 
  tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<center> <img src = "logo.jpg" height = 51></center>\');
      })
     ')),
  
  
  tabItems(
    
    # 2.4. MAIN DASHBOARD BODY  ----------------------------------
    
    tabItem(tabName = 'dashboard',
            
            # 2.4.1 Title  ------------------------------------------------
            
            uiOutput('title') %>% 
              withSpinner(color="#3C8DBC",type=4, proxy.height = "115px",size = 0.5),  
            
            br(),
            
            
            # 2.4.2 Value boxes  ------------------------------------------
            
            div(align='center',h2(tags$u("Overview - Total sales"),align = 'align')),
            
            # Total turnover
            h3("Total turnover"),
            fluidRow(
              valueBoxOutput("valuebox_turnover_sales",width = 6) %>% 
                withSpinner(color="#3C8DBC",type=4, proxy.height = "128px"),
              valueBoxOutput("valuebox_turnover_returns",width = 6) %>% 
                withSpinner(color="#ECF0F5",type=0,proxy.height = "0px")
            ),
            
            # Total number of transaction
            h3("Total number of transactions"),
            fluidRow(
              valueBoxOutput("valuebox_transactions_sales",width = 6) %>% 
                withSpinner(color="#3C8DBC",type=4, proxy.height = "128px"),
              valueBoxOutput("valuebox_transactions_returns",width = 6) %>% 
                withSpinner(color="#ECF0F5",type=0, proxy.height = "0px")
            ),
            
            br(),
            
            # 2.4.3 Leaflet Map  ------------------------------------------
            
            ### Title
            div(
              align='center',
              h2(tags$u("Geographical distribution of sales in Belgium"),
                 align = 'align')
              ),
            
            br(),
            
            ### Explanations
            h3("Here you can visualize in a map the distribution of the sales performances
            of the different Decathlon stores according to the total turnover, the total 
            number of transactions or the total quantity of products sold."),
            
            h3(
              tags$u((tags$b("Note:"))),
              " Click on the circle to see the name of the store and its corresponding value"
              ),
            
            br(),
            
            ### Leaflet box
            box(
            
            title = HTML("<div class='w-h-t-circle'> </div> 
            <b class='boxtitle' > 
            Geographical distribution of sales indicators 
                         </b>"),
            # Filter buttons
            fluidRow(
              
              column(2),
              
              column(
                3,
                radioGroupButtons(
                  "transaction_type_leaflet",
                  label = h5("Transaction type"), status = "primary",
                  choices=c("Sales" = "sale" ,"Returns" = "return"),
                  justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
              
              column(
                6,
                radioGroupButtons(
                  "radius", label = h5("Color intensity variable"),
                  status = "primary",
                  choices=c("Total transactions" = "the_transaction_id",
                            "Total turnover" = "turnover",
                            "Total quantity" = "quantity"),
                  selected = "turnover",
                  justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
              
               column(1)
             ),
            # Leaflet output
            leafletOutput("leaflet_Map",width="95%") %>%
               withSpinner(color="#3C8DBC",type=4, size = 1.1),
             br(),
            status="primary", solidHeader = T, width = 12,align='center'),
            
            
            # Quick&Dirty debug (to be reviewed later)
            p("white space so that I can break the line (to be resolved)",style='color: #ECF0F5;'),
            
            
            # 2.4.4 Temporal evolution Heat Map ---------------------------
            
            # title
            div(
              align='center',
              h2(tags$u("Temporal evolution Heat map"),
                 align = 'align')
              ),
            
            ### explanations
            h3("In this Heat Map you can see a graphic representation of the temporal 
            evolution of the totals of the 3 indicators seen before (turnover, product
            quantity or number of transaction),that are aggregated and superimposed in 
               a chosen period of time (days of the week, months or quarters)."),
            h4(tags$u((tags$b("Purpose:"))),
               "This type of heatmap is relevant to detect the possible effects of periodic events 
               impacting the sales performance (week end effect, peak sales seasons...), with reference to an item
               or a category of items. Therefore, it is more preferable to folter by a 
               single item or category of items for this graph to derive relevant information."),
            h4(tags$u((tags$b("Note:"))),
               "Click on the exporting button at the top right of Highcharts to view in full screen,
               to export Data or download the plot."),
            
            
            br(),br(),

            ### Box
            box(
            title = HTML("<div class='w-h-t-circle'> </div>
            <b class='boxtitle' >
            Heat Map : Periodic visualization of temporal evolution of sales performance
                             </b>"),
            # Filters
            fluidRow(
              
              column(
                1,
                br(),br(),
                tipify(
                  actionBttn(
                    inputId = "null", style = "stretch", color = "primary",
                    icon = icon("info"),size = "sm",block="T"
                    ),
                  "it is more preferable for this heatmap to choose a single item or a signle category of items for a meaningful analysis",
                  "bottom")
               ),
              
               column(
                 2,
                 radioGroupButtons(
                   "transaction_type_2", label = h5("Transaction type"), status = "primary",
                   choices=c("Sales" = "sale" ,"Returns" = "return"),
                   justified =T,width = "100%", size = "xs", individual = T
                      )
               ),
              
              column(
                6,
                radioGroupButtons(
                  "intensity_2", label = h5("Color intensity variable"), status = "primary",
                  choices=c("Total transactions" = "the_transaction_id",
                            "Total turnover" = "turnover",
                            "Total quantity" = "quantity"),
                  selected = "turnover", justified =T,width = "100%", size = "xs",
                  individual = T
                  )
                ),
              
               column(
                 2,
                 tipify(
                   radioGroupButtons(
                     "display", label = h5("Display by"), status = "primary",
                     choices=c("Days"="day","Months"='month', "Quarters"='quarter'),
                     justified =T,width = "100%", size = "xs", individual = T
                      ),
                   "If aggregated by months or quarters : make sure you filter by a large period of time (more than one year) so that you can visualize the effect of different categories of month/quarter on the sales",
                   "bottom"
                   )
                 ),
              
               column(1),
              
             ),
            # Heat map output
            highchartOutput("heat_map_chronologic") %>%
              withSpinner(color="#3C8DBC",type=4, size = 1.1),
            width=12,status="primary", solidHeader = T),
            
            
            # 2.4.5 Temporal evolution lines ------------------------------
            
            # Quick&Dirty debug (to be reviewed later)
            p("white space so that I can break the line (to be resolved)",style='color: #ECF0F5;'),
            
            ### title
            div(
              align='center',
              h2(tags$u("Temporal evolution By items"),
                 align = 'align')
              ),
            
            ### Explanation
            h3("Since the last graph provides a periodic but mainly categorical visualization,
            this one, which is by the way complementary to the one before, can be useful to
            follow the evolution in time of the item sales in an individual way, 
               and even more to compare them."),
            
            br(),
            
            ### Box
            box(
            title = HTML("<div class='w-h-t-circle'> </div> 
                     <b class='boxtitle' >
                         Lines : Temporal evolution by item
                         </b>"),
            
            # filters
            fluidRow(
              
              column(1),
              
              column(
                2,
                radioGroupButtons(
                  "transaction_type_line_HC", label = h5("Transaction type"),
                  status = "primary", choices=c("Sales" = "sale" ,"Returns" = "return"),
                  justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
              
              column(
                5,
                radioGroupButtons(
                  "Y_line_HC", label = h5("Y variable"), status = "primary",
                  choices=c("Total transactions" = "the_transaction_id",
                            "Total turnover" = "turnover",
                            "Total quantity" = "quantity"),
                  selected = "turnover",
                  justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
              
              column(
                3,
                radioGroupButtons(
                  "aggregation_line_HC", label = h5("Aggregate by"), status = "primary",
                  choices=c("Days"="days",
                            "Weeks"='weeks',
                            "Months"='months',
                            "Years"="years"
                            ),
                  selected = 'weeks', justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
              
              column(1),
                ),
            
            # Lines output
            highchartOutput("Build_Date_line_charts") %>%
              withSpinner(color="#3C8DBC",type=4, size = 1.1),
            width=12,status="primary", solidHeader = T),
            
            
            
            # 2.4.6 Correlation store/item Heat Map -------------------------
            
            p("white space so that I can break the line (to be resolved)",style='color: #ECF0F5;'),
            
            ### title
            div(align='center',h2(tags$u("Heatmap: item/store"),align = 'align')),
            
            ### explanation
            h3("This Heat Map is well suited to visualize and compare massively the sales
            performance of different items in different stores. In addition, it is usually
               useful to detect anomalies."),

            br(),
            
            ### Box
            box(
            title = HTML("<div class='w-h-t-circle'> </div>
                         <b class='boxtitle' > Heatmap of items by stores
                         </b>"),
            # filters
            fluidRow(
              column(1),
              
              column(
                2,
                radioGroupButtons(
                  "transaction_type_1", label = h5("Transaction type"), status = "primary",
                  choices=c("Sales" = "sale" ,"Returns" = "return"), justified =T,
                  width = "100%", size = "xs", individual = T, selected = 'return'
                  )
                ),
              
              column(1),
              
              column(
                6,
                radioGroupButtons(
                  "intensity_1", label = h5("Intensity variable"), status = "primary",
                  choices=c("Number of transactions" = "the_transaction_id",
                            "Total turnover" = "turnover",
                            "Total quantity" = "quantity"),
                  justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
              column(2)
              ),
            # Heat Map output
            highchartOutput("heat_map_correlation") %>%
              withSpinner(color="#3C8DBC",type=4, size = 1.1),
            width=12,status="primary", solidHeader = T),
            
            
            # 2.4.7 Bar Plot Comparison stores ----------------------------
            
            p("white space so that I can break the line (to be resolved)",style='color: #ECF0F5;'),
            
            ### title
            div(align='center',h2(tags$u("Comparaison of sales in different stores"),align = 'align')),
            
            ### explanation
            h3("This graph is also very complementary to the one before. It allows you to compare the sales performance
               of the different stores by summing up the three indicators for all the items selected in the filter."),
            
            br(),
            
            ### Box
            box(
            title = HTML("<div class='w-h-t-circle'> </div>
            <b class='boxtitle' > Barplot: Comparaison of sales in
                         different stores </b>"),
            # filters
            fluidRow(
              column(1),
              
              column(
                2,
                h5("Horizontal bars",style="margin-bottom: 1.15em;"),
                materialSwitch(
                  inputId = "horizontal",
                  value = F,
                  status = "primary",
                  )
                ),
              
              column(
                2,
                h5("Show percentage",style="margin-bottom: 1.15em;"),
                materialSwitch(
                  inputId = "percent",
                  value = F,
                  status = "primary",
                  )
                ),
              
              column(
                6,
                radioGroupButtons(
                  "barplot_y_axis", label = h5("Display by"), status = "primary",
                  choices=c("Number of transactions" = "the_transaction_id",
                            "Total turnover" = "turnover",
                            "Total quantity" = "quantity"),
                  justified =T,width = "100%", size = "xs", individual = T
                  )
                ),
              
              column(1)
                ),
            # Bar Plot output
            highchartOutput("Bar_plot_comparaison") %>%
              withSpinner(color="#3C8DBC",type=4, size = 1.1),
            width=12,status="primary", solidHeader = T),
            
            
            p("white space so that I can break the line (to be resolved)",style='color: #ECF0F5;'),

            ###. Button to scroll to the top (toggle sidebar)
            div(
              a("Scroll to top ", type="button", class = "btn btn-primary down",
                href = "#.sidebar-toggle", icon("arrow-up")),
              align = "center"
              )
            ),
    
    # 2.5. Filtered Data  -------------------------------------------------
    
    tabItem(
      
      tabName = "Filtered_Data",
      
      ### Download button
      fluidRow(
        align = 'center',
        downloadButton('Download', 'Download Filtered Data', class = "btn-primary btn down")
        ),
      
      br(),
      
      ### Box containing the Filtered Data
      box(
      title = HTML("<div class='w-h-t-circle'> </div>
      <b class='boxtitle' > Barplot comparaison of sales in
                   different stores </b>"),
      br(),
      # Data table output
      DT::dataTableOutput(outputId = "DT") %>%
        withSpinner(color="#3C8DBC",type=4, size = 1.1),
      width=12,status="primary", solidHeader = T),
      
      br()
      )
    )
  )



# 2.6 put UI together ---------------------------------------------------

ui <- 
  dashboardPage(header, sidebar, body )


# 3 SERVER  -------------------------------------------------------------

server <- function(input, output, session) {
  
  # 3.1 Reactive filtered Data  -----------------------------------------
  DT <- eventReactive(input$submit, ignoreNULL = F,{
    subset(
      Data,
      item_name %in% input$item_names &
        store_name %in% input$store_names &
        prices >= input$prices_range[1] &
        prices <= input$prices_range[2] &
        the_date_transaction >= input$dates_range[1] &
        the_date_transaction  <=  input$dates_range[2]
    )
  })
  
  # 3.2 Filter interdependence ------------------------------------------
  
  ### {item}: depends on <= {store} & {price range}###
  ### {Date}: independent filter                   ###
  ####################################################
  
  # Update {item} based on {store} with respecting the chosen {price range}
  observeEvent(
    input$store_names,ignoreInit = T,
    {
      item_names_update <- 
        unique(Data$item_name[Data$store_name %in% input$store_names &
                                Data$prices >= input$prices_range[1] & # to respect the chosen {price range}
                                Data$prices <= input$prices_range[2] ] # to respect the chosen {price range}
               )
      updatePickerInput(
        session, "item_names", choices = item_names_update,selected = item_names_update[1]
        )
      }
    )
  
  # Update {item} based on {price range} with respecting the chosen {store}
  observeEvent(
    input$prices_range,
    ignoreInit = T,
    {
      item_names_update <- 
        unique(Data$item_name[Data$store_name %in% input$store_names & # to respect chosen {store}
                                Data$prices >= input$prices_range[1] &
                                Data$prices <= input$prices_range[2] ]
               )
      updatePickerInput(
        session, "item_names", choices = item_names_update,selected = item_names_update[1]
        )
      }
    )
  
  # 3.3 Title render UI--------------------------------------------------
  
  output$title <- 
    renderUI({
      N_stores <-  length(input$store_names)
      N_items <- length(input$item_names)
      Date_1 <- input$dates_range[1]
      Date_2 <- input$dates_range[2]
      tagList(
        tags$h1("Decathlon Sales Performance Report for ",tags$b(N_items),
                " items in ",
                tags$b(N_stores),
                " different store"),
        tags$h1("Between ", tags$b(Date_1)," and ", tags$b(Date_2),':')
        )
      })
  
  
  # 3.4 Total sales indicators value boxes --------------------------------
  
  ### Total turnover sales
  valuebox_turnover_sales <- eventReactive(input$submit, ignoreNULL = F,{
    Build_valuebox(DT())$valuebox_turnover_sales
  })
  output$valuebox_turnover_sales <- renderValueBox(
    valuebox_turnover_sales()
  )
  ### Total turnover returns
  valuebox_turnover_returns <- eventReactive(input$submit, ignoreNULL = F,{
    Build_valuebox(DT())$valuebox_turnover_returns
  })
  output$valuebox_turnover_returns <- renderValueBox(
    valuebox_turnover_returns()
  )
  ### Total transactions sales
  valuebox_transactions_sales <- eventReactive(input$submit, ignoreNULL = F,{
    Build_valuebox(DT())$valuebox_transactions_sales
  })
  output$valuebox_transactions_sales <- renderValueBox(
    valuebox_transactions_sales()
  )
  ### Total transactions returns
  valuebox_transactions_returns <- eventReactive(input$submit, ignoreNULL = F,{
    Build_valuebox(DT())$valuebox_transactions_returns
  })
  output$valuebox_transactions_returns <- renderValueBox(
    valuebox_transactions_returns()
  )
  
  # 3.5 Stores cartography leaflet map -------------------------------------
  
  leaflet_Map <- 
    eventReactive(
      c(input$submit,
        input$radius,
        input$transaction_type_leaflet),
      ignoreNULL = F,{
        Build_leaflet_map(DT= DT(),
                          radius = input$radius,
                          transaction_type = input$transaction_type_leaflet)
        }
      )
  
  output$leaflet_Map <- renderLeaflet(leaflet_Map())  

  # 3.6 Temporal evolution Heat Map  ---------------------------------------
  
  output$heat_map_chronologic <-
    renderHighchart({
      Build_HC_Temporal_heatmap(DT= DT(),
                               X = input$display, # default input = 'day'
                               intensity = input$intensity_2,  # default input='the_transaction_id' for transaction occurrence
                               transaction_type = input$transaction_type_2 # default input='sale' 
                               )
      })
  
  # 3.7 Temporal evolution lines  ------------------------------------------
  
  output$Build_Date_line_charts <- 
    renderHighchart({
      Build_Date_line_charts(DT= DT(),
                             X = "the_date_transaction",
                             Y = input$Y_line_HC,  # default input='the_transaction_id' for transaction occurrence
                             group = "item_name",
                             transaction_type = input$transaction_type_line_HC, # default input='sale'
                             aggregation_period = input$aggregation_line_HC
                             )
      })
  
  # 3.8 Items/stores correlation heat map  ---------------------------------
  
  output$heat_map_correlation <- 
    renderHighchart({
      Build_HC_Heatmap_Correlation(DT = DT(), X = 'store_name',Y = 'item_name',
                                  intensity = input$intensity_1, # default input='the_transaction_id' for transaction occurrence
                                  transaction_type = input$transaction_type_1 # default input='return'
                                  )
      })
  
  # 3.8 Stores comparison Bar Plot  ----------------------------------------
  
  output$Bar_plot_comparaison <- 
    renderHighchart({
      Build_HC_Barplot(DT = DT(),
                      X = "store_name",
                      Y = input$barplot_y_axis, # default input='the_transaction_id' for transaction occurrence
                      group = "tdt_type_detail",
                      percent = input$percent, # default input = FALSE
                      horizontal = input$horizontal # default input = FALSE 
                      )
      })
  
  # 3.9 Display + Download Data --------------------------------------------
  
  ### Download button
  output$Download <-
    downloadHandler(
      paste0("Filtred-Data-", Sys.Date(), ".csv", sep = ""),
      content = function(file) {
        fwrite(DT()[1:10], file, sep = ";", row.names = F)
        }
      )
  
  ### Raw Data
  output$DT <-
    DT::renderDataTable(
      datatable(DT()[1:10],
                filter = 'top',
                rownames = F,
                options = list(scrollY = '365px')
                )
    )
  }

# 4.0 RUN APP -------------------------------------------------------------

shinyApp(ui, server)
