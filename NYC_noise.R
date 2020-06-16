library(shiny)
library(tidyverse)
library(modelr)
library(infer)
library(lubridate)
library(readr)
library(DT)

nyc <- read_csv("./party_in_nyc.csv")

nyc <- nyc %>% 
    mutate(
        minutes.open = interval(`Created Date`, `Closed Date`) / dminutes(1)
    ) %>% 
    drop_na(minutes.open) %>% 
    filter(minutes.open > 0)

# Define UI for application
ui <- fluidPage(
    
    titlePanel("Analysis of New York Noise Complaints, 2016"),
    
    navbarPage("",
               
               tabPanel("About",
                        
                        sidebarLayout(
                            
                            headerPanel("The '2016 Parties in New York' Dataset"),
                            
                            mainPanel(
                                p("Please note that some of the plots may take up to 10 seconds to load."),
                                p("This dataset contains all noise-complaint cases filed by police in New York City throughout the 2016 year. 
                        The columns include date opened, data closed, location type (e.g. store, residential, etc.), zip code, Borough, Latitude and Longitude.
                        The mean time cases were left open was about 5 hours, residential buildings had more complaints than any other building and Brooklyn had the most total complaints with almost 69,000. 
                        For our analysis, we graphically represented this data in many ways, including a heatmap, boxplots, bar charts and line graphs.")
                            )
                        )
               ),
               
               tabPanel("Heatmap", 
                        
                        headerPanel("Heatmap of Complaints throughout the NYC Boroughs"),
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                selectInput("borough", "Borough:",
                                            c("Manhattan" = "MANHATTAN",
                                              "Brooklyn" = "BROOKLYN",
                                              "Bronx" = "BRONX",
                                              "Queens" = "QUEENS",
                                              "Staten Island" = "STATEN ISLAND"
                                            )
                                ), 
                                p("This is a heatmap of all noise complaints in the selected Borough.
                               Case Count is the total amount of cases opened during 2016."),
                                
                                p("To create this plot I filtered by cleaned Longitude/Latitude coordinates,
                              plotted these coordinates as a scatterplot (x = Lon and y = Lat), then layered 
                              with a translucent 2d density plot using a low bin count.")),
                            
                            mainPanel(
                                
                                plotOutput("plot1")), 
                            
                        )),
               
               tabPanel("Line Graphs",
                        
                        headerPanel("Line Graphs of Months vs. Complaints by Borough or Location"),
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                
                                style = "position:fixed;width:inherit;", 
                                
                                selectInput("month", "Month",
                                            c("January" = "1",
                                              "February" = "2",
                                              "March" = "3",
                                              "April" = "4",
                                              "May" = "5",
                                              "June" = "6",
                                              "July" = "7",
                                              "August" = "8",
                                              "September" = "9",
                                              "October" = "10",
                                              "November" = "11",
                                              "December" = "12"
                                            )),
                                p("These are graphs of complaints throughout each month. The first graph shows complaints for Boroughs while the second
                               shows complaints for location type."), 
                                p("The most total complaints in one day occured on June 19th, with over 2,500, 
                              the mean number of complaints is 2,111 and the median is 2,066.
                              Of all locations, residential buildings had significantly more complaints with over 146,000
                              followed by sidewalks with 42,353."),
                                p("Note the sharp increase at the end of each week and on New Years.  
                              New Yorkers like to party!")
                            ),
                            
                            mainPanel(
                                
                                plotOutput("plot2"),
                                plotOutput("plot3"))
                        )
               ),
               
               tabPanel("Boxplots",
                        
                        headerPanel("Analysis of Noise Complaint Case Length by Geography"),
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                style = "position:fixed;width:inherit;",
                                
                                radioButtons("bp_type", "Analyze noise complaint case length by:",
                                             c("Borough", "Location Type")),
                                
                                p("It appears that the Bronx had the highest mean noise complaint case length
                      of the boroughs (second to the Unpsecified category),
                      and  the location that had the highest mean noise complaint case length
                      is the street/sidewalk."),
                                p("Note that the y-axis of the plot is a log scale, making the 
                      means seem closer together than they are."),
                                p("To perform this analysis, we first used the date/time each 
                    case was opened and closed
                      to determine how long each noise complaint case was open on
                      police records."),
                                p("Some of the cases ( <10 ) were never closed (meaning they were open
                      indefinitely), so we dropped those from this analysis."),
                                p("The tables to the right show the mean length of time a noise
                      complaint case was open both by Borough and by Location Type.")
                            ),
                            
                            mainPanel(
                                plotOutput("boxPlot"),
                                h3("Means by Borough"),
                                DT::dataTableOutput("meansByBorough"), br(),
                                h3("Means by Location Type"),
                                DT::dataTableOutput("meansByLocType")
                            )
                        )
               ),
               
               tabPanel("Stacked Histogram",
                        
                        headerPanel("Relative Number of Complaints By Location Type within Each Borough"),
                        
                        sidebarLayout(
                            sidebarPanel(
                                style = "position:fixed;width:inherit;",
                                
                                p("The number of cases within each Location Type category
                      were added up by Borough for the whole year, and then the quantities were
                      standardized to be within 0 and 1 for easy comparison."),
                                p("The table shows the actual number of complaints for the 
                      Locations in the plot, again by each Borough."),
                                p("It appears that the largest number complaints occurred
                      in a Residential Building/House in Manhattan, clocking in
                      at 44660. Also, things seem to stay pretty tame around Houses
                      of Worship in Staten Island apparently! In fact, across all
                      the Locations, House of Worship seems to have the lowest 
                      level of noise complaints.")
                            ),
                            
                            mainPanel(
                                plotOutput("fillPlot"),
                                br(),
                                DT::dataTableOutput("fillDT")
                            )
                        )
               ),
               
               tabPanel("Bootstrap Plots",
                        
                        sidebarLayout(
                            
                            sidebarPanel(
                                style = "position:fixed;width:inherit;",
                                selectInput("bor", "Bootstrap case lengths by:",
                                            # "ALL" = "",
                                            c("BRONX",
                                              "BROOKLYN",
                                              "MANHATTAN",
                                              "QUEENS",
                                              "STATEN ISLAND",
                                              "Unspecified"
                                            )
                                ),
                                p("The topmost plot to the right shows the distribution of actual
                      noise complaint case lengths for the whole year. It may look 
                      close to normal, but notice that the x-axis is a log scale, 
                      indicating that the data is quite skewed right. That's why 
                      the mean (denoted by the vertical red line) of 5 hours doesn't 
                      seem to quite fall in the center of the data, but to the right."),
                                p(code("nyc %>% t_test(response = minutes.open)")),
                                p("Performing a t_test using the 'infer' package, as shown above, 
                      indicates that the population mean case length is between 
                      296.6278 and 302.4662 minutes, with 95% confidence."),
                                p("The middle plot shows a distribution of sample means after 
                      taking 1000 resamples using bootstrap techniques. Now the data 
                      is approx. normal, which agrees with the central limit theorem. 
                      Below the plot is a table that shows the lower 2.5% and upper 
                      97.5% quantiles, which creates an interval that catches 95% 
                      of our bootstrap mean estimates. This gives us our bootstrapped 
                      confidence interval, which seems to match well with the t-test!"),
                                p("The bottom-most plot uses the same bootstrap method as the middle plot, 
                      but using the select box you can perform a bootstrap analysis 
                      by individual Borough. Try it!")
                            ),
                            
                            mainPanel(
                                plotOutput("caseLengthDist"),
                                plotOutput("entireBootstrapPlot"),
                                DT::dataTableOutput("entireBsCiDT"),
                                plotOutput("bootstrapPlot"),
                                DT::dataTableOutput("selectedBsCiDT")
                            )
                        )
               ),
               
               tabPanel("Acknowledgements",
                        
                        sidebarLayout(
                            
                            headerPanel("Acknowledgements"),
                            
                            mainPanel(
                                p("This dataset was aquired from Kaggle."),
                                a(href="https://www.kaggle.com/somesnm/partynyc", "2016 Parties in New York"), hr(),
                                p("The data itself is courtesy of the NYC Open Data portal."),
                                a(href="https://opendata.cityofnewyork.us/", "NYC Open Data portal")
                            )
                        )
               )
    )
)

# Define server logic
server <- function(input, output) {
    
    options(DT.options = list(dom = 't'))
    
    output$plot1 <- renderPlot({
        nyc %>% 
            filter(Borough == input$borough) %>% 
            ggplot(aes(x = as.numeric(Longitude), y = as.numeric(Latitude))) +
            geom_point(size = 0.05, alpha = .4) +
            ggtitle(paste("Borough: ",
                          input$borough, 
                          "   Case Count: ",
                          sum(nyc$Borough == input$borough), sep = "")) +
            theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20), colour = "black"),
                  axis.title = element_text(family = "Helvetica", size = (0), colour = "black"),
                  axis.text = element_text(size = (0)),
                  axis.ticks = element_blank(),
                  panel.background = element_rect(fill="white"),
                  legend.position = "none") +
            stat_density2d(
                aes(x = Longitude, y = Latitude, fill = ..level.., alpha = 1),
                size = 0.01, bins = 10,
                geom = "polygon"
            )
    })
    
    output$plot2 <- renderPlot({
        nyc %>% 
            filter(!Borough == "Unspecified", month(`Created Date`) == input$month) %>% 
            mutate(Day = day(`Created Date`)) %>% 
            group_by(Day, Borough) %>% 
            summarise(total_count = n()) %>% 
            ggplot(aes(x=Day, y = total_count, color = Borough)) +
            geom_line(size = 1.1) +
            ggtitle("Complaints Throughout Month") +
            xlab("Days") +
            ylab("Complaints") +
            theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20), colour = "black"),
                  axis.title = element_text(family = "Helvetica", face = "bold", size = (15), colour = "black"),
                  axis.text = element_text(family = "Helvetica", size = (15), colour = "black"),
                  panel.background = element_rect(fill="white")
            )
    })
    
    output$plot3 <- renderPlot({
        nyc %>% 
            filter(!Borough == "Unspecified", month(`Created Date`) == input$month) %>% 
            mutate(Day2 = day(`Created Date`)) %>% 
            group_by(`Location Type`, Day2) %>% 
            summarise(total_count = n()) %>% 
            ggplot(aes(x=Day2, y = total_count, color = `Location Type`)) +
            geom_line(size = 1.1) +
            ggtitle("") +
            xlab("Days") +
            ylab("Complaints") +
            theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20), colour = "black"),
                  axis.title = element_text(family = "Helvetica", face = "bold", size = (15), colour = "black"),
                  axis.text = element_text(family = "Helvetica", size = (15), colour = "black"),
                  panel.background = element_rect(fill="white")
            )
    })
    
    # Boxplot by burough or location type, via radio button input
    selectedBoxplot <- reactive({
        case_when(
            input$bp_type == "Borough" ~ nyc$Borough,
            input$bp_type == "Location Type" ~ nyc$`Location Type`
        )
    })
    
    output$boxPlot <- renderPlot({
        nyc %>% 
            ggplot() +
            geom_boxplot(aes(x = selectedBoxplot(), y = minutes.open), 
                         color = "cornflowerblue", 
                         show.legend = FALSE) +
            scale_y_continuous(trans='log10') +
            xlab(paste(input$bp_type)) +
            ylab("Minutes Case is Open") +
            ggtitle(paste("NY Police Noise Complaint Case Lengths by ", input$bp_type, ", 2016", sep = "")) +
            theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20), colour = "black"),
                  axis.title = element_text(family = "Helvetica", face = "bold", size = (15), colour = "black"),
                  axis.text = element_text(family = "Helvetica", size = (15), colour = "black"))
    })
    
    output$meansByBorough <- DT::renderDataTable({
        nyc %>%
            select(Borough, minutes.open) %>%
            group_by(Borough) %>%
            summarize(mean = round(mean(minutes.open), 3))
    })
    
    output$meansByLocType <- DT::renderDataTable({
        nyc %>%
            select(`Location Type`, minutes.open) %>%
            group_by(`Location Type`) %>%
            summarize(mean = round(mean(minutes.open), 3))
    })
    
    # Histogram filled by location type, no interaction
    output$fillPlot <- renderPlot({
        nyc %>% 
            group_by(Borough, `Location Type`) %>% 
            summarize(n = n()) %>% 
            ungroup() %>% 
            ggplot(aes(Borough, n, fill = `Location Type`)) +
            geom_bar(stat = "identity", position = "fill") +
            scale_fill_discrete(name = "Location Type") +
            ylab("Relative Number of Cases") +
            ggtitle("Relative Amount of Complaints By Location Type within Boroughs, NY 2016") +
            theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20), colour = "black"),
                  axis.title = element_text(family = "Helvetica", face = "bold", size = (15), colour = "black"),
                  axis.text = element_text(family = "Helvetica", size = (15), colour = "black"),
                  panel.background = element_rect(fill="white"))
    })
    
    output$fillDT <- DT::renderDataTable({
        nyc %>% 
            group_by(Borough, `Location Type`) %>% 
            summarize(n = n()) %>% 
            ungroup() %>% 
            pivot_wider(names_from = `Location Type`, values_from = n, values_fill = list(n = 0)) %>% 
            datatable()
    })
    
    # Histogram density that shows distribution of case length, no intrxn
    output$caseLengthDist <- renderPlot({
        nyc %>% 
            ggplot(aes(x = minutes.open)) + 
            geom_histogram(aes(y = ..density..),
                           fill = "white",
                           color = "black") + 
            geom_density(fill = "cornflowerblue", alpha = 0.5) + 
            geom_vline(xintercept = mean(nyc$minutes.open), color = "red", lwd = 2) +
            scale_x_continuous(trans='log10') +
            xlab("Minutes Noise Complaint Case was Open") +
            ggtitle("NY Police Noise Complaint Case Lengths, 2016") +
            theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20), colour = "black"),
                  axis.title = element_text(family = "Helvetica", face = "bold", size = (15), colour = "black"),
                  axis.text = element_text(family = "Helvetica", size = (15), colour = "black"),
                  panel.background = element_rect(fill="white"))
    })
    
    # Bootstrap for all of NY. not interactive
    bs1 <- nyc %>% 
        select(minutes.open) %>% 
        bootstrap(1000) %>% 
        mutate(
            means = map_dbl(strap, ~mean(data.frame(.x)$minutes.open))
        )
    
    output$entireBootstrapPlot <- renderPlot({
        ggplot(bs1, aes(x = means)) + 
            geom_histogram(aes(y = ..density..),
                           fill = "white",
                           color = "black") + 
            geom_density(fill = "cornflowerblue", alpha = 0.5) + 
            geom_vline(xintercept = mean(nyc$minutes.open), color = "red", lwd = 2) +
            xlab("Mean Case Lengths of Re-samples, in Minutes") +
            ggtitle("Bootstrap Distribution of Sample Mean Case Lengths") +
            theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20), colour = "black"),
                  axis.title = element_text(family = "Helvetica", face = "bold", size = (15), colour = "black"),
                  axis.text = element_text(family = "Helvetica", size = (15), colour = "black"),
                  panel.background = element_rect(fill="white"))
    })
    
    output$entireBsCiDT <- renderDataTable({
        data.frame( confidence_interval = bs %>% 
                        pull(means) %>% 
                        quantile(c(0.025, 0.975)) %>% 
                        round(3) )
    })
    
    # Bootstrap by borough Select box input specifies borough
    selectedData <- reactive({
        nyc %>% 
            filter(Borough == input$bor)
    })
    
    bs2 <- reactive({ selectedData() %>% 
            select(minutes.open) %>% 
            bootstrap(1000) %>% 
            mutate(
                means = map_dbl(strap, ~mean(data.frame(.x)$minutes.open))
            )})
    
    output$bootstrapPlot <- renderPlot({
        ggplot(bs2(), aes(x = means)) + 
            geom_histogram(aes(y = ..density..),
                           fill = "white",
                           color = "black") + 
            geom_density(fill = "cornflowerblue", alpha = 0.5) + 
            geom_vline(xintercept = mean(bs2()$means), color = "red", lwd = 2) +
            xlab("Mean Case Lengths of Re-samples, in Minutes") +
            ggtitle(paste("Bootstrap Distribution of Sample Mean Case Lengths,", input$bor)) +
            theme(plot.title = element_text(family = "Helvetica", face = "bold", size = (20), colour = "black"),
                  axis.title = element_text(family = "Helvetica", face = "bold", size = (15), colour = "black"),
                  axis.text = element_text(family = "Helvetica", size = (15), colour = "black"),
                  panel.background = element_rect(fill="white"))
    })
    
    output$selectedBsCiDT <- DT::renderDataTable({
        data.frame( confidence_interval = bs2() %>% 
                        pull(means) %>% 
                        quantile(c(0.025, 0.975)) %>% 
                        round(3))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
