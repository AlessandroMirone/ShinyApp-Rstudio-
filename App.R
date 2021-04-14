#load libraries and dataset
library(shiny)
library(ggplot2)
library(readr)
regData<-read_tsv('data/regData.tsv', col_names = T)

# Define UI ----
ui <- fluidPage(
  titlePanel(strong("Italy's levels of Internet Access")), 
  fluidRow(
    column(4, #side panel
           wellPanel(
             h3('Visualizing Italian digital development'),
             br(),
             p('This interactive application uses data from',
               a('European Eurostat Database', 
                 href='https://ec.europa.eu/eurostat/web/main/data/database'),
               'to analyze the Italian level of digital development.'),
             p("The plot's facets in the",
               span("Data Visualization Interface", style='color:blue'),
               'represent the different regions of Italy, plus the national
               and European mean.'),
             p('Users can modify the visualization through the',
               span("Options", style='color:blue'),
               'panel, below the interface.'),
             p(span("Summary", style='color:blue'),"and",
               span("Table",style="color:blue"),"tabs provide
               additional information about the dataset."),
             br(),
             img(src = "UNIMI-LOGO-ATENEO.png", height = 140, width = 200),
             p(em("This project was created in 2021 for the",
                  a("Data Science and Economics'",
                    href="https://www.unimi.it/it/corsi/corsi-di-laurea/data-science-and-economics-dse"),
                  "course 'Coding with R' at the University of Milan"))
           )
    ),
    column(8, #main panel
           (tabsetPanel(tabPanel("Plot",
                                 br(),
                                 br(),
                                 h4('Data Visualization Interface', align = 'center'),
                                 plotOutput("plot")),
                        tabPanel("Summary",
                                 br(),
                                 br(),
                                 h4("Coefficient's summary", align= "center"),
                                 verbatimTextOutput("summary")),
                        tabPanel("Table",
                                 br(),
                                 br(),
                                 h4("Data viewer", align="center"),
                                 tableOutput("table"))
           )
           )
    )
  ),
  fluidRow(
    column(8, offset = 4, 
           wellPanel(h4('Options', align= 'center'), #options panel
                     fluidRow(column(6,
                                     (sliderInput("slider", "Time Period",
                                                  min = range(regData$Year)[1],
                                                  max = range(regData$Year)[2],
                                                  value = c(2009,2019), ticks = F, sep = ""))),
                              column(4,offset = 2,
                                     (selectInput("plot_type",
                                                  "Choose type of plot",
                                                  choices = list("line graph " = "line",
                                                                 "linear regression" = "lm"),
                                                  selected = "line")))
                     ),
                     fluidRow(column(4, offset = 8,
                                     numericInput(inputId = "obs",
                                                  label = "Number of observations to view in Table:",
                                                  value = 10, min = 0, max = 247)))
           )
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$plot<-renderPlot({
    
    new_data <- subset(regData, subset=c(Year<=input$slider[2]&Year>=input$slider[1]))
    
    plot_base <- ggplot(new_data, aes(y = Coefficient, x = Year, group = Region)) +
      geom_point() +
      facet_wrap(~Region, ncol = 5) +
      labs(y="Level of Households with Broadband Connection - Italy", x = "Year") +
      scale_y_continuous(
        breaks = seq(0,100, by=20 ),limits = c(20,110), expand=c(0,0)) +
      scale_x_continuous(
        name= "Year", input$slider,breaks = seq(input$slider[1],input$slider[2], by=1)) +
      theme_bw() +
      theme(axis.text.x = element_text(size=9,angle=45))
    
    if(input$plot_type == "line") {
      plot_base +
        geom_line()
    } else {
      plot_base +
        geom_smooth(method = "lm", se= F, col='dark green')
    }
    
  })
  
  output$summary <-renderPrint({summary(regData$Coefficient)
  })
  
  output$table <- renderTable({
    head(regData, n = input$obs)
  }, width=700, striped = T, align = 'c', digits = 0)
  
}

# Run the app 
shinyApp(ui = ui, server = server)
