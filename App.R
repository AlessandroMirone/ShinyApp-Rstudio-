#load libraries and dataset
library(shiny)
library(ggplot2)
library(readr)
regData<-read_tsv('data/regData.tsv', col_names = T)
regiData<-read_tsv('data/regiData.tsv', col_names = T)
regaData<-read_tsv('data/regaData.tsv', col_names = T)

#subsetting datasets to remove aggregates' average in the summary
regDatas<-regData[-c(226:247), ]
regiDatas<-regiData[-c(188:205), ]
regaDatas<-regaData[-c(1:9,10:18,55:63,118:126,145:153,198:206,242:250), ]


# Define UI ----
ui <- fluidPage(
  titlePanel(strong("Italy's levels of Internet Access")), 
  fluidRow(
    column(4, #side panel
           wellPanel(
             h3('Visualizing Italian digital development'),
             br(),
             p('This interactive application uses datasets from',
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
               additional information about the datasets."),
             br(),
             img(src = "UNIMI-LOGO-ATENEO.png", height = 140, width = 200),
             p(em("This project was created in 2021 for the",
                  a("Data Science and Economics'",
                    href="https://www.unimi.it/it/corsi/corsi-di-laurea/data-science-and-economics-dse"),
                  "course 'Coding within the R framework' at the University of Milan."))
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
                                     (uiOutput("slidercontrol"))),
                              column(4,offset = 2,
                                     (selectInput("plot_type",
                                                  "Choose type of plot",
                                                  choices = list("line graph " = "line",
                                                                 "linear regression" = "lm"),
                                                  selected = "line")))
                     ),
                     fluidRow(column(4,
                                     numericInput(inputId = "obs",
                                                  label = "Number of observations to view in Table:",
                                                  value = 10, min = 0, max = 259)),
                              column(4, offset = 4,
                                     selectInput("dataset",
                                                 "Choose dataset",
                                                 choices = c("Broadband access",
                                                                "Internet access",
                                                                "Interaction with Public Authorities through internet"),
                                                 selected = "Broadband access")))
           )
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  output$plot<-renderPlot({
    
    new_data <- subset(regData, subset=c(Year<=input$slider[2]&Year>=input$slider[1]))
    new_data2 <- subset(regiData, subset=c(Year<=input$slider[2]&Year>=input$slider[1]))
    new_data3 <- subset(regaData, subset=c(Year<=input$slider[2]&Year>=input$slider[1]))
    
    if(input$dataset == "Broadband access") {
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
    }}
   else if (input$dataset == "Internet access"){
      plot_base2 <- ggplot(new_data2, aes(y = Coefficient, x = Year, group = Region)) +
        geom_point() +
        facet_wrap(~Region, ncol = 5) +
        labs(y="Level of Households with Internet Connection - Italy", x = "Year") +
        scale_y_continuous(
          breaks = seq(0,100, by=20 ),limits = c(20,110), expand=c(0,0)) +
        scale_x_continuous(
          name= "Year", input$slider,breaks = seq(input$slider[1],input$slider[2], by=1)) +
        theme_bw() +
        theme(axis.text.x = element_text(size=9,angle=45))
      
      if(input$plot_type == "line") {
        plot_base2 +
          geom_line()
      } else {
        plot_base2 +
          geom_smooth(method = "lm", se= F, col='dark green')
      }
   } else {plot_base3 <- ggplot(new_data3, aes(y = Coefficient, x = Year, group = Region)) +
     geom_point() +
     facet_wrap(~Region, ncol = 5) +
     labs(y="Web-based interaction with Public Authorities - Italy", x = "Year") +
     scale_y_continuous(
       breaks = seq(0,100, by=20 ),limits = c(10,110), expand=c(0,0)) +
     scale_x_continuous(
       name= "Year", input$slider,breaks = seq(input$slider[1],input$slider[2], by=1)) +
     theme_bw() +
     theme(axis.text.x = element_text(size=9,angle=45))
   
   if(input$plot_type == "line") {
     plot_base3 +
       geom_line()
   } else {
     plot_base3 +
       geom_smooth(method = "lm", se= F, col='dark green')
   } }
        
  })
  
  output$summary <-renderPrint({summary(datasetCoefficientInput())
  })
  
  output$table <- renderTable({
    head(datasetInput(), n = input$obs)
  }, width=700, striped = T, align = 'c', digits = 0)
  
  output$slidercontrol <- renderUI({ if(input$dataset == "Broadband access") {
    (sliderInput("slider", "Time Period",
                 min = range(regData$Year)[1],
                 max = range(regData$Year)[2],
                 value = c(2009,2019), ticks = F, sep = ""))
    
  } else if(input$dataset == "Internet access"){
    (sliderInput("slider", "Time Period",
                 min = range(regiData$Year)[1],
                 max = range(regiData$Year)[2],
                 value = c(2011,2019), ticks = F, sep = ""))
  } else {(sliderInput("slider", "Time Period",
                       min = range(regaData$Year)[1],
                       max = range(regaData$Year)[2],
                       value = c(2011,2019), ticks = F, sep = ""))
    }
    
  })
  datasetInput <- reactive({
    switch(input$dataset,
           "Broadband access" = regData,
           "Internet access" = regiData,
           "Interaction with Public Authorities through internet" = regaData)
  })
  datasetCoefficientInput <- reactive({
    switch(input$dataset, #with aggregates' averages removed
           "Broadband access" = regDatas$Coefficient, 
           "Internet access" = regiDatas$Coefficient,
           "Interaction with Public Authorities through internet" = regaDatas$Coefficient)
  })
  
}

# Run the app 
shinyApp(ui = ui, server = server)
