library(shiny)
library(tidyverse)
library(ggplot2)
library(modelr)

h15 <- read_csv("2015.csv")
h16 <- read_csv("2016.csv")
h17 <- read_csv("2017.csv")
h18 <- read_csv("2018.csv")
h19 <- read_csv("2019.csv")
h20 <- read_csv("2020.csv")
predictors = c("Happiness Score",
              "Social Support Score",
              "GDP Per Capita Score",
              "Healthy Life Expectancy Score",
              "Generosity Score",
              "Freedom in Life Choices Score",
              "(Low) Institutional Corruption Score",
              "Happiness Rank")

h20 <- h20 %>%
  transmute(
    "Score" = `Ladder score`,
    "Generosity" = `Generosity`,
    "Social" = `Social support`,
    "Life" = `Healthy life expectancy`,
    "GDP" = `Logged GDP per capita`,
    "Corruption" = 1-`Perceptions of corruption`,
    "Rank" = `Rank`,
    "Freedom" = `Freedom to make life choices`,
    "Country" = `Country name`,
    "Region" = `Regional indicator`
  )

h20 <- h20 %>%
  mutate(
    "Score2" = round(percent_rank(Score)*10),
    "Generosity2" = round(percent_rank(Generosity)*10),
    "Social2" = round(percent_rank(Social)*10),
    "Life2" = round(percent_rank(Life)*10),
    "GDP2" = round(percent_rank(GDP)*10),
    "Corruption2" = round(percent_rank(Corruption)*10),
    "Freedom2" = round(percent_rank(Freedom)*10),
  )


h19 <- h19 %>%
  transmute(
    "Score" = `Score`,
    "Generosity" = `Generosity`,
    "Social" = `Social support`,
    "Life" = `Healthy life expectancy`,
    "GDP" = `GDP per capita`,
    "Corruption" = `Perceptions of corruption`,
    "Rank" = `Overall rank`,
    "Freedom" = `Freedom to make life choices`,
    "Country" = `Country or region`,
  )

h19 <- h19 %>%
  mutate(
    "Score2" = round(percent_rank(Score)*10),
    "Generosity2" = round(percent_rank(Generosity)*10),
    "Social2" = round(percent_rank(Social)*10),
    "Life2" = round(percent_rank(Life)*10),
    "GDP2" = round(percent_rank(GDP)*10),
    "Corruption2" = round(percent_rank(Corruption)*10),
    "Freedom2" = round(percent_rank(Freedom)*10),
  )

h18 <- h18 %>%
  transmute(
    "Score" = `Score`,
    "Generosity" = `Generosity`,
    "Social" = `Social support`,
    "Life" = `Healthy life expectancy`,
    "GDP" = `GDP per capita`,
    "Corruption" = `Perceptions of corruption`,
    "Rank" = `Overall rank`,
    "Freedom" = `Freedom to make life choices`,
    "Country" = `Country or region`,
  )

h18 <- h18 %>%
  mutate(
    "Score2" = round(percent_rank(Score)*10),
    "Generosity2" = round(percent_rank(Generosity)*10),
    "Social2" = round(percent_rank(Social)*10),
    "Life2" = round(percent_rank(Life)*10),
    "GDP2" = round(percent_rank(GDP)*10),
    "Corruption2" = round(percent_rank(Corruption)*10),
    "Freedom2" = round(percent_rank(Freedom)*10),
  )

h17 <- h17 %>%
  transmute(
    "Score" = `Happiness.Score`,
    "Generosity" = `Generosity`,
    "Social" = `Family`,
    "Life" = `Health..Life.Expectancy.`,
    "GDP" = `Economy..GDP.per.Capita.`,
    "Corruption" = `Trust..Government.Corruption.`,
    "Rank" = `Happiness.Rank`,
    "Freedom" = `Freedom`,
    "Country" = `Country`,
  )

h17 <- h17 %>%
  mutate(
    "Score2" = round(percent_rank(Score)*10),
    "Generosity2" = round(percent_rank(Generosity)*10),
    "Social2" = round(percent_rank(Social)*10),
    "Life2" = round(percent_rank(Life)*10),
    "GDP2" = round(percent_rank(GDP)*10),
    "Corruption2" = round(percent_rank(Corruption)*10),
    "Freedom2" = round(percent_rank(Freedom)*10),
  )

h16 <- h16 %>%
  transmute(
    "Score" = `Happiness Score`,
    "Generosity" = `Generosity`,
    "Social" = `Family`,
    "Life" = `Health (Life Expectancy)`,
    "GDP" = `Economy (GDP per Capita)`,
    "Corruption" = `Trust (Government Corruption)`,
    "Rank" = `Happiness Rank`,
    "Freedom" = `Freedom`,
    "Country" = `Country`,
  )

h16 <- h16 %>%
  mutate(
    "Score2" = round(percent_rank(Score)*10),
    "Generosity2" = round(percent_rank(Generosity)*10),
    "Social2" = round(percent_rank(Social)*10),
    "Life2" = round(percent_rank(Life)*10),
    "GDP2" = round(percent_rank(GDP)*10),
    "Corruption2" = round(percent_rank(Corruption)*10),
    "Freedom2" = round(percent_rank(Freedom)*10),
  )

h15 <- h15 %>%
  transmute(
    "Score" = `Happiness Score`,
    "Generosity" = `Generosity`,
    "Social" = `Family`,
    "Life" = `Health (Life Expectancy)`,
    "GDP" = `Economy (GDP per Capita)`,
    "Corruption" = `Trust (Government Corruption)`,
    "Rank" = `Happiness Rank`,
    "Freedom" = `Freedom`,
    "Country" = `Country`,
  )

h15 <- h15 %>%
  mutate(
    "Score2" = round(percent_rank(Score)*10),
    "Generosity2" = round(percent_rank(Generosity)*10),
    "Social2" = round(percent_rank(Social)*10),
    "Life2" = round(percent_rank(Life)*10),
    "GDP2" = round(percent_rank(GDP)*10),
    "Corruption2" = round(percent_rank(Corruption)*10),
    "Freedom2" = round(percent_rank(Freedom)*10),
  )


# subset data points into train and test sets
set.seed(123)
sample <- sample(c(TRUE, FALSE), nrow(h20), replace = T, prob = c(0.6,0.4))

# define train and test
train <- h20[sample, ]
test <- h20[!sample, ]

################################################################################
# Shiny
################################################################################

# Define UI ----
ui <- fluidPage(
  
  # Item: Title
  titlePanel("Analyzing the World Happiness Report"),
  
  # Item: 2020 ranking
  wellPanel(
    
    titlePanel("View the 2020 Ranking"),
    fluidRow(
      
      column(3, numericInput("rank_display_count", label = "count", value = 5, min = 1, max = 153)),
      column(3, radioButtons("happy_sad_button", "order by", choices = c("Happiest Countries", "Least Happy Countries"))),
      column(6, tableOutput("ranking_list"))
    )
    
  ),

  # Item: Stats, Histograms
  wellPanel(
    titlePanel("Explore the data set"),
    fluidRow(selectInput("var", 
                         label = "Choose a variable to display",
                         choices = predictors[predictors != "Happiness Rank"],
                         selected = "Happiness Score")),
    fluidRow(textOutput("definition")),
    headerPanel(""),
    headerPanel(""),
    headerPanel(""),
    headerPanel(""),
    headerPanel(""),
    fluidRow(
      column(2, helpText("Highest Scoring Countries"),tableOutput("high_scores")),
      column(2, helpText("Lowest Scoring Countries"), tableOutput("low_scores")),
      column(8,plotOutput("hist2"))
    ),
    headerPanel(""),
    headerPanel(""),
    fluidRow(
      column(6),
      column(4,verbatimTextOutput("stats")),
      column(2)
      )
    
  ),
  
  # Item: Preference Sliders
  wellPanel(
    titlePanel("Adjust happiness score according to your preferences"),
    fluidRow(
      column(9,
             helpText("How important do you think these factors are in determining what makes a country 'happy'? (0: lowest importance, 10: highest importance)"),
             sliderInput("social",
                         label = "Strength of Social Support",
                         min = 0, max  = 10, value = 7),
             sliderInput("gdp",
                         label = "High level of gdp per capita",
                         min = 0, max  = 10, value = 5),
             sliderInput("life",
                         label = "Healthy lifestyle and high life expectancy",
                         min = 0, max  = 10, value = 9),
             sliderInput("generosity",
                         label = "Cultural importance of generosity",
                         min = 0, max  = 10, value = 4),
             sliderInput("freedom",
                         label = "Freedom to make life choices",
                         min = 0, max  = 10, value = 6),
             sliderInput("corruption",
                         label = "Low level of institutional corruption",
                         min = 0, max  = 10, value = 8)),
      column(3,helpText("The 20 countries best matching your selections are: "),
             tableOutput("preference_top"))
    )
    
  ),
  
  # Item: Single Country Report
  wellPanel(
    titlePanel("Analyze a specific country"),
    
    fluidRow(
      
      column(6,
             selectInput("country", 
                            label = "Choose a country",
                            choices = sort(h20$Country),
                            selected = "Switzerland")
            ),
      column(6, selectInput("country_var", 
                                      label = "Display a variable score over time",
                                      choices = predictors,
                                      selected = "Freedom in Life Choices Score"))
      
    ),
    
    helpText("Full Report for 2020", style = "font-size:20px"),
    tableOutput("full_report_2020"),
    plotOutput("stat_time_series")
      

  ),
  
  # Item: Linear Regression
  wellPanel(
    titlePanel("Regression Analysis"),
    selectInput("regression_var",
                       label = "Select Variable to Regress Happiness Score Upon",
                       choices = predictors[2:7],
                       selected = "Social Support Score"
                       ),
    textOutput("model_summary"),
    plotOutput("regression_plot"),
    headerPanel(""),
    headerPanel(""),
    headerPanel(""),
    headerPanel(""),
    headerPanel(""),
    helpText("Multiple regression: You may notice that some of the factors in this data set appear to have a stronger relationship with the happiness score than others. In particular GDP, Life Expectancy, and Social Support.", style = "font-size:20px"),
    headerPanel(""),
    textOutput("mlr_results1"),
    textOutput("mlr_results2")
  )
  
  
)

server <- function(input, output) {

################################################################################
  # Item: View the 2020 Ranking
  ranking_count <- reactive({input$rank_display_count})
  ranking_order <- reactive({input$happy_sad_button})
  
  output$ranking_list <- renderTable(
    if (ranking_order() == "Happiest Countries"){
      
      h20[1:ranking_count(),] %>%
        select(Rank, Country) %>%
        as.data.frame() %>%
        transmute("Happiness Rank" = as.integer(Rank), "Country" = Country)
      
      
    }
    else {
      h20[(153-ranking_count() + 1):153,] %>%
        select(Rank, Country) %>%
        as.data.frame() %>%
        transmute("Happiness Rank" = as.integer(Rank), "Country" = Country) %>%
        arrange(desc(`Happiness Rank`))
    }
  )
  
  # Item: Stats, Histogram
  var_choice <- reactive({switch(input$var, 
                                 "Happiness Score" = h20$Score,
                                 "Generosity Score" = h20$Generosity,
                                 "GDP Per Capita Score" = h20$GDP,
                                 "Happiness Rank" = h20$Rank,
                                 "Social Support Score" = h20$Social,
                                 "Healthy Life Expectancy Score" = h20$Life,
                                 "Freedom in Life Choices Score" = h20$Freedom,
                                 "(Low) Institutional Corruption Score" = h20$Corruption
  )
  })
  
  output$definition <- reactive({
    switch(
      input$var,
      "Happiness Score" = "A composite score made by the publishers of the report based on the other variables",
      "Generosity Score" = "Generosity is defined as the residual of regressing the national average of responses to the question, 'Have you donated money to a charity in past months?' on GDP capita.",
      "GDP Per Capita Score" = "GDP per capita is a measure of a country's economic output that accounts for its number of people.",
      "Social Support Score" = "Social support means having friends and other people, including family, turning to in times of need or crisis to give you a broader focus and positive self-image.",
      "Healthy Life Expectancy Score" = "Healthy Life Expectancy is the average number of years that a newborn can expect to live in 'full health' - in other words, not hampered by disabling illnesses or injuries.",
      "Freedom in Life Choices Score" = "Freedom of choice describes an individual's opportunity and autonomy to perform an action selected from at least two available options, unconstrained by external parties.",
      "(Low) Institutional Corruption Score" = "The Institutional Corrupton score is based on the Corruption Perceptions Index (CPI) is an index published annually by Transparency International since 1995, which ranks countries 'by their perceived levels of public sector corruption, as determined by expert assessments and opinion surveys.' In this shiny app a high score corresponds to low corruption"
    )
    })
  
  sorted_list <- reactive({
    h20 %>%
      arrange(desc(var_choice())) %>%
      select(Country)
  })
  
    
  output$high_scores <- renderTable({
        sorted_list()$Country[1:10] %>%
          as.data.frame()
  })
  
  output$low_scores <- renderTable({
        sorted_list()$Country[144:153] %>%
          rev() %>%
          as.data.frame()
  })
    output$hist2 <- renderPlot({
             ggplot(h20, aes(var_choice()))+ geom_histogram(bins = 15, color = "darkblue", fill = "lightblue") + labs(title = "Distribution of Variable", x = input$var, y = "count") + scale_x_discrete()

    })
  
  output$stats <- renderPrint({
    summary(var_choice())
  })
  
  ##############################################################################
  #Item: Preference Sliders
  top_preferences <- reactive({
    h20 %>%
      transmute(
      "Country" = Country,
      "composite" = input$social * h20$Social
      + input$freedom * h20$Freedom
      + input$life * h20$Life
      + input$corruption * h20$Corruption
      + input$generosity * h20$Generosity
      + input$gdp * h20$GDP
      ) %>%
      arrange(desc(composite))
  })
  
  output$preference_top <- renderTable({
    top_preferences()[1:20,1:1]
  })
  
  ##############################################################################
  # Item: Single Country Report
  choice20 <- reactive({
    h20 %>%
      filter(Country == input$country)
  })
  
  pltval20 <- reactive({switch(input$country_var, 
                               "Happiness Score" = choice20()$Score2,
                               "Generosity Score" = choice20()$Generosity2,
                               "GDP Per Capita Score" = choice20()$GDP2,
                               "Happiness Rank" = choice20()$Rank,
                               "Social Support Score" = choice20()$Social2,
                               "Healthy Life Expectancy Score" = choice20()$Life2,
                               "Freedom in Life Choices Score" = choice20()$Freedom2,
                               "(Low) Institutional Corruption Score" = choice20()$Corruption2)
  })
  
  choice19 <- reactive({
    h19 %>%
    filter(Country == input$country)
  })
  
  pltval19 <- reactive({switch(input$country_var, 
                                         "Happiness Score" = choice19()$Score2,
                                         "Generosity Score" = choice19()$Generosity2,
                                         "GDP Per Capita Score" = choice19()$GDP2,
                                         "Happiness Rank" = choice19()$Rank,
                                         "Social Support Score" = choice19()$Social2,
                                         "Healthy Life Expectancy Score" = choice19()$Life2,
                                         "Freedom in Life Choices Score" = choice19()$Freedom2,
                                         "(Low) Institutional Corruption Score" = choice19()$Corruption2)
  })
  
  choice18 <- reactive({
    h18 %>%
      filter(Country == input$country)
  })
  
  pltval18 <- reactive({switch(input$country_var, 
                               "Happiness Score" = choice18()$Score2,
                               "Generosity Score" = choice18()$Generosity2,
                               "GDP Per Capita Score" = choice18()$GDP2,
                               "Happiness Rank" = choice18()$Rank,
                               "Social Support Score" = choice18()$Social2,
                               "Healthy Life Expectancy Score" = choice18()$Life2,
                               "Freedom in Life Choices Score" = choice18()$Freedom2,
                               "(Low) Institutional Corruption Score" = choice18()$Corruption2)
  })
  
  choice17 <- reactive({
    h17 %>%
      filter(Country == input$country)
  })
  
  pltval17 <- reactive({switch(input$country_var, 
                               "Happiness Score" = choice17()$Score2,
                               "Generosity Score" = choice17()$Generosity2,
                               "GDP Per Capita Score" = choice17()$GDP2,
                               "Happiness Rank" = choice17()$Rank,
                               "Social Support Score" = choice17()$Social2,
                               "Healthy Life Expectancy Score" = choice17()$Life2,
                               "Freedom in Life Choices Score" = choice17()$Freedom2,
                               "(Low) Institutional Corruption Score" = choice17()$Corruption2)
  })
  
  choice16 <- reactive({
    h16 %>%
      filter(Country == input$country)
  })
  
  pltval16 <- reactive({switch(input$country_var, 
                               "Happiness Score" = choice16()$Score2,
                               "Generosity Score" = choice16()$Generosity2,
                               "GDP Per Capita Score" = choice16()$GDP2,
                               "Happiness Rank" = choice16()$Rank,
                               "Social Support Score" = choice16()$Social2,
                               "Healthy Life Expectancy Score" = choice16()$Life2,
                               "Freedom in Life Choices Score" = choice16()$Freedom2,
                               "(Low) Institutional Corruption Score" = choice16()$Corruption2)
  })
  
  choice15 <- reactive({
    h15 %>%
      filter(Country == input$country)
  })
  
  pltval15 <- reactive({switch(input$country_var, 
                               "Happiness Score" = choice15()$Score2,
                               "Generosity Score" = choice15()$Generosity2,
                               "GDP Per Capita Score" = choice15()$GDP2,
                               "Happiness Rank" = choice15()$Rank,
                               "Social Support Score" = choice15()$Social2,
                               "Healthy Life Expectancy Score" = choice15()$Life2,
                               "Freedom in Life Choices Score" = choice15()$Freedom2,
                               "(Low) Institutional Corruption Score" = choice15()$Corruption2)
  })
  
  output$full_report_2020 <- renderTable(
    choice20() %>%
      transmute(
        "Happiness Rank" = as.integer(Rank),
        "Happiness Score" = Score2,
        "Freedom in Life Choices Score" = Freedom2,
        "Generosity Score" = Generosity2,
        "GDP Score" = GDP2,
        "Social Support Score" = Social2,
        "Life Expectancy Score" = Life2,
        "Low Corruption Score" = Corruption2
        
      )
  )
  
  output$stat_time_series <- renderPlot(
    
    if (input$country_var == "Happiness Rank"){
      
      cbind(
        c(2015,2016,2017,2018,2019,2020),
        c(pltval15(), pltval16(), pltval17(), pltval18(), pltval19(), pltval20())
      )%>%
        as.data.frame() %>%
        ggplot(aes(x = V1, y = V2)) + geom_point(size = 5, color = "green") + labs(title = "Variable Score by Year", x = "Report", y = "Score")
      
    }
    
    else{
      
      cbind(
        c(2015,2016,2017,2018,2019,2020),
        c(pltval15(), pltval16(), pltval17(), pltval18(), pltval19(), pltval20())
      )%>%
        as.data.frame() %>%
        ggplot(aes(x = V1, y = V2)) + geom_point(size = 5, color = "green") + ylim(0,10) + labs(title = "Variable Score by Year", x = "Report", y = "Score")
      
    }
    
  )
  

################################################################################
  # Item: Linear Regression
  
  regression_var_chosen <- reactive({
    switch(input$regression_var,
           "Generosity Score" = h20$Generosity,
           "GDP Per Capita Score" = h20$GDP,
           "Social Support Score" = h20$Social,
           "Healthy Life Expectancy Score" = h20$Life,
           "Freedom in Life Choices Score" = h20$Freedom,
           "(Low) Institutional Corruption Score" = h20$Corruption)
  })
  
 model <- reactive({
    lm(h20$Score ~ regression_var_chosen())
  })
  
  output$model_summary <- renderPrint({
    paste(
      "Happiness Score  = ", summary(model())$coefficients[1],
      " + ", summary(model())$coefficients[2],
      " * (", input$regression_var,
      "),         ",
      "R-squared: ", summary(model())$r.squared,
      "R-squared adjusted: ", summary(model())$adj.r.squared
      )
  })
  
  output$regression_plot <- renderPlot({
      ggplot(h20, aes(regression_var_chosen(), h20$Score))+ geom_point()+ geom_smooth(method = "lm")+ geom_smooth(se = FALSE, color = "red") + labs(title = "Predicting Happiness Score", x = input$regression_var, y = "Happiness Score")
  })
  
  # build second model
  model_2 <- isolate(lm(h20$Score ~ h20$GDP + h20$Life + h20$Social))
  r_sq_2 <- isolate(summary(model_2))$r.squared * 100
  output$mlr_results1 <- renderPrint(
    paste(
      "Based on a multiple linear regression model, we have determined that ",r_sq_2, " percent of the variation in country happiness score can be predicted by scores for GDP per capita, Life Expectancy and Social Support."
      ))
  output$mlr_results2 <- renderPrint(
    paste(
      "Happiness Score = ",
      summary(model_2)$coefficients[1], " + ",
      summary(model_2)$coefficients[2], " * (GDP score) + ",
      summary(model_2)$coefficients[3], " * (Life expectancy score) + ",
      summary(model_2)$coefficients[4], " * (Social support score)"
    )
  )
    
}


# Run the app ----
shinyApp(ui = ui, server = server)