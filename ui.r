if(!require(shiny)){
  install.packages("shiny")
  library(shiny)
}
if(!require(shinythemes)){
  install.packages("shinythemes")
  library(shinythemes)
}
if(!require(plotly)){
  install.packages("plotly")
  library(plotly)
}
#create drop down list elements
hlc <-
  c("High",
    "Low",
    "Close")
currencies <-
  c(
    "EUR.USD",
    "USD.JPY",
    "USD.CHF",
    "GBP.USD",
    "USD.CAD",
    "AUD.USD",
    "NZD.USD",
    "EUR.JPY",
    "GBP.AUD",
    "EUR.AUD"
  )
#create the layout of the UI
#here we are using the navbar layout
navbarPage(
  "FX Analysis Dashboard",
  #apply theme
  theme = shinytheme("darkly"),
  #create tab for single currency analysis
  tabPanel("Single FX Analysis",
           sidebarLayout(
             #create sidebar elements
             sidebarPanel(
               selectInput(
                 "currency",
                 label = h3("Currency pair"),
                 choices = currencies
               ),
               selectInput(
                 "hlc",
                 label = h3("High-Low-Close"),
                 choices = hlc
               ),
               sliderInput(
                 "bins",
                 label = "Bins",
                 min = 1,
                 max = 20,
                 value = 10,
                 step = 1
               ),
               sliderInput(
                 "ci",
                 label = "Confidence Interval",
                 min = 0,
                 max = 100,
                 value = 20,
                 step = 5
               ),
               #show the confidence intervals
               h4('Confidence Interval of Mean'),
               verbatimTextOutput("cim"),
               h4('Confidence Interval of Variance'),
               verbatimTextOutput("civ")
             ),
             mainPanel(tabsetPanel(
               #create tab for histogram
               tabPanel(
                 "Histogram & Normal Distribution",
                 h4('Histogram & Normal Distribution'),
                 plotlyOutput("plot", height = 500),
                 h4('Q-Q Plot'),
                 plotlyOutput("qqplot", height = 500)
               ),
               #create tab for regression on time
               tabPanel(
                 "Regressing returns on time",
                 h4('Regression Plot'),
                 plotlyOutput("regression", height = 500),
                 column(4,
                        h4("Intercept Estimate"),
                        verbatimTextOutput("intercept")),
                 column(4,
                        h4("Slope Estimate"),
                        verbatimTextOutput("slope")),
                 column(4,
                        h4("R-square"),
                        verbatimTextOutput("r2")),
                 h4('Residual Plot'),
                 plotlyOutput("residue1", height = 500)
               )
             ))
           )),
  #create main tab for dual currency analysis
  tabPanel("Dual FX Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "currency1",
                 label = h3("First Currency pair"),
                 choices = currencies
               ),
               selectInput(
                 "currency2",
                 label = h3("Second Currency pair"),
                 choices = currencies,
                 selected = currencies[2]
               ),
               sliderInput(
                 "ci2",
                 label = "Confidence Interval for t-test",
                 min = 0,
                 max = 100,
                 value = 20,
                 step = 5
               ),
               h4("T-test for equality of population means"),
               verbatimTextOutput("ttest")
             ),
             mainPanel(
               h4('Regressing first FX pair returns on second'),
               plotlyOutput("regression2", height = 500),
               column(4,
                      h4("Intercept Estimate"),
                      verbatimTextOutput("intercept2")),
               column(4,
                      h4("Slope Estimate"),
                      verbatimTextOutput("slope2")),
               column(4,
                      h4("R-square"),
                      verbatimTextOutput("r22")),
               h4('Residual Plot'),
               plotlyOutput("residue2", height = 500)
             )
           )),
  #last tab for fx value and market value analysis
  tabPanel("Market FX Analysis",
           sidebarLayout(
             sidebarPanel(
               selectInput(
                 "currency3",
                 label = h3("Currency pair"),
                 choices = currencies
               ),
               h4('Coorelation'),
               verbatimTextOutput("corrm"),
               #create the text box which shows what we are trying to ahieve
               h4(
                 "We are trying to verify the general tendencis of price movement in currency trading w.r.t the market value."
               ),
               h4("The general expectation is-"),
               h4("When market moves up, the following pairs move up-"),
               h4("AUD/USD"),
               h4("NSD/USD"),
               h4("EUR/JPY"),
               h4("and the pairs which face downward pressure are-"),
               h4("USD/CAD"),
               h4("EUR/AUD"),
               h4("GPB/AUD"),
               h4(
                 "To check the validity of this claim, we are looking for corealtion between market value and the FX pair value."
               ),
               h4(
                 "
                 We also try to estimate the value of the FX pair by regressing on the market value."
               )
               ),
             mainPanel(
               h4('Regression Plot'),
               plotlyOutput("regressionm", height = 500),
               column(4,
                      h4("Intercept Estimate"),
                      verbatimTextOutput("interceptm")),
               column(4,
                      h4("Slope Estimate"),
                      verbatimTextOutput("slopem")),
               column(4,
                      h4("R - square"),
                      verbatimTextOutput("r2m")),
               h4('Residual Plot'),
               plotlyOutput("residuem", height = 500)
             )
             ))
  )