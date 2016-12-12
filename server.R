function(input, output, session) {
  #compute the binssizes from the number of bins
  compute_bins <- function(x, n) {
    list(
      start = min(x),
      end = max(x),
      size = (max(x) - min(x)) / n
    )
  }
  #read the forex csv
  forex <- read.csv("forex.csv")
  # change in to date format
  forex$Date <- as.Date(forex$Date, format = "%m/%d/%Y")
  #omit the na values
  market <- na.omit(forex)
  #read the currency name
  currency <- reactive({
    paste(input$currency, input$hlc, sep = ".")
  })
  #calculate the returns
  returns <-
    reactive({
      log(forex[, eval(currency())][-1] / forex[, eval(currency())][-length(forex[, eval(currency())])])
    })
  n <- reactive({
    length(returns())
  })
  #calculate the upper limit for the mean CI
  ci <- reactive({
    qt(input$ci / 200 + 0.5, n() - 1) * sd(returns()) / sqrt(n())
  })
  #upper limit for var CI
  up <- reactive({
    qchisq((1 - (100 - input$ci) / 200), n() - 1)
  })
  #lower limit for the var CI
  down <- reactive({
    qchisq((100 - input$ci) / 200, n() - 1)
  })
  #create the qqplot to check if dist is normal
  output$qqplot <- renderPlotly({
    y <- quantile(returns(), c(0.25, 0.75))
    x <- qnorm(c(0.25, 0.75))
    slope <- diff(y) / diff(x)
    int <- y[1L] - slope * x[1L]
    d <- data.frame(resids = returns())
    p <-
      ggplot(d, aes(sample = resids)) + stat_qq() + geom_abline(slope = slope, intercept = int)
    ggplotly(p)
  })
  #create the hstogram and plot a normal dist
  output$plot <- renderPlotly({
    x <- returns()
    fit <- dnorm(x, mean = mean(x), sd = sd(x))
    xbins <- compute_bins(x, input$bins)
    plot_ly(x = x) %>%
      add_histogram(xbins = xbins, name = "Histogram") %>%
      add_lines(y = fit,
                color = "tozeroy",
                name = "Normal") %>%
      layout(yaxis = list(overlaying = "y", side = "left"))
  })
  #calculate the CI for mean
  output$cim <- renderPrint({
    as.symbol(paste("(",
                    mean(returns()) - ci(),
                    ",",
                    mean(returns()) + ci(),
                    ")"))
  })
  #calculate the CI for var
  output$civ <- renderPrint({
    as.symbol(paste(
      input$ci,
      "(",
      var(returns()) * (n() - 1) / down(),
      ",",
      var(returns()) * (n() - 1) / up(),
      ")"
    ))
  })
  #regression wrt time
  regtime <- reactive({
    dates <- forex$Date[2:length(forex$Date)]
    lm(returns() ~ dates)
  })
  #calculate the intercept
  output$intercept <-
    renderPrint(as.symbol(summary(regtime())$coefficients[1]))
  #calculate the slope
  output$slope <-
    renderPrint(as.symbol(summary(regtime())$coefficients[2]))
  #r-square
  output$r2 <- renderPrint(as.symbol(summary(regtime())$r.squared))
  #plot the residual graph
  output$residue1 <- renderPlotly({
    dates <- forex$Date[2:length(forex$Date)]
    fit <- density(summary(regtime())$residuals)
    plot_ly(x = ~ dates) %>%
      add_lines(
        x = fit$x,
        y = fit$y,
        fill = "tozeroy",
        line = list(color = "#5E88FC")
      )
  })
  #plot regression
  output$regression <- renderPlotly({
    dates <- forex$Date[2:length(forex$Date)]
    regtime <- lm(returns() ~ dates)
    plot_ly(x = ~ dates) %>%
      add_markers(
        y = ~ returns(),
        showlegend = TRUE,
        name = "Actual Return",
        marker = list(size = 5,
                      color = 'rgba(255, 100, 193, 1)')
      ) %>%
      add_lines(
        y =  ~ fitted(regtime),
        line = list(color = '#07A4B5'),
        name = "Estimated Return",
        showlegend = TRUE
      ) %>%
      layout(
        yaxis = list(zeroline = FALSE, title = "Log Returns"),
        xaxis = list(zeroline = FALSE, title = "Date")
      )
  })
  #read currency 1
  currency1 <- reactive({
    paste(input$currency1, "Close", sep = ".")
  })
  #read currency 2
  currency2 <- reactive({
    paste(input$currency2, "Close", sep = ".")
  })
  #calculate return for currency 1
  returns1 <-
    reactive({
      log(forex[, eval(currency1())][-1] / forex[, eval(currency1())][-length(forex[, eval(currency1())])])
    })
  #calculate return for currency 2
  returns2 <-
    reactive({
      log(forex[, eval(currency2())][-1] / forex[, eval(currency2())][-length(forex[, eval(currency2())])])
    })
  #perform the t test
  output$ttest <- renderPrint({
    p <- t.test(returns1(), returns2())$p.val
    if (p > (1 - (input$ci2 / 100))) {
      as.symbol("Means are Equal")
    }
    else{
      as.symbol("Means are Unequal")
    }
  })
  #regress 2nd returns on the first
  reg2 <- reactive({
    lm(returns1() ~ returns2())
  })
  #calculate the intercept
  output$intercept2 <-
    renderPrint(as.symbol(summary(reg2())$coefficients[1]))
  #slope
  output$slope2 <-
    renderPrint(as.symbol(summary(reg2())$coefficients[2]))
  #rsquared
  output$r22 <- renderPrint(as.symbol(summary(reg2())$r.squared))
  #residuals
  output$residue2 <- renderPlotly({
    fit <- density(summary(reg2())$residuals)
    plot_ly(x = ~ returns2()) %>%
      add_lines(
        x = fit$x,
        y = fit$y,
        fill = "tozeroy",
        line = list(color = "#5E88FC")
      )
  })
  #plot the regression
  output$regression2 <- renderPlotly({
    reg2 <- lm(returns1() ~ returns2())
    plot_ly(x = ~ returns2()) %>%
      add_markers(
        y = ~ returns1(),
        showlegend = TRUE,
        name = "Actual FX2 Return",
        marker = list(size = 5,
                      color = 'rgba(255, 100, 193, 1)')
      ) %>%
      add_lines(
        y =  ~ fitted(reg2),
        line = list(color = '#07A4B5'),
        name = "Estimated FX2 Return",
        showlegend = TRUE
      ) %>%
      layout(
        yaxis = list(zeroline = FALSE, title = "1st FX pair Returns"),
        xaxis = list(zeroline = FALSE, title = "2nd FX pair Returns")
      )
  })
  currency3 <- reactive({
    paste(input$currency3, "Close", sep = ".")
  })
  #regression on market value
  regm <- reactive({
    lm(market[, eval(currency3())] ~ market[, "market"])
  })
  #calculate the corelation
  output$corrm <-
    renderPrint(as.symbol(cor(market[, eval(currency3())], market[, "market"])))
  output$interceptm <-
    renderPrint(as.symbol(summary(regm())$coefficients[1]))
  output$slopem <-
    renderPrint(as.symbol(summary(regm())$coefficients[2]))
  output$r2m <- renderPrint(as.symbol(summary(regm())$r.squared))
  output$residuem <- renderPlotly({
    fit <- density(summary(regm())$residuals)
    plot_ly(x = ~ market[, "market"]) %>%
      add_lines(
        x = fit$x,
        y = fit$y,
        fill = "tozeroy",
        line = list(color = "#5E88FC")
      )
  })
  output$regressionm <- renderPlotly({
    regm <- lm(market[, eval(currency3())] ~ market[, "market"])
    plot_ly(x = ~ market[, "market"]) %>%
      add_markers(
        y = ~ market[, eval(currency3())],
        showlegend = TRUE,
        name = "Actual FX Value",
        marker = list(size = 5,
                      color = 'rgba(255, 100, 193, 1)')
      ) %>%
      add_lines(
        y =  ~ fitted(regm),
        line = list(color = '#07A4B5'),
        name = "Estimated FX Value",
        showlegend = TRUE
      ) %>%
      layout(
        yaxis = list(zeroline = FALSE, title = "FX Value"),
        xaxis = list(zeroline = FALSE, title = "Market Value")
      )
  })
}