## ---------------------------
##
## Script name: server.R
##
## Purpose of script: Specifies server interface for PAX-Crossings Dashboard
##
## Author: Domingo Velazquez
##
## Date Created: 2020-03-20
##
## ---------------------------

# Load variables to use 

#Prueba de cambio
#dos
caso1 <<- list(a = c("2"))
caso2 <<- list(a = c("3"))

# Define Server

shinyServer(function(input, output) {
  # Reactive expressions for direction radio button
  
  FilterDirection <- reactive({
    if (input$DirectionID == 1) {
      df <- df_global[,c("Date", 
                         "A_Total", 
                         "B_Total", 
                         "H_Total", 
                         "A_Day", 
                         "H_Day", 
                         "A_WD", 
                         "H_WD", 
                         "A_M", 
                         "H_M", 
                         "A_DM", 
                         "H_DM", 
                         "Year")]
    } else {
      if (input$DirectionID == 2) {
        df <- df_global[,c("Date", 
                           "A_NB", 
                           "B_NB", 
                           "H_NB", 
                           "A_Day", 
                           "H_Day", 
                           "A_WD", 
                           "H_WD", 
                           "A_M", 
                           "H_M", 
                           "A_DM", 
                           "H_DM", 
                           "Year")]
      } else {
        df <- df_global[,c("Date",
                           "A_SB", 
                           "B_SB", 
                           "H_SB", 
                           "A_Day",
                           "H_Day",
                           "A_WD", 
                           "H_WD", 
                           "A_M", 
                           "H_M", 
                           "A_DM",
                           "H_DM",
                           "Year")]
      }
    }
    df
  })
  
  # Reactive expressions for base year drill down
  
  FilterBYear <- reactive({
    df1 <- subset(FilterDirection(), 
                  Year == input$PeriodID)
    df1
  })
  
  # Reactive expressions for recurrency drill down
  
  FilterRecurrency <- reactive({
    if (input$RecurrencyID == 2) {
      df2 <- subset(FilterBYear(),
                    A_M == input$MonthsID)
    } else {
      df2 <- FilterBYear()
    }
    df2
  })
  
  # Reactive expressions to get actuals and budget
  
  actual <- reactive({
    act <- data.frame(Actual = FilterRecurrency()[,2])
    act
  })
  
  actCum <- reactive({
    ac <- data.frame(actual())
    dfacum <- data.frame(actual())
    for (i in 1:ncol(ac)) {
      dfacum[,i] <- cumsum(ac[,i])
    }
    dfacum
  })
  
  budget <- reactive({
    bgt <- data.frame(Budget = FilterRecurrency()[,3])
    bgt
  })
  
  bgtCum <- reactive({
    bg <- data.frame(budget())
    dfbcum <- data.frame(budget())
    for (i in 1:ncol(bg)) {
      dfbcum[,i] <- cumsum(bg[,i])
    }
    dfbcum
  })
  
  # Reactive expressions to get date
  
  fecha <- reactive({
    f <- data.frame(Date = as.Date(as.character(FilterRecurrency()$Date)))
    f
  })
  
  # Reactive expresion to get weekdays or months
  
  day <- reactive({
    if(input$RecurrencyID == 2){
      wd <- data.frame(Day = FilterRecurrency()[,5])
    } else {
      wd <- data.frame(Day = FilterRecurrency()[,9])
    }
    wd
  })
  
  # Reactive expressions for scenario checkbox group
  
  FilterScenario <- reactive({
    if (input$RecurrencyID == 2) {
      df <- t(FilterRecurrency()[,-2])
      v <- data.frame(v = c(0,2,3,0,0,0,0,0,0,0,0,0))
      df <- cbind(df,v)
      df <- subset(df, v %in% input$ScenarioID)
    } else {
      df <- t(FilterRecurrency()[,-2])
      v <- data.frame(v = c(0,2,3,0,0,0,0,0,0,0,0,0))
      df <- cbind(df,v)
      df <- subset(df, v %in% input$ScenarioID)
      df <- df
    }
    df
  })
  
  # List scenario
  
  getlist <- reactive({
    if (input$RecurrencyID == 2) {
      l <- input$ScenarioID 
      l <- c(l)
    } else {
      l <- input$ScenarioID 
      l <- c(l)
    }
    l
  })
  
  # Index name to selection
  
  nombres <- reactive({
    nn <- list(getlist())
    if (nn %in% caso1) {
      dfn <- list("Budget")
    } else {
      if (nn %in% caso2) {
        dfn <- list("YoY")
      } else {
        dfn <- list("Budget", "YoY")
      }
    }
    dfn
  })
  
  nombresmn <- reactive({
    nnm <- list(getlist())
    if (nnm %in% caso1) {
      mdfn <- list("Mean Budget")
    } else {
      if (nnm %in% caso2) {
        mdfn <- list("Mean YoY")
      } else {
        mdfn <- list("Mean Budget", "Mean YoY")
      }
    }
    mdfn
  })
  
  # Index color to selection
  
  colores <- reactive({
    clrs <- list(getlist())
    if (clrs %in% caso1) {
      dfc <- list("#D8D8D8")
    } else {
      if (clrs %in% caso2) {
        dfc <- list("#3F2A55")
      } else {
        dfc <- list("#D8D8D8", "#3F2A55")
      }
    }
    dfc
  })
  
  # Index form to selection
  
  forms <- reactive({
    frm <- list(getlist())
    if (frm %in% caso1) {
      frdf <- list("dot")
    } else {
      if (frm %in% caso2) {
        frdf <- list("")
      } else {
        frdf <- list("dot", "")
      }
    }
    frdf
  })
  
  # Traspose and delete extra id row
  
  FilterScenario2 <- reactive({
    df <- t(FilterScenario())
    df <- data.frame(df[-nrow(df),])
    df
  })
  
  # Change data from factor to numeric
  
  Mutate <- reactive({
    df <- data.frame(FilterScenario2())
    df <- mutate_all(df, function(x) as.character(x))
    df <- mutate_all(df, function(x) as.numeric(x))
    df
  })
  
  # Diff calculations unit and percentage
  
  Diff_0 <- reactive({
    df <- Mutate()
    dfdif <- Mutate()
    for (i in 1:ncol(dfdif)) {
      dfdif[,i] <- actual() - df[,i]
    }
    dfdif
  })
  
  Diff <- reactive({
    at <- data.frame(actual())
    row_sub <- apply(at, 1,function(row) all(row !=0 ))
    #at <- data.frame(at[row_sub,])
    d <- data.frame(Diff_0())
    nd <- nrow(data.frame(Diff_0()))
    na <- nrow(data.frame(at[row_sub,]))
    x <- nd - na
    nc <- ncol(data.frame(Diff_0()))
    if(nd > na){
      if(nc == 1){
        df1 <- data.frame(x = d[1:na,])
        df2 <- data.frame(x = rep(0,x))
        df <- rbind(df1,df2)
      } else {
        df1 <- data.frame(x = d[1:na,1], y = d[1:na,2])
        df2 <- data.frame(x = rep(0,x), y = rep(0,x))
        df <- rbind(df1,df2)
      }
      df
      } else{
        df <- Diff_0()
      }
    df
    dftr <- df
    dftr
  })
  
  pDiff <- reactive({
    df <- Mutate()
    dfpdif <- Mutate()
    for (i in 1:ncol(dfpdif)) {
      dfpdif[,i] <- round(((actual()/df[,i])-1)*100,0)
    }
    dfpdif
  })
  
  Mean <- reactive({
    contador <- data.frame(Actual = dflma()$Actual)
    x <- data.frame(Diff()[1:nrow(contador),])
    df <- data.frame(Diff()[1:nrow(contador),])
    for (i in 1:ncol(df)) {
      for (j in 1:nrow(df)) {
        df[j,i] <- round(mean(x[,i]),0)
      }}
    df
  })
  
  pMean <- reactive({
    contador <- data.frame(Actual = dflma()$Actual)
    x <- data.frame(pDiff()[1:nrow(contador),])
    df <- data.frame(pDiff())
    for (i in 1:ncol(df)){
      for (j in 1:nrow(df)){
        df[j,i] <- round(mean(x[,i]),0)
      }}
    df
  })
  
  # Cumulative calculations unit and percentage
  
  Cum <- reactive({
    x <- Mutate()
    df <- Mutate()
    for (i in 1:ncol(x)) {
      df[,i] <- cumsum(x[,i])
    }
    df
  })
  
  Diff_0_a <- reactive({
    df <- Cum()
    dfdif_a <- Cum()
    for (i in 1:ncol(dfdif_a)) {
      dfdif_a[,i] <- actCum() - df[,i]
    }
    dfdif_a
  })
  
  Diff_a <- reactive({
    at <- data.frame(actual())
    row_sub <- apply(at, 1,function(row) all(row !=0 ))
    #at <- data.frame(at[row_sub,])
    d <- data.frame(Diff_0_a())
    nd <- nrow(data.frame(Diff_0_a()))
    na <- nrow(data.frame(at[row_sub,]))
    x <- nd - na
    nc <- ncol(data.frame(Diff_0_a()))
    if(nd > na){
      if(nc == 1){
        df1 <- data.frame(x = d[1:na,])
        df2 <- data.frame(x = rep(0,x))
        df <- rbind(df1,df2)
      } else {
        df1 <- data.frame(x = d[1:na,1], y = d[1:na,2])
        df2 <- data.frame(x = rep(0,x), y = rep(0,x))
        df <- rbind(df1,df2)
      }
      df
    } else{
      df <- Diff_0()
    }
    df
    dftr <- df
    dftr
  })
  
  pDiff_a <- reactive({
    df <- Cum()
    dfpdif <- Cum()
    for (i in 1:ncol(dfpdif)) {
      dfpdif[,i] <- round(((actCum()/df[,i])-1)*100,0)
    }
    dfpdif
  })
  
  Mean_a <- reactive({
    contador <- data.frame(Actual = dflma()$Actual)
    x <- data.frame(Diff_a()[1:nrow(contador),])
    df <- data.frame(Diff_a()[1:nrow(contador),])
    for (i in 1:ncol(df)) {
      for (j in 1:nrow(df)) {
        df[j,i] <- round(mean(x[,i]),0)
      }}
    df
  })
  
  pMean_a <- reactive({
    contador <- data.frame(Actual = dflma()$Actual)
    x <- data.frame(pDiff_a()[1:nrow(contador),])
    df <- data.frame(pDiff_a())
    for (i in 1:ncol(df)){
      for (j in 1:nrow(df)){
        df[j,i] <- round(mean(x[,i]),0)
      }}
    df
  })
  
 # Join week days df and passenger crossings
  
  dfday <- reactive({
    if (length(input$ScenarioID) != 0) {
    d <- data.frame(Day = day())
    sumday <- data.frame(Day = day(),
                         Actual =actual(),
                         Mutate())
    ncd <- ncol(sumday)
    row_sub <- apply(sumday, 1,function(row) all(row !=0 ))
    sumday <- sumday[row_sub,]
    nrd <- nrow(sumday)
    nrds <- nrd + 1
    nd <- nrow(data.frame(Mutate()))
    at <- data.frame(actual())
    row_sub <- apply(at, 1,function(row) all(row !=0 ))
    na <- nrow(data.frame(at[row_sub,]))
    if(nd > na){
      if(ncd == 3){
        df1 <- data.frame(Day = sumday[,1], Actual = sumday[,2],  x = sumday[,3])
        df2 <- data.frame(Day = d[nrds:nrow(d),] , Actual = rep(0, nrow(d) - nrd) , x = rep(0,nrow(d) - nrd))
        df <- rbind(df1,df2)
      } else {
        df1 <- data.frame(Day = sumday[,1], Actual = sumday[,2],  x = sumday[,3], y = sumday[,4])
        df2 <- data.frame(Day = d[nrds:nrow(d),] , Actual = rep(0, nrow(d) - nrd) , x = rep(0,nrow(d) - nrd), y = rep(0,nrow(d) - nrd))
        df <- rbind(df1,df2)
      }
      df
    } else{
      df <- data.frame(Day = day(), Actual =actual(), Mutate())
      }
    df
    } else {
      d <- data.frame(Day = day())
      sumday <- data.frame(Day = day(),
                           Actual =actual())
      df <- data.frame(Day = sumday[,1], Actual = sumday[,2])
      df
    }
  })
  
  dfwdays <- reactive({
    week <- dfday()
    weeks <- week %>% 
                  group_by(Day) %>%
                                summarise_all(sum)
    weeks
  })
  
  #####
  #dfdayA <- reactive({
   # d <- data.frame(Day = day())
    #sumday <- data.frame(Day = day(),
   
  #                      Actual =actual())
    #df <- data.frame(Day = sumday[,1], Actual = sumday[,2])
    #TTT <<- df
    #df
  #})
  
  #dfwdaysA <- reactive({
   # week <- dfdayA()
    #weeks <- week %>% 
     # group_by(Day) %>%
      #summarise_all(sum)
    #weetks<<- weeks
  #})
  
  # Reactive expression to cut de actual vector
  
  dflma <- reactive({
    fechas <- data.frame(Date = as.numeric(factor(fecha()$Date)))
    df <- data.frame(fechas, data.frame(Actual = actual()))
    row_sub <- apply(df, 1,function(row) all(row !=0 ))
    df <- df[row_sub,]
    df
  })
  
  dflmb <- reactive({
    fechas <- data.frame(Date = as.numeric(factor(fecha()$Date)))
    df <- data.frame(fechas, data.frame(Budget = budget()))
    row_sub <- apply(df, 1, function(row) all(row !=0 ))
    df <- df[row_sub,]
    df
  })
  
  # Crossing plots
  
  ## Daily
  
  output$rawPlot <- renderPlotly({
    if (length(input$ScenarioID) != 0) {
    dfact1 <- data.frame(Actual = dflma()$Actual)
    fechasr <- data.frame(Date = fecha()[1:nrow(dfact1),])
    dfact2 <- data.frame(fechasr, dfact1)
    df5 <- data.frame(Date = fecha(), Mutate())
    l2 <- nombres()
    l3 <- colores()
    fig <- plot_ly(df5, type = "scatter", mode = "none")
    fig <- fig %>% add_lines(y = dfact2[,2],
                             x = dfact2[,1],
                             mode = "lines", 
                             line = list(color = "#FF3094"),
                             name = "Actual",
                             hoverinfo = "text+name", 
                             text = paste(format(dfact2$Date, "%b %d"), comma(dfact2[,2]))
    )
    #add_lines(data = b, x = ~x, y = ~y)
    for (i in 2:ncol(df5)){
      fig <- fig %>% add_trace(y = df5[,i],
                               x = df5[,1],
                               mode = "lines", 
                               line = list(color = l3[[i-1]]),
                               name = l2[[i-1]],
                               hoverinfo = "text+name", 
                               text = paste(format(df5$Date, "%b %d"), comma(df5[,i])))
    }
    fig <- fig %>% layout(showlegend = FALSE,
                          hovermode = "compare") %>%
                                                  config(displayModeBar = TRUE)
    } else {
      dfact1 <- data.frame(Actual = dflma()$Actual)
      fechasr <- data.frame(Date = fecha()[1:nrow(dfact1),])
      dfact2 <- data.frame(fechasr, dfact1)
      l2 <- nombres()
      l3 <- colores()
      fig <- plot_ly(dfact2, type = "scatter", mode = "none")
      fig <- fig %>% add_lines(y = dfact2[,2],
                               x = dfact2[,1],
                               mode = "lines", 
                               line = list(color = "#FF3094"),
                               name = "Actual",
                               hoverinfo = "text+name", 
                               text = paste(format(dfact2$Date, "%b %d"), comma(dfact2[,2]))
      )
     fig <- fig %>% layout(showlegend = FALSE,
                            hovermode = "compare") %>%
                                                    config(displayModeBar = TRUE)
      }
  })
  
  ## Cumulative
  
  output$cumPlot <- renderPlotly({
    if (length(input$ScenarioID) != 0) {
    dfact0 <- data.frame(Actual = dflma()$Actual)
    dfact <- data.frame(Date = fecha()[1:nrow(dfact0),], actCum()[1:nrow(dfact0),])
    df6 <- data.frame(Date = fecha(), Cum())
    l2 <- nombres()
    l3 <- colores()
    fig <- plot_ly(df6, type = "scatter", mode = "lines")
    fig <- fig %>% add_lines(y = dfact[,2],
                             x = dfact[,1],
                             line = list(color = "#FF3094"),
                             name = "Actual",
                             fill = 'tozeroy',
                             fillcolor = list(color = "#FF3094"),
                             hoverinfo = "text+name", 
                             text = paste(format(dfact$Date, "%b %d"), comma(dfact[,2]))
    )
    for (i in 2:ncol(df6)){
      fig <- fig %>% add_trace(y = df6[,i],
                               x = df6[,1],
                               line = list(color = l3[[i-1]]),
                               name = l2[[i-1]],
                               fill = 'tozeroy',
                               fillcolor = list(color = l3[[i-1]]),
                               hoverinfo = "text+name", 
                               text = paste(format(df6$Date, "%b %d"), comma(df6[,i])))
    }
    fig <- fig %>% layout(showlegend = TRUE,
                          hovermode = "compare") %>%
                                                  config(displayModeBar = TRUE)
    } else{
      dfact0 <- data.frame(Actual = dflma()$Actual)
      dfact <- data.frame(Date = fecha()[1:nrow(dfact0),], actCum()[1:nrow(dfact0),])
      l2 <- nombres()
      l3 <- colores()
      fig <- plot_ly(dfact, type = "scatter", mode = "lines")
      fig <- fig %>% add_lines(y = dfact[,2],
                               x = dfact[,1],
                               line = list(color = "#FF3094"),
                               name = "Actual",
                               fill = 'tozeroy',
                               fillcolor = list(color = "#FF3094"),
                               hoverinfo = "text+name", 
                               text = paste(format(dfact$Date, "%b %d"), comma(dfact[,2]))
      )
      fig <- fig %>% layout(showlegend = TRUE,
                            hovermode = "compare") %>%
                                                    config(displayModeBar = TRUE)
    }
  })
  
 
  # Reactive expression for monthly and yearly plot change
  
  diff_r <- reactive({
    dif <- data.frame(Date = fecha(), Diff())
    dif
  })
  
  diff_pr <- reactive({
    dif <- data.frame(Date = fecha(), pDiff())
    dif
  })
  
  diff_ra <- reactive({
    dif <- data.frame(Date = fecha(), Diff_a())
    dif
  })
  
  diff_pra <- reactive({
    dif <- data.frame(Date = fecha(), pDiff_a())
    dif
  })
  
  
  # Differences plots
  
  ## Units daily
  
  output$difPlot <- renderPlotly({
    contador <- data.frame(Actual = dflma()$Actual)
    mn <- data.frame(Date = fecha()[1:nrow(contador),], Mean()[1:nrow(contador),])
    pmn <- data.frame(Date = fecha()[1:nrow(contador),], pMean()[1:nrow(contador),])
    dif <- diff_r()
    pdif <- diff_pr()
    f1 <- forms()
    f2 <- nombresmn()
    l4 <- nombres()
    l5 <- colores()
    fig <- plot_ly()
    for (i in 2:ncol(dif)){
      fig <- fig %>% add_lines(y = mn[,i], 
                               x = mn[,1], 
                               type = "scatter",
                               mode = "lines",
                               line = list(color = "#FF3094", dash = f1[[i-1]]),
                               name = f2[[i-1]],
                               hoverinfo = "text+name", 
                               text = paste(format(mn$Date, "%b %d"), comma(mn[,i]), "(",comma(pmn[,i]),"%",")"))
    }
    for (i in 2:ncol(dif)){
      fig <- fig %>% add_trace (dif,
                                y = dif[,i], 
                                x = dif[,1], 
                                marker = list(color = l5[[i-1]]),
                                type = "bar", 
                                showlegend = FALSE, 
                                name = l4[[i-1]],
                                hoverinfo = "text+name", 
                                text = paste(format(dif$Date, "%b %d"), comma(dif[,i]), "(",comma(pdif[,i]),"%",")"))
    }
    fig <- fig %>% layout(showlegend = FALSE,
                          hovermode = "compare") %>%
      config(displayModeBar = TRUE)
  })
  
  ## Units cumulative
  
  output$adifPlot <- renderPlotly({
    contador <- data.frame(Actual = dflma()$Actual)
    mn <- data.frame(Date = fecha()[1:nrow(contador),], Mean_a()[1:nrow(contador),])
    pmn <- data.frame(Date = fecha()[1:nrow(contador),], pMean_a()[1:nrow(contador),])
    dif <- diff_ra()
    pdif <- diff_pra()
    f1 <- forms()
    f2 <- nombresmn()
    l4 <- nombres()
    l5 <- colores()
    fig <- plot_ly()
    for (i in 2:ncol(dif)){
      fig <- fig %>% add_trace (mn,
                                y = mn[,i], 
                                x = mn[,1], 
                                type = "scatter",
                                mode = "lines",
                                line = list(color = "#FF3094", dash = f1[[i-1]]),
                                name = f2[[i-1]],
                                hoverinfo = "text+name", 
                                text = paste(format(mn$Date, "%b %d"), comma(mn[,i]), "(",comma(pmn[,i]),"%",")"))
    }
    for (i in 2:ncol(dif)){
      fig <- fig %>% add_trace (dif,
                                y = dif[,i], 
                                x = dif[,1], 
                                marker = list(color = l5[[i-1]]),
                                type = "bar", 
                                showlegend = FALSE, 
                                name = l4[[i-1]],
                                hoverinfo = "text+name", 
                                text = paste(format(dif$Date, "%b %d"), comma(dif[,i]), "(",comma(pdif[,i]),"%",")"))
    }
    fig <- fig %>% layout(showlegend = TRUE,
                          hovermode = "compare") %>%
      config(displayModeBar = TRUE)
  })
 
  # Reactive expressions to set name of day or month
  
  ww <- reactive({
    if(input$RecurrencyID == 1){
      df <- list("January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December")
    } else {
      df <- list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    }
    df
  })
  
  www <- reactive({
    if(input$RecurrencyID == 1){
      df <- list(1,2,3,4,5,6,7,8,9,10,11,12)
    } else {
      df <- list(1,2,3,4,5,6,7)
    }
    df
  })
  
  ## Weekday plots
  
  output$wdPlot <- renderPlotly({
    if (length(input$ScenarioID) != 0) {
      wnames <- ww()
      wnamesf <- www()
      wk <- data.frame(dfwdays()[,1],dfwdays()[,-1:-2])
      wka <- data.frame(dfwdays()[,1],dfwdays()[,2])
      g1 <- nombres()
      g2 <- colores()
      fig <- plot_ly(orientation = "v")
      fig <- fig %>% add_trace (wka,
                                y = wka[,2], 
                                x = wka[,1],
                                type = "bar",
                                marker = list(color = "#FF3094"),
                                showlegend = FALSE,
                                name = "Actual",
                                hoverinfo = "text+name", 
                                text = paste(wnames, comma(wka[,2])))
      for (i in 2:ncol(wk)){
        fig <- fig %>% add_trace (wk,
                                  y = wk[,i], 
                                  x = wk[,1], 
                                  marker = list(color = g2[[i-1]]),
                                  type = "bar", 
                                  showlegend = FALSE, 
                                  name = g1[[i-1]],
                                  hoverinfo = "text+name", 
                                  text = paste(wnames, comma(wk[,i])))
      }
      fig <- fig %>% layout(xaxis = list(ticktext =  wnames, tickvals = wnamesf))
      fig <- fig %>% layout(showlegend = TRUE,
                            hovermode = "compare") %>%
                                                    config(displayModeBar = TRUE)
    } else{
      wnames <- ww()
      wnamesf <- www()
      wka <- data.frame(dfwdays()[,1],dfwdays()[,2])
      g1 <- nombres()
      g2 <- colores()
      fig <- plot_ly(orientation = "v")
      fig <- fig %>% add_trace (wka,
                                y = wka[,2], 
                                x = wka[,1],
                                type = "bar",
                                marker = list(color = "#FF3094"),
                                showlegend = FALSE,
                                name = "Actual",
                                hoverinfo = "text+name", 
                                text = paste(wnames, comma(wka[,2])))
      fig <- fig %>% layout(xaxis = list(ticktext =  wnames, tickvals = wnamesf))
      fig <- fig %>% layout(showlegend = TRUE,
                            hovermode = "compare") %>%
                                                    config(displayModeBar = TRUE)
    }
  })
  
  # Reactive expressions for lm
  
  fita <- reactive({
    xfita <- dflma()
    afit <- lm(Actual~Date, xfita)
    afit
  })
  
  fitb <- reactive({
    xfitb <- dflmb()
    bfit <- lm(Budget~Date, xfitb)
    bfit
  })
  
  # Reactive expressions for value box
  
  vb <- reactive({
   v <- FilterBYear()[,2:4]
   row_sub <- apply(v, 1, function(row) all(row !=0 ))
   v <- data.frame(v[row_sub,])
   v <- v[nrow(v),]
  })
    
  estado_1 <- reactive({
    nn <- round(((vb()[,1]/vb()[,3]))*100,0)
    if ( nn >= 110) {
      e <- "fuchsia"
    } else {
      if ( nn >= 100 && nn < 110) {
        e <- "green"
      } else if (nn >= 97 && nn < 100) {
        e <- "yellow"
      } else if (nn >= 92 && nn < 97) {
        e <- "red"
      } else {
        e <- "blue"
      }
    }
    e
  }) 
  
  estado_2 <- reactive({
    nn <- round(((vb()[,1]/vb()[,2]))*100,0)
    if ( nn >= 110) {
      e <- "fuchsia"
    } else {
      if ( nn >= 100 && nn < 110) {
        e <- "green"
      } else if (nn >= 97 && nn < 100) {
        e <- "yellow"
      } else if (nn >= 92 && nn < 97) {
        e <- "red"
      } else {
        e <- "blue"
      }
    }
    e
  })
  
  estado_3 <- reactive({
    nn <- round(((vbCum()[,1]/vbCum()[,3]))*100,0)
    if ( nn >= 110) {
      e <- "fuchsia"
    } else {
      if ( nn >= 100 && nn < 110) {
        e <- "green"
      } else if (nn >= 97 && nn < 100) {
        e <- "yellow"
      } else if (nn >= 92 && nn < 97) {
        e <- "red"
      } else {
        e <- "blue"
      }
    }
    e
  })
  
  estado_4 <- reactive({
    nn <- round(((vbCum()[,1]/vbCum()[,2]))*100,0)
    if ( nn >= 110) {
      e <- "fuchsia"
    } else {
      if ( nn >= 100 && nn < 110) {
        e <- "green"
      } else if (nn >= 97 && nn < 100) {
        e <- "yellow"
      } else if (nn >= 92 && nn < 97) {
        e <- "red"
      } else {
        e <- "blue"
      }
    }
    e
  })
  
  # Value boxes  
    
  output$actualBox <- renderValueBox({
     valueBox(
      tags$p(comma(vb()[,1]), style = "font-size: 60%;"),
      tags$p(paste0(format(Sys.Date()-1, "%d-%B-%Y"), " ", ": Actual"), style = "font-size: 90%;"),
      #icon = icon("user", lib = "font-awesome"),
      color = "purple"
    )
  })
  
  output$yoyBox <- renderValueBox({
     valueBox(
      tags$p(paste(comma(vb()[,3])," ","|"," ",round(((vb()[,1]/vb()[,3])-1)*100,0),"%"), style = "font-size: 60%;"),
      tags$p(paste0(format(Sys.Date()-1, "%d-%B-%Y"), " ", ": YoY"), style = "font-size: 90%;"),
      #icon = icon("user", lib = "font-awesome"),
      color = estado_1()
    )
  })
  
  output$budgetBox <- renderValueBox({
    valueBox(
      tags$p(paste(comma(vb()[,2])," ","|"," ",round(((vb()[,1]/vb()[,2])-1)*100,0),"%"), style = "font-size: 60%;"),
      tags$p(paste0(format(Sys.Date()-1, "%d-%B-%Y"), " ", ": Budget"), style = "font-size: 90%;"),
      #icon = icon("user", lib = "font-awesome"),
      color = estado_2()
    )
  })
  
  vbCum <- reactive({
    x <- FilterRecurrency()[,2:4]
    row_sub <- apply(x, 1, function(row) all(row !=0 ))
    x <- data.frame(x[row_sub,])
    for (i in 1:ncol(x)) {
      x[,i] <- cumsum(x[,i])
    }
    x <- x[nrow(x),]
  })
  
  output$actualBox2 <- renderValueBox({
    valueBox(
      tags$p(comma(vbCum()[,1]), style = "font-size: 60%;"),
      tags$p("Selected Period: Actual", style = "font-size: 90%;"),
      #icon = icon("users", lib = "font-awesome"),
      color = "purple"
    )
  })
  
  output$yoyBox2 <- renderValueBox({
    valueBox(
      tags$p(paste(comma(vbCum()[,3])," ","|"," ",round(((vbCum()[,1]/vbCum()[,3])-1)*100,0),"%"), style = "font-size: 60%;"),
      tags$p("Selected Period: YoY", style = "font-size: 90%;"),
      #icon = icon("users", lib = "font-awesome"),
      color = estado_3()
    )
  })
  
  output$budgetBox2 <- renderValueBox({
    valueBox(
      tags$p(paste(comma(vbCum()[,2])," ","|"," ",round(((vbCum()[,1]/vbCum()[,2])-1)*100,0),"%"), style = "font-size: 60%;"),
      tags$p("Selected Period: Budget", style = "font-size: 90%;"),
      #icon = icon("users", lib = "font-awesome"),
      color = estado_4()
    )
  })
  
  fitvalues <- reactive({
    srya <- summary(fita())
    sryb <- summary(fitb())
    fva <- data.frame(aBeta = srya$coefficients[ , 1], aSTD = srya$coefficients[ , 2], aCIB = (srya$coefficients[ , 1]-(1*srya$coefficients[ , 2])), aCIA=(srya$coefficients[ , 1]+(1*srya$coefficients[ , 2])))
    fvb <- data.frame(bBeta = sryb$coefficients[ , 1], bSTD = sryb$coefficients[ , 2], bCIB = (sryb$coefficients[ , 1]-(1*sryb$coefficients[ , 2])), bCIA=(sryb$coefficients[ , 1]+(1*sryb$coefficients[ , 2])))
    fv <- data.frame(fva, fvb)
    fv
  })
  
  output$deltaBox <- renderValueBox({
    valueBox(
      tags$p(paste(comma(fitvalues()[2,1])," ","vs"," ",comma(fitvalues()[2,5])), style = "font-size: 60%;"),
      tags$p("Selected Period: Delta (Actual vs Budget)", style = "font-size: 90%;"),
      #icon = icon("users", lib = "font-awesome"),
      color = "purple"
    )
  })
  
  #output$ciBox <- renderValueBox({
   # valueBox(
    #  tags$p(paste0("[",comma(fitvalues()[2,3])," ",","," ",comma(fitvalues()[2,4]),"]"," ","vs"," ","[",comma(fitvalues()[2,7])," ",","," ",comma(fitvalues()[2,8]),"]"), style = "font-size: 60%;"),
     # tags$p("Selected Period: Delta (Actual vs Budget)", style = "font-size: 90%;"),
      #icon = icon("users", lib = "font-awesome"),
      #color = "purple"
    #)
  #})
  
  # Name of Cumulative box
  
  output$box <- renderText ({
    if (input$RecurrencyID == 2) {
      titulo <- "Month to Date"
    } else {
      titulo <- "Year to Date"
    }
    titulo 
  })
  
  
  
})
