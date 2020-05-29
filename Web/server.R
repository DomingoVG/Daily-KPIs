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

caso1 <<- list(a = c("2"))
caso2 <<- list(a = c("3"))

# Define Server

shinyServer(function(input, output) {
  
  ############################### Paasenger Count Server ###################################
  
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
    df <- subset(FilterDirection(), 
                  Year == input$PeriodID)
    df
  })
  
  # Reactive expressions for recurrency drill down
  
  FilterRecurrency <- reactive({
    if (input$RecurrencyID == 2) {
      df <- subset(FilterBYear(),
                    A_M == input$MonthsID)
    } else {
      df <- FilterBYear()
    }
    df
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
      wd <- data.frame(Day = FilterRecurrency()[,5])
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
                             text = paste(format(dfact2$Date, "%b %d"),":", comma(dfact2[,2]))
    )
    for (i in 2:ncol(df5)){
      fig <- fig %>% add_trace(y = df5[,i],
                               x = df5[,1],
                               mode = "lines", 
                               line = list(color = l3[[i-1]]),
                               name = l2[[i-1]],
                               hoverinfo = "text+name", 
                               text = paste(format(df5$Date, "%b %d"),":", comma(df5[,i])))
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
                               text = paste(format(dfact2$Date, "%b %d"),":", comma(dfact2[,2]))
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
                             text = paste(format(dfact$Date, "%b %d"),":", comma(dfact[,2]))
    )
    for (i in 2:ncol(df6)){
      fig <- fig %>% add_trace(y = df6[,i],
                               x = df6[,1],
                               line = list(color = l3[[i-1]]),
                               name = l2[[i-1]],
                               fill = 'tozeroy',
                               fillcolor = list(color = l3[[i-1]]),
                               hoverinfo = "text+name", 
                               text = paste(format(df6$Date, "%b %d"),":", comma(df6[,i])))
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
                               text = paste(format(dfact$Date, "%b %d"),":", comma(dfact[,2]))
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
                               text = paste(format(mn$Date, "%b %d"),":", comma(mn[,i]), "(",comma(pmn[,i]),"%",")"))
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
                                text = paste(format(dif$Date, "%b %d"),":", comma(dif[,i]), "(",comma(pdif[,i]),"%",")"))
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
                                text = paste(format(mn$Date, "%b %d"),":", comma(mn[,i]), "(",comma(pmn[,i]),"%",")"))
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
                                text = paste(format(dif$Date, "%b %d"),":", comma(dif[,i]), "(",comma(pdif[,i]),"%",")"))
    }
    fig <- fig %>% layout(showlegend = TRUE,
                          hovermode = "compare") %>%
                                                  config(displayModeBar = TRUE)
  })
 
  # Reactive expressions to set name of day or month
  
  ww <- reactive({
    if(input$RecurrencyID == 1){
      #df <- list("January", "February", "March", "April","May", "June", "July", "August", "September", "October", "November", "December")
      df <- list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    } else {
      df <- list("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    }
    df
  })
  
  www <- reactive({
    if(input$RecurrencyID == 1){
      #df <- list(1,2,3,4,5,6,7,8,9,10,11,12)
      df <- list(1,2,3,4,5,6,7)
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
                                text = paste(wnames,":", comma(wka[,2])))
      for (i in 2:ncol(wk)){
        fig <- fig %>% add_trace (wk,
                                  y = wk[,i], 
                                  x = wk[,1], 
                                  marker = list(color = g2[[i-1]]),
                                  type = "bar", 
                                  showlegend = FALSE, 
                                  name = g1[[i-1]],
                                  hoverinfo = "text+name", 
                                  text = paste(wnames,":", comma(wk[,i])))
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
                                text = paste(wnames,":", comma(wka[,2])))
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
   v
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
    
  today <- reactive ({
    dfact1 <- data.frame(Actual = dflma()$Actual)
    fechasr <- data.frame(Date = fecha()[nrow(dfact1),])
    df <- fechasr
    df
  })
  
 output$actualBox <- renderValueBox({
    infoBox(
      tags$p(paste0(format(today(), "%d-%B-%Y"), " ", ": Actual"), style = "font-size: 90%;"),
      tags$p(comma(vb()[,1]), style = "font-size: 25px;"), 
      icon = icon("users"),
      color = "purple",
      fill = TRUE
    )
  })
  
  output$yoyBox <- renderValueBox({
     infoBox(
      tags$p(paste0(format(today(), "%d-%B-%Y"), " ", ": YoY"), style = "font-size: 90%;"),
      tags$p(paste(comma(vb()[,3])," ","|"," ",round(((vb()[,1]/vb()[,3])-1)*100,0),"%"), style = "font-size: 25px;"),
      icon = icon("calendar-alt", lib = "font-awesome"),
      color = estado_1(),
      fill = TRUE
    )
  })
  
  output$budgetBox <- renderValueBox({
    infoBox(
      tags$p(paste0(format(today(), "%d-%B-%Y"), " ", ": Budget"), style = "font-size: 90%;"),
      tags$p(paste(comma(vb()[,2])," ","|"," ",round(((vb()[,1]/vb()[,2])-1)*100,0),"%"), style = "font-size: 25px;"),
      icon = icon("wallet", lib = "font-awesome"),
      color = estado_2(),
      fill = TRUE
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
    infoBox(
      tags$p("Selected Period: Actual", style = "font-size: 90%;"),
      tags$p(comma(vbCum()[,1]), style = "font-size: 25px;"),
      icon = icon("users", lib = "font-awesome"),
      color = "purple",
      fill = TRUE
    )
  })
  
  output$yoyBox2 <- renderValueBox({
    infoBox(
      tags$p("Selected Period: YoY", style = "font-size: 90%;"),
      tags$p(paste(comma(vbCum()[,3])," ","|"," ",round(((vbCum()[,1]/vbCum()[,3])-1)*100,0),"%"), style = "font-size: 25px;"),
      icon = icon("calendar-alt", lib = "font-awesome"),
      color = estado_3(),
      fill = TRUE
    )
  })
  
  output$budgetBox2 <- renderValueBox({
    infoBox(
      tags$p("Selected Period: Budget", style = "font-size: 90%;"),
      tags$p(paste(comma(vbCum()[,2])," ","|"," ",round(((vbCum()[,1]/vbCum()[,2])-1)*100,0),"%"), style = "font-size: 25px;"),
      icon = icon("wallet", lib = "font-awesome"),
      color = estado_4(),
      fill = TRUE
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
    infoBox(
      tags$p("Selected Period: Delta", style = "font-size: 90%;"),
      tags$p(paste(comma(fitvalues()[2,1])," ","vs"," ",comma(fitvalues()[2,5])), style = "font-size: 25px;"),
      icon = icon("exchange-alt", lib = "font-awesome"),
      color = "purple",
      fill = TRUE
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
  
############################### Heatmap Server ###################################
  
  # Reactive expressions for direction radio button
  
   FilterDirection_hm <- reactive({
    if (input$DirectionID_hm == 1) {
      df <- df_heat[,c("Date", 
                         "Time", 
                         "Total", 
                         "Day", 
                         "Month", 
                         "Year")]
      colnames(df)[3] <- "Passengers"
    } else {
      if (input$DirectionID_hm == 2) {
        df <- df_heat[,c("Date", 
                           "Time", 
                           "NB", 
                           "Day", 
                           "Month", 
                           "Year")]
        colnames(df)[3] <- "Passengers"
      } else {
        df <- df_heat[,c("Date", 
                           "Time", 
                           "SB", 
                           "Day", 
                           "Month", 
                           "Year")]
        colnames(df)[3] <- "Passengers"
      }
    }
    df
  })
  
  # Reactive expressions for base year drill down
  
  FilterBYear_hm <- reactive({
    df <- filter(FilterDirection_hm(), 
                  Year == input$PeriodID_hm)
    df
  })
  
  # Reactive expressions for month drill down
  
  FilterMonth_hm <- reactive({
      df <- filter(FilterBYear_hm(),
                   Month == input$MonthsID_hm)
      df 
  })
  
  output$filter <- renderTable({
    FilterMonth_hm() 
  })
 
  # Reactive expressions to create heatmap dfs to display  
   
  Heatmap_df_na <- reactive({
    df <- FilterMonth_hm()
    df$Passengers[df$Passengers == 0] <- NA
    df
  })
  
  # Heatmap Plots
  
    # Monthly
  
    output$hm_monthly <- renderPlot({
      df_na <- data.frame(Heatmap_df_na())
      df <- data.frame(FilterMonth_hm())
      ggplot(df_na,aes(Date, Time, fill = Passengers))+
            geom_tile(color= "white", size=.1) + 
            scale_fill_gradientn(colours = c("Red", "orange", "lightgreen", "green"),
                         breaks=c(0,round(max(df$Passengers)/4),(round(max(df$Passengers)/4))*2,(round(max(df$Passengers)/4))*3, max(df$Passengers)),
                         limits = (range(df$Passengers)),
                         na.value = "#FFFFFF") +
            scale_y_continuous( trans = "reverse",breaks = unique(df_na$Time), expand = c(0,0), labels = list("00:00", "01:00", "02:00","03:00",
                                                                                                              "04:00","05:00","06:00","07:00",
                                                                                                              "08:00","09:00","10:00","11:00",
                                                                                                              "12:00","13:00","14:00","15:00",
                                                                                                              "16:00","17:00","18:00","19:00",
                                                                                                              "20:00","21:00","22:00","23:00")) +
            scale_x_date(date_labels =  "%b %d", breaks = unique(df_na$Date), expand = c(0,0), position = "top") +
            theme_minimal(base_size = 10) +
            #theme(legend.position = "bottom")+
            theme(axis.text.y = element_text(size = 12)) +
            theme(strip.background = element_rect(colour="white"))+
            theme(axis.ticks = element_blank())+
            theme(axis.text.x = element_text(size = 12))+
            #theme(legend.title=element_blank())+
            #theme(legend.text=element_text(size = 10)) +
            theme(legend.position = "none") +
            theme(axis.title.x = element_blank()) +
            theme(axis.title.y = element_blank()) +
            #geom_tile(color = "#FFFFFF") +
            geom_text(aes(label = Passengers), size = 4, color = "black") +
            removeGrid()
       })
    
  # Reactive expressions for weekday heatmap
    
    wd_hm <- reactive({
      df <- FilterMonth_hm()[,2:4]
      df <- df %>% 
                group_by(Day,Time) %>%
                                    summarise_all(sum)
      df
    })
    
    wd_hm_na <- reactive({
      df <- wd_hm()
      df$Passengers[df$Passengers == 0] <- NA
      df
    })
    
    # Weekday
    
    output$hm_wd <- renderPlot({
      df_na <- data.frame(wd_hm_na())
      df <- data.frame(wd_hm())
      ggplot(df_na,aes(Day, Time, fill = Passengers))+
        geom_tile(color= "white", size=.1) + 
        scale_fill_gradientn(colours = c("Red", "orange", "lightgreen", "green"),
                             breaks=c(0,round(max(df$Passengers)/4),(round(max(df$Passengers)/4))*2,(round(max(df$Passengers)/4))*3, max(df$Passengers)),
                             limits = (range(df$Passengers)),
                             na.value = "#FFFFFF") +
        scale_y_continuous(trans = "reverse", breaks = unique(df_na$Time), expand = c(0,0), labels = list("00:00", "01:00", "02:00","03:00",
                                                                                                          "04:00","05:00","06:00","07:00",
                                                                                                          "08:00","09:00","10:00","11:00",
                                                                                                          "12:00","13:00","14:00","15:00",
                                                                                                          "16:00","17:00","18:00","19:00",
                                                                                                          "20:00","21:00","22:00","23:00")) +
        scale_x_continuous(breaks = unique(df_na$Day), expand = c(0,0), position = "top", labels = list("Monday", "Tuesday", "Wednesday", "Thursday",
                                                                                                        "Friday", "Saturday", "Sunday")) +
        theme_minimal(base_size = 10) +
        #theme(legend.position = "bottom")+
        theme(axis.text.y = element_text(size = 12)) +
        theme(strip.background = element_rect(colour="white"))+
        theme(axis.ticks = element_blank())+
        theme(axis.text.x = element_text(size = 12))+
        #theme(legend.title=element_blank())+
        #theme(legend.text=element_text(size = 10)) +
        theme(legend.position = "none") +
        theme(axis.title.x = element_blank()) +
        theme(axis.title.y = element_blank()) +
        geom_tile(color = "#FFFFFF") +
        geom_text(aes(label = Passengers), size = 4, color = "black") +
        removeGrid()
    })
    
  # Reactive expressions to calculate mean and total per hour (Daily and Weekday)
    
    cum_daily_hm <- reactive({
      df <-  data.frame(FilterMonth_hm())
      df <-  df[,2:3] %>% 
                       group_by(Time) %>%
                                       summarise_all(sum)
      df
    })
    
    avg_daily_hm <- reactive({
      df <-  data.frame(FilterMonth_hm())
      df <-  df[,2:3] %>% 
                       group_by(Time) %>%
                                       summarise_all(mean)
      df$Passengers <- round(df$Passengers,0)
      df
    })
    
    caprate_hm <- reactive({
      df <-  data.frame(cum_daily_hm())
      df <-  df %>% 
                 transmute(Time = Time, CR = (Passengers/sum(Passengers))*100)
      df$CR <- round(df$CR,1)
      df
    })
    
    caprate_wd_hm <- reactive({
      df <- data.frame(FilterMonth_hm())
      df <- df[,c("Day", "Passengers")] %>% 
                                         group_by(Day) %>%
                                                         summarise_all(sum) %>%
                                                                             transmute(Day = Day, CR = (Passengers/sum(Passengers))*100)
      df$CR <- round(df$CR,0)
      df
    })
    
    
    
  # Per hour plots (Cumulative Mean and Capture Rate)  
    
    output$hourPlot <- renderPlotly({
      avg <- data.frame(avg_daily_hm())
      cum <- data.frame(cum_daily_hm())
      cr <- data.frame(caprate_hm())
      hr <- c("00:00", "01:00", "02:00","03:00",
              "04:00","05:00","06:00","07:00",
              "08:00","09:00","10:00","11:00",
              "12:00","13:00","14:00","15:00",
              "16:00","17:00","18:00","19:00",
              "20:00","21:00","22:00","23:00")
      hrv <- c(0,1,2,3,4,
                  5,6,7,8,
                  9,10,11,12,
                  13,14,15,16,
                  17,18,19,20,
                  21,22,23)
      fig <- plot_ly()
      fig <- fig %>% add_trace(avg,
                              y = avg[,2], 
                              x = avg[,1], 
                              type = "scatter",
                              mode = "markers",
                              marker = list(color = "#FF3094"),
                              name = "Average",
                              hoverinfo = "text+name", 
                              text = paste(hr,":", comma(avg[,2])))
      fig <- fig %>% add_trace (cum,
                                y = cum[,2], 
                                x = cum[,1], 
                                marker = list(color = "#D8D8D8"),
                                type = "bar", 
                                name = "Cumulative",
                                hoverinfo = "text+name", 
                                text = paste(hr,":", comma(cum[,2])))
      fig <- fig %>% layout(showlegend = TRUE, hovermode = "compare") %>%
                                                                         config(displayModeBar = TRUE)
      fig <- fig %>% layout(xaxis = list(ticktext =  hr, tickvals = hrv ))
    })
    
    
    output$crPlot <- renderPlotly({
      cr <- data.frame(caprate_hm())
      hr <- c("00:00", "01:00", "02:00","03:00",
              "04:00","05:00","06:00","07:00",
              "08:00","09:00","10:00","11:00",
              "12:00","13:00","14:00","15:00",
              "16:00","17:00","18:00","19:00",
              "20:00","21:00","22:00","23:00")
      hrv <- c(0,1,2,3,4,
               5,6,7,8,
               9,10,11,12,
               13,14,15,16,
               17,18,19,20,
               21,22,23)
      fig <- plot_ly()
      fig <- fig %>% add_trace (cr,
                                y = cr[,2], 
                                x = cr[,1], 
                                marker = list(color = "#3F2A55"),
                                type = "bar", 
                                name = "Capture Rate",
                                hoverinfo = "text+name", 
                                text = paste(hr,":", comma(cr[,2]), "%"))
      fig <- fig %>% 
                  layout(showlegend = FALSE, hovermode = "compare") %>%
                                                                     config(displayModeBar = TRUE)
      #fig <- fig %>% layout(xaxis= list(tickvals= cr$Time,ticktext= cr$Time))
      fig <- fig %>% layout(xaxis = list(ticktext =  hr, tickvals = hrv), yaxis = list(title= "%"))
    })
   
  # Reactive expressions to calculate mean and capture rate per day (Weekday)
    
    pd_avg <- reactive({
      df <- data.frame(FilterMonth_hm())
      df <- df[,c("Date", "Passengers")] %>% 
                                          group_by(Date) %>%
                                                          summarise_all(sum) %>%
                                                                              transmute(Date = Date, Avg = Passengers/24)
      df$Avg <- round(df$Avg,0)
      df
    })
    
    wd_avg <- reactive({
      df <- data.frame(FilterMonth_hm())
      df <- df[,c("Day", "Passengers")] %>% 
                                          group_by(Day) %>%
                                                          summarise_all(sum) %>%
                                                                              transmute(Day = Day, Avg = Passengers/24)
      df$Avg <- round(df$Avg,0)
      df
    })
    
     # Per day plot (Mean)
    
    output$avg_pdPlot <- renderPlotly({
      avg <- data.frame(pd_avg())
      fig <- plot_ly()
      fig <- fig %>% add_trace (avg,
                                y = avg[,2], 
                                x = avg[,1], 
                                marker = list(color = "#FF3094"),
                                type = "bar", 
                                name = "Average",
                                hoverinfo = "text+name", 
                                text = paste(format(avg$Date, "%b %d"),":", comma(avg[,2])))
      fig <- fig %>%
                  layout(showlegend = FALSE, hovermode = "compare") %>%
                                                                     config(displayModeBar = TRUE)
    })
    
    
    # Per weekday plot (CR and Mean)
    
    output$cr_wdPlot <- renderPlotly({
      cr <- data.frame(caprate_wd_hm())
      day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      dayv <- c(1,2,3,4,5,6,7) 
      fig <- plot_ly()
      fig <- fig %>% add_trace (cr,
                                y = cr[,2], 
                                x = cr[,1], 
                                marker = list(color = "#3F2A55"),
                                type = "bar", 
                                name = "Capture Rate",
                                hoverinfo = "text+name", 
                                text = paste(day,":", comma(cr[,2]), "%"))
      fig <- fig %>% 
                  layout(showlegend = FALSE, hovermode = "compare") %>%
                                                                     config(displayModeBar = TRUE)
      fig <- fig %>% layout(xaxis = list(ticktext = day , tickvals = dayv), yaxis = list(title= "%"))
    })
    
    output$avg_wdPlot <- renderPlotly({
      avg <- data.frame(wd_avg())
      day <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
      dayv <- c(1,2,3,4,5,6,7)
      fig <- plot_ly()
      fig <- fig %>% add_trace (avg,
                                y = avg[,2], 
                                x = avg[,1], 
                                marker = list(color = "#FF3094"),
                                type = "bar", 
                                name = "Average",
                                hoverinfo = "text+name", 
                                text = paste(day,":", comma(avg[,2])))
      fig <- fig %>% layout(showlegend = FALSE, hovermode = "compare") %>%
                                                                     config(displayModeBar = TRUE)
      fig <- fig %>% layout(xaxis = list(ticktext =  day, tickvals = dayv))
    })
    
# Reactive expression to create by weekday number plot
    
   wd_hm_number <- reactive({
     df <- data.frame(FilterMonth_hm())
     df <- df %>% select(Day, Date, Passengers) %>%
                                                  group_by(Day, Date) %>%
                                                                        summarise_all(sum)
     df
   })
    
    # By weekday number plot
   
   output$wd_nrPlot <- renderPlotly({
     df <- data.frame(wd_hm_number())
     Monday <- df %>% filter(Day == 1) %>% transmute(Date = Date, Passengers = Passengers)
     Tuesday <- df %>% filter(Day == 2) %>% transmute(Date = Date, Passengers = Passengers)
     Wednesday <- df %>% filter(Day == 3) %>% transmute(Date = Date, Passengers = Passengers)
     Thursday <- df %>% filter(Day == 4) %>% transmute(Date = Date, Passengers = Passengers)
     Friday <- df %>% filter(Day == 5) %>% transmute(Date = Date, Passengers = Passengers)
     Saturday <- df %>% filter(Day == 6) %>% transmute(Date = Date, Passengers = Passengers)
     Sunday <- df %>% filter(Day == 7) %>% transmute(Date = Date, Passengers = Passengers)
     pMonday <- df %>% filter(Day == 1) %>% transmute(Date = Date, Passengers = round(Passengers/sum(Passengers)*100,0))
     pTuesday <- df %>% filter(Day == 2) %>% transmute(Date = Date, Passengers = round(Passengers/sum(Passengers)*100,0))
     pWednesday <- df %>% filter(Day == 3) %>% transmute(Date = Date, Passengers = round(Passengers/sum(Passengers)*100,0))
     pThursday <- df %>% filter(Day == 4) %>% transmute(Date = Date, Passengers = round(Passengers/sum(Passengers)*100,0))
     pFriday <- df %>% filter(Day == 5) %>% transmute(Date = Date, Passengers = round(Passengers/sum(Passengers)*100,0))
     pSaturday <- df %>% filter(Day == 6) %>% transmute(Date = Date, Passengers = round(Passengers/sum(Passengers)*100,0))
     pSunday <- df %>% filter(Day == 7) %>% transmute(Date = Date, Passengers = round(Passengers/sum(Passengers)*100,0))
     colores <- c("#D8D8D8", "#FF3094", "#726C79", "#00C7C5", "#3F2A55")
     fig1 <- plot_ly(Monday,
                     x = Monday$Date,
                     y = Monday$Passengers,
                     marker = list(color = colores),
                     type = "bar",
                     name = "Monday",
                     hoverinfo = "text+name", 
                     text = paste(format(Monday$Date, "%b %d"),":", Monday$Passengers,"(", pMonday$Passengers, "%", ")"))
     fig2 <- plot_ly(Tuesday,
                     x = Tuesday$Date,
                     y =Tuesday$Passengers,
                     marker = list(color = colores),
                     type = "bar",
                     name = "Tuesday",
                     hoverinfo = "text+name", 
                     text = paste(format(Tuesday$Date, "%b %d"),":", Tuesday$Passengers, "(", pTuesday$Passengers, "%", ")"))
     fig3 <- plot_ly(Wednesday,
                     x = Wednesday$Date,
                     y = Wednesday$Passengers,
                     marker = list(color = colores),
                     type = "bar",
                     name = "Wednesday",
                     hoverinfo = "text+name", 
                     text = paste(format(Wednesday$Date, "%b %d"),":", Wednesday$Passengers, "(", pWednesday$Passengers, "%", ")"))
     fig4 <- plot_ly(Thursday,
                     x = Thursday$Date,
                     y =Thursday$Passengers, 
                     marker = list(color = colores),
                     type = "bar",
                     name = "Thursday",
                     hoverinfo = "text+name", 
                     text = paste(format(Thursday$Date, "%b %d"),":", Thursday$Passengers, "(", pThursday$Passengers, "%", ")"))
     fig5 <- plot_ly(Friday,
                     x = Friday$Date,
                     y = Friday$Passengers,
                     marker = list(color = colores),
                     type = "bar",
                     name = "Friday",
                     hoverinfo = "text+name", 
                     text = paste(format(Friday$Date, "%b %d"),":", Friday$Passengers, "(", pFriday$Passengers, "%", ")"))
     fig6 <- plot_ly(Saturday,
                     x = Saturday$Date,
                     y =Saturday$Passengers,
                     marker = list(color = colores),
                     type = "bar",
                     name = "Saturday",
                     hoverinfo = "text+name", 
                     text = paste(format(Saturday$Date, "%b %d"),":", Saturday$Passengers, "(", pSaturday$Passengers, "%", ")"))
     fig7 <- plot_ly(Sunday,
                     x = Sunday$Date,
                     y = Sunday$Passengers, 
                     marker = list(color = colores),
                     type = "bar",
                     name = "Sunday",
                     hoverinfo = "text+name", 
                     text = paste(format(Sunday$Date, "%b %d"),":", Sunday$Passengers, "(", pSunday$Passengers, "%", ")"))
     fig <- plot_ly()
     fig <- subplot(fig1, fig2, fig3, fig4, fig5, fig6, fig7, shareY = TRUE)
     fig <- fig %>% layout(xaxis = list(showticklabels = FALSE, title = "Monday"),
                           xaxis2 = list(showticklabels = FALSE, title = "Tuesday"),
                           xaxis3 = list(showticklabels = FALSE, title = "Wednesday"),
                           xaxis4 = list(showticklabels = FALSE, title = "Thursday"),
                           xaxis5 = list(showticklabels = FALSE, title = "Friday"),
                           xaxis6 = list(showticklabels = FALSE, title = "Saturday"),
                           xaxis7 = list(showticklabels = FALSE, title = "Sunday"),
                           showlegend = FALSE)
   })
    
})

