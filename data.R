library(wbstats)
library(dplyr)
library(tibble)

get.data <- function(){
  china <- as.character(wbcountries() %>% filter(country == "China") %>% select(iso3c))
  metadata <- as.tibble(read.csv("definitions.csv"))
  names(metadata)[1:3] <- c("indicatorID","name","definition") 
  codes <- as.vector(metadata$indicatorID)
  ret <- as.tibble(wb(china,codes,1960,2018))
  
  series <- list()
  i <- 1
  for(code in codes){
    series[[i]] <- list()
    subret <- ret %>% filter(indicatorID == code)
    submeta <- metadata %>% filter(indicatorID == code)
    st <- min(as.numeric(subret$date))
    series[[i]][["series"]] <- ts(rev(subret$value), start = st, frequency = 1)
    series[[i]][["name"]] <- as.character(submeta$name)
    series[[i]][["description"]] <- as.character(submeta$definition)
    i = i + 1
  }
  
  series[[i]] <- list()
  series[[i]][["series"]] <- ts(read.csv("compensation.csv")[,2],start=1978, frequency=1)
  series[[i]][["name"]] <- "Compensation of Employees, %GDP"
  series[[i]][["description"]] <- "Shares of gross domestic income: Compensation of employees, paid: Wage and salary accruals: Disbursements: To persons, Percent, Annual, Not Seasonally Adjusted"
  
  i <- i + 1
  series[[i]] <- list()
  series[[i]][["series"]] <- ts(read.csv("stock.csv")[,2],start=1982)
  series[[i]][["name"]] <- "Capital Stock"
  series[[i]][["description"]] <- "Millions of 2011 U.S. Dollars, Not Seasonally Adjusted"
  
  names(series) <- c(codes,"COMP","STK")
  
  return(series)
}

data <- get.data()

calibrate <- function(except = "none"){
  pre <- list()
  post <- list()
  
  s.alpha <- get.series("alpha")$series
  s.lambda <- get.series("lambda")$series
  s.beta <- get.series("beta")$series
  s.tau <- get.series("tau")$series
  s.x <- get.series("x")$series
  s.g <- get.series("g")$series
  
  pre$alpha <- round(mean(window(s.alpha, end = 1981))/100,2)
  pre$lambda <- 1 - round(mean(window(s.lambda, end = 1981))/100,2) - pre$alpha 
  pre$beta <- round(mean(window(s.beta, end = 1981))/100,2)
  pre$tau <- round(mean(s.tau)/100,2)
  pre$x <- 0
  pre$g <- round(mean(window(s.g,start=1962, end=1981))/100,2)
  
  if(except != 'alpha'){
    post$alpha <- round(mean(window(s.alpha, start = 1982))/100,2)
  }
  
  if(except != 'lambda'){
    if(except != 'alpha'){
      post$lambda <- 1 - round(mean(window(s.lambda, start = 1982))/100,2) - post$alpha
    }
    else {
      post$lambda <- 1 - round(mean(window(s.lambda, start = 1982))/100,2)
    }
  } 
  
  if(except != 'beta'){
    post$beta <- round(mean(window(s.beta, start = 1982))/100,2)
  }
  
  if(except != 'tau'){
    post$tau <- pre$tau
  }
  
  if(except != 'x'){
    post$x <- round(mean(s.x)/100,2)
  }
  
  if(except != 'g'){
    post$g <- round(mean(window(s.g,start=1982))/100,2)
  }
  
  return(list(post = post, pre = pre))
}

get.series <- function(parameter){
  
  pop <- window(data[["SP.POP.TOTL"]]$series,start = 1982)
  
  if(parameter == "alpha"){
    return(data[["NE.GDI.FTOT.ZS"]])
  } else if(parameter == "beta") {
    return(data[["NY.GDS.TOTL.ZS"]])
  } else if(parameter == "x") {
    return(data[["BM.KLT.DINV.WD.GD.ZS"]])
  } else if(parameter == "lambda") {
    return(data[["COMP"]])
  } else if(parameter == "tau") {
    return(data[["GC.TAX.TOTL.GD.ZS"]])
  } else if(parameter == "g") {
    return(data[["NY.GDP.MKTP.KD.ZG"]])
  } else if(parameter == "stk") {
    return(data[["STK"]])
  } else if(parameter == "y") {
    return(data[["NY.GDP.MKTP.KD"]])
  } else if(parameter == "stkpc") {
    obj <- data[["STK"]]
    obj$series <- (data[["STK"]]$series)*(10^6) / pop
    return(obj)
  } else if(parameter == "ypc") {
    obj <- data[["NY.GDP.MKTP.KD"]]
    obj$series <- window(obj$series, start = 1982)/pop
    return(obj)
  } else if(parameter == "c") {
    return(data[["NE.CON.TOTL.KD"]])
  } else {
    return("Invalid Parameter")
  }
}

adj.series <- function(series,l0){
  fac <- series[1]/l0
  return(list(series = round(series/fac,2), factor = round(fac,2)))
}


stdz.dates <- function(list.series){
  
  max <- 1900
  min <- 3000
  
  for(series in list.series){
    
    s <- start(series)[1]
    e <- end(series)[1]

    if(s > max){
      max <- s
    }
    
    if(e < min){
      min <- e
    }
  }
  
  print(min)
  print(max)
  
  for(i in 1:length(list.series)){
    list.series[[i]] <- window(list.series[[i]], start = max, end = min)
  }
  
  return(list.series)
}
