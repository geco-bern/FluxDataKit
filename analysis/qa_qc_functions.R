Pre <- function(mydata, site){
  my_file <- mydata %>%
    select(starts_with("date"),
           gpp,
           temp,
           prec,
           ppfd,
           sitename) %>%
    filter(sitename == site )
  #replace -9999 with NAs
  my_file <- my_file %>%
    na_if(-9999)
  #have the daily data
  my_daily_file$date
  my_daily<- my_file %>%
    group_by(date) %>%
    summarise(gpp = mean(gpp, na.rm = TRUE),
              temp = mean(temp, na.rm = TRUE),
              prec = mean(prec, na.rm = TRUE),
              ppdf = mean(ppfd, na.rm = TRUE))
  return(my_daily)
}

add_outliers <- function(mydata){
  t_lm <- lm(gpp ~
               temp+
               prec+
               ppdf,
             data = mydata)
  res <- t_lm$residuals
  outres <- boxplot.stats(res)$out
  #some gpp is not exist, what should be done? i use the na.omit...
  n1_mydaily <- na.omit(mydata) %>%
    mutate(res = res) %>%
    mutate(outline = if_else(res %in% outres,"out", "in"))
  # > n1_mydaily <- my_daily %>% mutate(res = res) %>%
  #   +   mutate(outline = if_else(res %in% outres,"out", "in"))
  # Error: Problem with `mutate()` column `res`.
  # i `res = res`.
  # i `res` must be size 2555 or 1, not 2546.
  return(n1_mydaily)
  }

add_drift <- function(mydata){
  drift_dat <- mydata %>%
    mutate(year_dec = year(date)+ (lubridate::yday(date) - 1)/365)
  #create the daily data for using
  lm_d <- lm(gpp ~ temp +
               prec+
               ppdf+
               year_dec,
             data = drift_dat)
  s <- summary(lm_d)
  drift_dat$tfordrift <- s$coefficients["year_dec",3]#?add the t value
  return(drift_dat)
}

add_spurious <- function(mydata){
  c <- mydata%>% select(gpp)
  m <- c$gpp
  c1 <- c %>% table() %>% as.data.frame()
  #update: add the extra column here for the freq
  c_new <- sapply(c$gpp,function(x){m <- c1[c1$.== x, "Freq"]})
  b <- rep(NA,length(c_new))
  for (i in 1:length(c_new)){
    b[i] <- c_new[[i]]
  }
  mydata <- mydata %>% mutate(rep = b)
  return(mydata)
}

add_step_change <- function(mydata){
  library(strucchange)
  test <- mydata %>% mutate(month_year = month(date), year = year(date))
  test <- test %>% mutate(month_dec = year+(month_year-1)/12)
  test <- test %>% group_by(month_dec) %>% summarise(gpp = mean(gpp),
                                                     prec = mean(prec),
                                                     ppdf = mean(ppdf),
                                                     temp = mean(temp))
  #here have the  monthly data
  mlmt <- lm(gpp ~ prec+ppdf+temp, data = test)
  nrow(test)
  sctest(gpp ~ prec+ppdf+temp, data = test, type = "Chow", point =77)#why 5~77
  test <- test %>% mutate(tp = NA)
  for (i in 5:(nrow(test)-7)){#i have some problems with how many points can be done for the sctest
    tsc <- sctest(gpp ~ prec+ppdf+temp, data = test, type = "Chow",point = i)
    test$tp[i] <- tsc$p.value
  }
  test <- test %>% mutate(SCpoint = if_else(tp < 0.05,"SC","NON")) %>%
    mutate()
  a <- 1
  test$SCvalue <- NA
  for(j in 1:nrow(test)){
    if(is.na(test$SCpoint[j])){
      test$SCvalue[j] <- a
    } else if(test$SCpoint[j] == "SC") {
      a <- a+1
    } else {
      a <- a
    }

    test$SCvalue[j] <- a
  }
  return(test)
}

GrowingSeason <- function(mydata){
  get <-  quantile(mydata$gpp, probs = c(0.05,0.95),na.rm = TRUE)
  datanew <- dat1 %>% mutate(value = (dat1$gpp-get[1])/(get[2]-get[1]))
  datanew2 <-datanew %>%
    mutate(is_growing_season = if_else(value < 0.05,"F","T"))
  return(datanew2)
}
