library(lubridate)
library(ggplot2)
library(nortest)
library(xts)
library(GAS)
library(rugarch)
library(reshape2)

kursy <- read.csv("kursy.csv", sep = ";", dec = ",")

kursy$data <- ymd(kursy$data)
kursy <- data.frame(kursy$data, kursy$X1THB, kursy$X1EUR, kursy$X1UAH)
colnames(kursy) <- c("data", "THB", "EUR", "UAH")

summary(kursy)

ggplot(kursy, aes(x = data, y = UAH)) + geom_line()


doVaR <- data.frame(date = kursy$data)
doVaR["data"] <- kursy$UAH
doVaR["st_zwrotu"] <- c()
doVaR["VAR"] <- c()
doVaR["waga"] <- c()
doVaR["ES"] <- c()
doVaR["predsd"] <- NA
q <- 0.99



hist <- function(dane){
  data <- data.frame(data = dane)
  st_zwrotu <- c()
  VAR <- c()
  ES <- c()
  for (i in 2:nrow(data)){
    st_zwrotu[i] <- log(data[i,]/data[i-1,])*100 
  }
  st_zwrotu <- st_zwrotu[-1]
  data <- data[-1,1]
  for(j in 1:(length(data) - 499)){
    es <- st_zwrotu[j:(499+j)]
    VAR[499+j] <- -quantile(st_zwrotu[j:(j+499)],0.01, na.rm = T)
    ES[499+j] <- - mean(es[st_zwrotu[j:(499+j)] <= -VAR[499+j]])
  }
  doVaR <- data.frame(date = kursy$data[-1], data = data, st_zwrotu, VAR,ES)
}

#historical method
for (i in 2:nrow(doVaR)){
  doVaR$st_zwrotu[i] <- log(doVaR$data[i]/doVaR$data[i-1])*100 
}
doVaR <- doVaR[-1,]
waga <- c()
for(i in 1:500){
  waga[i] <- q^(500-i) * (1-q) / (1-q^500)
}

for(j in 1:(nrow(doVaR) - 499)){
  es <- doVaR[j:(499+j),]
  doVaR$VAR[499+j] <- -quantile(doVaR$st_zwrotu[j:(j+499)],0.01, na.rm = T)
  doVaR$ES[499+j] <- - mean(es[doVaR$st_zwrotu[j:(499+j)] <= -doVaR$VAR[499+j],3])
}



doVaR <- data.frame(date = kursy$data[-1])
hist_wag <- function(dane,q){
  data <- data.frame(data = dane)
  st_zwrotu <- c()
  VAR <- c()
  ES <- c()
  waga <- c()
  for(i in 1:500){
    waga[i] <- q^(500-i) * (1-q) / (1-q^500)
  }
  for (i in 2:nrow(data)){
    st_zwrotu[i] <- log(data[i,]/data[i-1,])*100 
  }
  st_zwrotu <- st_zwrotu[-1]
  data <- data[-1,1]
  for(j in 1:(length(data) - 499)){
    zwag <- data.frame(st_zwrotu[j:(499+j)],waga)
    zwag <- zwag[order(zwag[,1]),]
    zwag <- cbind(zwag, skumulowane = cumsum(zwag$waga))
    VAR[499+j] <- -zwag[nrow(zwag[zwag$skumulowane < 0.01,])+ 1,1]
    ES[499+j] <- - mean(zwag[ zwag$st_zwrotu<= -VAR[499+j],1])
  }
  
  doVaR <- cbind(doVaR, data, st_zwrotu, VAR,ES)
  melt <- melt(doVaR, id.vars = c("date", "data"), na.rm = T)
  ggplot(melt, aes(x = date, y = value, colour = variable)) + geom_line()
}


#weights
for(j in 1:(nrow(data) - 499)){
  zwag <- data.frame(date = kursy$data, st_zwrotu[j:(499+j)],waga)
  zwag <- zwag[order(zwag[,2]),]
  zwag <- cbind(zwag, skumulowane = cumsum(zwag$waga))
  VAR[499+j] <- -zwag[nrow(zwag[zwag$skumulowane < 0.01,])+ 1,2]
  ES[499+j] <- - mean(zwag[ zwag$st_zwrotu<= -VAR[499+j],2])
}

  melt <- melt(doVaR, id.vars = c("date", "data","st_zwrotu"), na.rm = T)
  ggplot(melt, aes(x = date, y = value, colour = variable)) + geom_line()
 
  #bootstrap
  probka <- NA
  vary <- data.frame(probka)
  for(j in 1:(nrow(doVaR)-499)){
    vary <- NA
    es <- NA
  for(i in 1:20){
    rand <- sample(doVaR$st_zwrotu[j:(j+499)], 1000, replace = T)
    vary <- rbind(vary,-quantile(rand,0.01, na.rm = T))
    es <- rbind(es,-mean(rand[rand <= quantile(rand,0.01, na.rm = T)]))
  }  
    doVaR$VAR[j+499] <- mean(vary[-1,])
    doVaR$ES[499+j] <- mean(es[-1,])
    
  }
  
  doVaR_b <- data.frame(date = kursy$data[-1])
  boot <- function(dane){
    data <- data.frame(data = dane)
    st_zwrotu <- c()
    VAR <- c()
    ES <- c()
    for (i in 2:nrow(data)){
      st_zwrotu[i] <- log(data[i,]/data[i-1,])*100 
    }
    st_zwrotu <- st_zwrotu[-1]
    data <- data[-1,1]
    for(j in 1:(length(data)-499)){
      vary <- NA
      es <- NA
      for(i in 1:20){
        rand <- sample(st_zwrotu[j:(j+499)], 1000, replace = T)
        vary <- rbind(vary,-quantile(rand,0.01, na.rm = T))
        es <- rbind(es,-mean(rand[rand <= quantile(rand,0.01, na.rm = T)]))
      }  
      VAR[j+499] <- mean(vary[-1,])
      ES[499+j] <- mean(es[-1,])
      
    }
    
    doVaR_b <- cbind(doVaR_b, data, st_zwrotu, VAR,ES)
    melt <- melt(doVaR_b, id.vars = c("date", "data"), na.rm = T)
    ggplot(melt, aes(x = date, y = value, colour = variable)) + geom_line()
  }
  melt <- melt(doVaR, id.vars = c("date", "data"), na.rm = T)
  ggplot(melt, aes(x = date, y = value, colour = variable)) + geom_line()
  
 #GARCH
  m <- mean(doVaR$st_zwrotu)
  e <- doVaR$st_zwrotu - m
  omega <- weighted.mean(var(doVaR$st_zwrotu))
  alpha <- 0.1
  e2 <- e^2
  beta <- 0.8
  par(mfrow = c(2,1),mar = c(3, 2, 2, 2))
  plot(abs(e))
  
  # Plot the acf of the absolute prediction errors
  acf(abs(e))
  #predicted variance
  predvar <- c()
  predvar[1] <- var(doVaR$st_zwrotu)
  for(t in 2:nrow(doVaR)){
    predvar[t] <- omega + alpha * e2[t-1] + beta * predvar[t-1]
  }
  presd <- sqrt(predvar)
  
  for(  i in 1:length(predsd)){
    doVaR$VAR
  }
  
  
wyjatki <- 0
#back-tests
for (i in 501:nrow(doVaR)){
  if(doVaR$st_zwrotu[i] > doVaR$VAR[i])
  {
    wyjatki = wyjatki +1
  }
  proc <- wyjatki/1263 *100
}

doVaR_EWMA <- data.frame(date = kursy$data[-(1:501)])
EWMA <- function(dane, sigma1){
  lambda <- 0.94
  data <- data.frame(data = dane)
  st_zwrotu <- c()
  sigma <- c(sigma1)
  wartosc <- c()
  VAR <- c()
  ES <- c()
  for (i in 2:nrow(data)){
    st_zwrotu[i] <- log(data[i,]/data[i-1,])*100 
  }
  st_zwrotu <- st_zwrotu[-1]
  data <- data[-1,1]
  for(i in 1:length(st_zwrotu)){
    sigma[i+1] = sqrt(lambda *sigma[i] ^2 + (1-lambda) * st_zwrotu[i]^2)
  }
  for(i in 1:length(st_zwrotu)){
    wartosc[i] <- st_zwrotu[i] * (sigma[length(sigma)]/sigma[i])
  }
  for(j in 1:(length(data) - 499)){
    es <- wartosc[j:(499+j)]
    VAR[499+j] <- -quantile(wartosc[j:(j+499)],0.01, na.rm = T)
    ES[499+j] <- - mean(es[wartosc[j:(499+j)] <= -VAR[499+j]])
  }
  wartosc <- wartosc[-(1:500)]
  VAR <- VAR[-(1:500)]
  ES <- ES[-(1:500)]
  data <- data[-(1:500)]
  doVaR_EWMA <- cbind(doVaR_EWMA, data, wartosc, VAR,ES)
  melt <- melt(doVaR_EWMA, id.vars = c("date", "data"), na.rm = T)
  z <- ggplot(melt, aes(x = date, y = value, colour = variable)) + geom_line()
  plot(z)
  return(doVaR_EWMA$VAR)
}
 EUR_EWMA <- EWMA(kursy$EUR, 0.94)
 