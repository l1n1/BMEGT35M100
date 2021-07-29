rm(list = ls())

library(tidyr)
library(dplyr) 
library(quantmod)
source("FitARMAGarch.R")
source("RegressResiduals.R")
source("TestUnitroot.R")
source("SaveHistACF.R")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))


# https://www.mnb.hu/monetaris-politika/a-monetaris-politikai-eszkoztar/aukciok-tenderek-gyorstenderek/forint-tenderek-gyorstenderek


# ==============
# DATA

# Set data
tickers <- c("MS","SPY")
data = data.frame()
for(ticker in tickers){
  temp <- getSymbols(ticker, from = Sys.Date()-365,
                     to = Sys.Date(), env = NULL, return.class="data.frame", auto.assign=TRUE)
  temp <- temp[colnames(temp)[6]]
  if(length(colnames(data)) == 0){
    data = temp
  }else{
    data <- merge(data, temp, all = TRUE, by=0)
  }
}
rownames(data) <- data[,1]
data <- data[,-c(1)]

# Read test data

# Fit ARIMA-GARCH(1,1)
models <- fitARMAGarch(data, 1)

# Prepare TS residuals
Residuals <- as.data.frame(models$`GARCH residuals`)
colnames(Residuals) <- tickers
row.names(Residuals) <- Residuals$Row.names
Residuals$Row.names <- NULL
# Overwrite not daily
for(c in tickers){Residuals[is.na(Residuals[c]), c] <- 0}
# Save Residuals
write.csv(Residuals, file = paste("Residuals", paste(paste(tickers, collapse = '_'), "csv", sep = "."), sep = "/"))


# ==============
# ANALAYZE FIT

# Stationarity tests
ur <- TestUnitroot(Residuals)
write.csv(ur, file = paste("UnitRoot", paste(paste(tickers, collapse = '_'), "csv", sep = "."), sep = "/"))

# PLot Hist, P/ACF
SaveHist(Residuals, paste("Hist", "", sep="/"))
SaveACF(Residuals, paste("ACF", "", sep="/"))


# ==============
# OLS

# Initialize OLS
dependent <- tickers[1]
independent <- tickers[-c(1)] 

# Run OLS
ols <- lmResiduals(Residuals, dependent, independent)

# Save summary
sink(paste("OLS", paste(paste(tickers, collapse = '_'), "txt", sep = "."), sep = "/"))
print(summary(ols))
sink()