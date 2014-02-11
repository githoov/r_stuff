#! /usr/bin/Rscript

require(PerformanceAnalytics)
require(zoo)
require(tseries)

TGEIX.prices = get.hist.quote(instrument="tgeix", start="2003-01-01",
                            end="2013-01-01", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
index(TGEIX.prices) = as.yearmon(index(TGEIX.prices))

LEXCX.prices = get.hist.quote(instrument="lexcx", start="2003-01-01",
                            end="2013-01-01", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
index(LEXCX.prices) = as.yearmon(index(LEXCX.prices))

FHEIX.prices = get.hist.quote(instrument="fheix", start="2003-01-01",
                            end="2013-01-01", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
index(FHEIX.prices) = as.yearmon(index(FHEIX.prices))

PRNHX.prices = get.hist.quote(instrument="prnhx", start="2003-01-01",
                            end="2013-01-01", quote="AdjClose",
                             provider="yahoo", origin="1970-01-01",
                             compression="m", retclass="zoo")
index(PRNHX.prices) = as.yearmon(index(PRNHX.prices))

Prices = merge(TGEIX.prices,LEXCX.prices,FHEIX.prices,PRNHX.prices)
colnames(Prices) = c("TGEIX", "LEXCX", "FHEIX", "PRNHX")
Returns = diff(log(Prices))

xTGEIX = mean(Returns$TGEIX)
xLEXCX = mean(Returns$LEXCX)
xFHEIX = mean(Returns$FHEIX)
xPRNHX = mean(Returns$PRNHX)

sdTGEIX = sd(Returns$TGEIX)
sdLEXCX = sd(Returns$LEXCX)
sdFHEIX = sd(Returns$FHEIX)
sdPRNHX = sd(Returns$PRNHX)

muVec = as.matrix(c(xTGEIX, xLEXCX, xFHEIX, xPRNHX))
covMat = cov(Returns)

topMat = cbind(2*covMat, muVec, rep(1,4))
midVec = c(muVec, 0, 0)
botVec = c(rep(1,4), 0, 0)
Ax.Mat = rbind(topMat, midVec, botVec)
retVec = c(rep(0, 4), muVec[4], 1)
z.Mat = solve(Ax.Mat)%*%retVec
x.Vec = z.Mat[1:4,]; x.Vec
mu.px = as.numeric(crossprod(x.Vec, muVec))
sig2.px = as.numeric(t(x.Vec)%*%covMat%*%x.Vec)