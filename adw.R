#ANALISIS DERET WAKTU

#mengaktifkan dataset
library(tseries)
library(forecast)

#masukan dataset
Data = read.csv("~/Documents/avg_rainfalltemp in UK - Sheet1 2.csv")

#mengubah dataframe menjadi "time series dataframe"
tsData = ts(as.numeric(Data$Avg.rainfall.in.mm.), start = c(1, 1), end = c(70, 1), frequency = 1)
tsData

#plot data
ts.plot(tsData, ylab = "Curah Hujan (mm)", xlab = "Bulan ke-", main = "Grafik Curah Hujan terhadap Waktu")

#uji kestasioneran data
adf.test(tsData) #jika p-value < alpha mengindikasikan data stasioner

#jika data series belum stasioner lakukan differensiasi
tsDatadiff <- diff(tsData, differences = 1) #differensiasi hingga orde ke-n
ts.plot(tsDatadiff, ylab="diff=1", xlab="waktu")
adf.test(tsDatadiff)

#plot grafik ACF dan PACF dari data differensiasi
acf(tsDatadiff, main = "Grafik ACF")
pacf(tsDatadiff, main = "Grafik PACF")

#model deret waktu ARIMA
#model ARIMA(1,1,1) manual dengan p:AR, d:differential, q:MA
modelmanual1 <- arima(tsData, order = c(1, 1, 1))
print(modelmanual1)

#model ARIMA(2,1,1) manual
modelmanual2 <- arima(tsData, order = c(2, 1, 1))
print(modelmanual2)

#model ARIMA otomatis
modelauto <- auto.arima(tsData, seasonal = F)
print(modelauto)

#forecast
fcast = forecast(modelmanual1, h = 2, level = 0.95)
fcast
plot(fcast)
