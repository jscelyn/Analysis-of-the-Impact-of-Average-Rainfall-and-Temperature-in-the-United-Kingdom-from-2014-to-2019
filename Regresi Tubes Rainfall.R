# Import Data 
library(readxl)
data <- read_excel("D:/ITB/Semester 3/Analisis Data/Praktikum/Tubes/avg_rainfalltemp-in-UK.xlsx")
View(data)

# REGRESI HUBUNGAN POPULASI DENGAN JUMLAH KASUS BARU COVID 19
# x/ prediktor = Avg rainfall(in mm)
# y/ respon = Avg temp(in centigrade)

# Input
avg_rainfall = as.numeric(data$"Avg rainfall(in mm)")
avg_temp= as.numeric(data$"Avg temp(in centigrade)")

# Visualisasi Peubah Acak
# 1) Diagram Pancar
scatter.smooth(x=avg_rainfall, y=avg_temp, main="avg_rainfall ~ avg_temp")

# 2) Diagram Kotak Titik
boxplot(avg_rainfall, main="Avg rainfall(in mm)", sub=paste("Outlier rows: ", boxplot.stats(avg_rainfall)$out)) 
boxplot(avg_temp, main="Avg temp(in centigrade)", sub=paste("Outlier rows: ", boxplot.stats(avg_temp)$out))

# 3) Diagram Densitas
library(e1071) 
par(mfrow=c(1, 2))
plot(density(avg_rainfall), main="Density Plot: Avg rainfall(in mm)", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(avg_rainfall), 2))) 
polygon(density(avg_rainfall), col="red") 

plot(density(avg_temp), main="Density Plot: Avg temp(in centigrade)", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(avg_temp), 2))) 
polygon(density(avg_temp), col="red") 

# Persamaan regresi linear sederhana
linearMod <- lm(avg_temp ~ avg_rainfall, data) 
linearMod

(modelSummary = summary(linearMod))
(modelCoeffs=modelSummary$coefficients)

# AIC dan BIC
(AIC(linearMod))
(BIC(linearMod))

# Korelasi
cov(avg_rainfall,avg_temp) 
cor(avg_rainfall,avg_temp) 


