data <- read.csv("~/Downloads/avg_rainfalltemp in UK - Sheet1 2.csv")
data2 <- data.frame(data$Avg.rainfall.in.mm., data$Avg.temp.in.centigrade.)

summary(data2)

#library(psych)
#describe(data2)

#library(Hmisc)
#describe(data2)

library(pastecs)
stat.desc(data2)

# PLOT
plot(data$Avg.rainfall.in.mm.,type="o",main="Grafik Curah Hujan Rata-Rata",
     xlab="Bulan Ke-", ylab="Curah Hujan (mm)")

plot(data$Avg.temp.in.centigrade.,type="o",main="Grafik Suhu Rata-Rata",
     xlab="Bulan Ke-", ylab="Suhu (celsius)")

boxplot(data$Avg.rainfall.in.mm., horizontal=T, 
        main="Curah Hujan Rata-Rata")

boxplot(data$Avg.temp.in.centigrade., horizontal=T, 
        main="Suhu Rata-Rata")

d1 <- density(data$Avg.rainfall.in.mm.)
plot(d1, main="Kepadatan Data Curah Hujan")
polygon(d1, col="red")

d2 <- density(data$Avg.temp.in.centigrade.)
plot(d2, main="Kepadatan Data Suhu Rata-Rata")
polygon(d2, col="blue")
