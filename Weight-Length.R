library(readxl)
library(FSA)
library(FSAdata)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(ggpubr)
library(grDevices)
library(pwr)

setwd("D:/Data Sains/Panjang dan Berat Ikan")
data_wl <- read_excel("data-WL.xlsx")

rru <- subset(data_wl,!is.na(weight) & !is.na(length))
rru$logL <- log(rru$length)
rru$logW <- log(rru$weight)

lm <- lm(rru$logW ~ rru$logL)
summary(lm)
str(lm)

#coeff=coefficients(lm)
#r2 = rSquared(lm)

#eq = paste0("y = ", round(coeff[2],3), "*x ", round(coeff[1],3), " , R-squared = ", round(r2,3))

#fitPlot(lm,xlab="log Total Length (mm)",ylab="log Weight (g)",main=eq) 
#abline(lm,lwd=2, col="blue")

eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                 list(a = format(unname(coef(lm)[1]), digits = 4),
                      b = format(unname(coef(lm)[2]), digits = 4),
                      r2 = format(summary(lm)$r.squared, digits = 3)))
as.character(as.expression(eq))

plot <- ggplot(rru,aes(x = logL, y = logW)) + geom_point()+
        geom_smooth(method = "lm") + coord_cartesian(xlim=c(5.3, 6.20))+
        scale_x_continuous(breaks=seq(5.2, 6.20, 0.2)) +
        coord_cartesian(ylim=c(4.5, 7.5))+
        scale_y_continuous(breaks=seq(4.5,7.5, 0.5)) +
        xlab("log Total Length (mm)") + ylab("log Weight (g)") +
        annotate(geom="text", x = 5.4, y = 7.3, label= eq, hjust="left")
plot

#Koefisien Panjang dan Berat serta Pola Pertumbuhan Ikan
w <- exp(lm$coefficients[1]) *10
min(rru1$length)
lm$coefficients[2]

#############################################################################

#Weight Estimation
nrow(data_wl)
summary(data_wl)                  
sl_kelas <- 2

rru1 <- data.frame(
  length1 = seq(min(data_wl$length), max(data_wl$length), by = sl_kelas),
  log_length = log(rru1$length),
  w_est = exp(lm$coefficients[1]) * (rru1$length ^ lm$coefficients[2]), row.names = NULL
)

eq1 <- substitute(italic(W) == a ~ italic(L)^b, 
                 list(a = format(unname(w), digits = 4),
                      b = format(unname(coef(lm)[2]), digits = 4)))
as.character(as.expression(eq1))


plot1 <- ggplot(data=rru,aes(x = length, y = weight)) + geom_point(aes(color="Estimasi")) +
  geom_point(data=rru1,aes(x = length1, y = w_est, color="Pengukuran"))+
  scale_color_manual(values = c("Estimasi" = "black", "Pengukuran" = "red")) +
  labs(title="Hubungan Panjang dan Berat Ikan", color=NULL) + 
  xlab("Total Length (mm)") + ylab("Weight (g)")+
  coord_cartesian(xlim=c(200, 500))+
  scale_x_continuous(breaks=seq(200, 500, 50)) +
  coord_cartesian(ylim=c(100, 1800)) +
  scale_y_continuous(breaks=seq(250,1800, 250)) +
  annotate(geom="text", x = 200, y = 1750, label= eq1, hjust="left")
  
plot1
