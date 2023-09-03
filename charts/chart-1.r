setwd("~/Projects/forest-ecology/data")
dane <- read.csv2("BIOMARK-BLUP_2.csv", sep = ";")

dane <- dane[order(dane$year_time),] 
dane$Year <- as.character(dane$year_time)

plot(x = dane$Exploration_rate, 
     y = dane$dispersal_distance,
     pch = 16, cex = 0.5, 
     ylab = "Seed dispersal distance (cm)",
     xlab = "Crossings")