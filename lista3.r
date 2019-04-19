library(dplyr)
library(stringr)
library(plotly)

if (!exists("d")) {
  d <- read.csv('./201807-citibike-tripdata.csv', sep = ',')

  x <- quantile(d$tripduration, probs = c(0.995))[[1]]
  d <- d[d$tripduration <= x, ]
  print(x)

  d$start.day <- as.numeric(substr(d$starttime, 9, 10))
  d$stop.day <- as.numeric(substr(d$stoptime, 9, 10))
}

if (!exists("week2")) {
  week2 <- d[d$stop.day >= 9 & d$start.day <= 15, ]
  attach(week2)
}

if (!exists("bikesInUse")) {
  bikesInUse <- group_by (
    week2[stop.day - start.day == 1 & start.day <= 14, ], 
    start.day[stop.day - start.day == 1 & start.day <= 14]
  ) %>% summarize (qtd  = n())
  print(bikesInUse)
}

# item 3
week2$start.hour <- as.numeric(substr(week2$starttime, 12, 13))
week2$stop.hour <- as.numeric(substr(week2$stoptime, 12, 13))
week2$start.min <- as.numeric(substr(week2$starttime, 15, 16))
week2$stop.min <- as.numeric(substr(week2$stoptime, 15, 16))
week2$start.sec <- as.numeric(substr(week2$starttime, 18, 19))
week2$stop.sec <- as.numeric(substr(week2$stoptime, 18, 19))
attach(week2)
week2$start.t <- (start.day - 9) * (24 * 3600) + start.hour*3600 + start.min*60 + start.sec
week2$stop.t <- (stop.day - 9) * (24 * 3600) + stop.hour*3600 + stop.min*60 + stop.sec
attach(week2)

bikesActivity <- function (tripdata, interval = 60, start = 0, stop = 7*24*60*60) {
  tripdata <- tripdata[, c("start.t", "stop.t")]
  bikes <- c()
  for (t in seq(start, stop, interval)) {
    tripdata <- tripdata[tripdata$stop.t >= t, ]
    n <- sum(tripdata$start.t <= t & tripdata$stop.t >= t)
    bikes <- c(bikes, n)
  }

  return(bikes)
}

print(summary(bikesActivity(week2)))

plotDayActivity <- function(data, day, title, filename) {
  x <- seq(0, 24, 1/60)
  dayData <- data[data$stop.day >= day & data$start.day <= day, ]
  dayActivity <- bikesActivity(dayData, start = (day - 9)*24*60*60, stop = (day - 8)*24*60*60)
  dayPlot <- plot_ly(x = x, y = dayActivity, mode = "lines") %>%
    layout(
      title = title,
      xaxis = list(title = "Tempo (segundos)"), 
      yaxis = list(title = "Bicicletas em uso")
    )
  orca(dayPlot,  paste(c(filename, ".png"), sep=""))
}

plotDayActivity(week2, 10, "Terça", "bike-activity_terca")

plotDayActivity(week2[usertype == "Customer", ], 10, 
  "Terça (Customers)", "bike-activity_terca-customers")

plotDayActivity(week2[usertype == "Subscriber", ], 10, 
  "Terça (Subscribers)", "bike-activity_terca-subscribers")

plotDayActivity(week2[gender == 1, ], 10, 
  "Terça (Homens)", "bike-activity_terca-homens")

plotDayActivity(week2[gender == 2, ], 10, 
  "Terça (Mulheres)", "bike-activity_terca-mulheres")


plotDayActivity(week2, 15, "Domingo", "bike-activity_domingo")

plotDayActivity(week2[usertype == "Customer", ], 15, 
  "Domingo (Customers)", "bike-activity_domingo-customers")

plotDayActivity(week2[usertype == "Subscriber", ], 15, 
  "Domingo (Subscribers)", "bike-activity_domingo-subscribers")

plotDayActivity(week2[gender == 1, ], 15, 
  "Domingo (Homens)", "bike-activity_domingo-homens")

plotDayActivity(week2[gender == 2, ], 15, 
  "Domingo (Mulheres)", "bike-activity_domingo-mulheres")