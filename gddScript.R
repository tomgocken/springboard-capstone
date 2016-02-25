mydata <- read.delim("C:/Projects/data/Air Temperature.txt")
mydata <- dplyr::mutate(mydata, Date = as.Date(paste(mydata$Year.Code, mydata$Month.Code, mydata$Day.of.Month.Code, sep="-")))
k <- dplyr::filter(mydata, Year == 2011 & Month %in% c("Apr", "May"),
                   County == "Columbia County, WI")
k <- dplyr::select(k,
                   County,
                   Year,
                   DayOfYr = Day.of.Year,
                   Date,
                   MaxAirTemp = Avg.Daily.Max.Air.Temperature..F.,
                   MinAirTemp = Avg.Daily.Min.Air.Temperature..F.
)
# alt in base: x <- mydata[mydata$Year == 2011 & (mydata$Month == "Apr" | mydata$Month == "May"), ]
PltDt <- as.Date("2011-04-10") # planting date
m <- dplyr::mutate(k,
                   gdd = ifelse(MaxAirTemp < 50, 0, # exclude negative values
                                (((ifelse(MaxAirTemp > 86, 86, MaxAirTemp) + ifelse(MinAirTemp < 50, 50, MinAirTemp)) / 2) - 50)),
                   agdd = dplyr::with_order(Date, cumsum, ifelse(Date <= PltDt, 0, gdd))
                   # doesn't work: agdd = dplyr::order_by(tmpdt, cumsum(ifelse(tmpdt <= pltdt, 0, gdd)))
)
plot(x = m$Date, y = m$agdd)
# Lower Limit (Base) = 50 degrees F; Upper Limit = 86 degrees F
# REF: https://en.wikipedia.org/wiki/Growing_degree-day
# REF: http://agron-www.agron.iastate.edu/Courses/agron212/Calculations/GDD.htm