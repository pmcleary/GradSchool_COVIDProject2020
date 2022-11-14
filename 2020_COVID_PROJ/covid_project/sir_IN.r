library(tidyverse)
library(data.table)
library(lubridate)
IN_cases <- fread("covid_cases_usafacts_through3MAY.csv")
IN_cases <- IN_cases[State == "IN" & countyFIPS != 0 & countyFIPS != 0, -c("countyFIPS","stateFIPS")]
#IN_cases <- IN_cases[1:60,]
IN_pop <-  fread("co-est2019-alldata.csv")
#IN_cases <- IN_cases[1:48, c("Date Reported", "Total Cases")]
#IN_cases <- IN_cases %>% arrange(`Total Cases`)
#remove_wdays <- c("Monday, ","Tuesday, ","Wednesday, ","Thursday, ","Friday, ","Saturday, ","Sunday, ")
#IN_cases$`Date Reported` <- as.data.table(sapply(IN_cases$`Date Reported`, function(x) 
#  gsub(paste(remove_wdays, collapse = '|'), '', x)))
IN_cases$`Date Reported` <- as.POSIXct(strptime(IN_cases$`Date Reported`, "%B %d,%Y"))
#IN_cases$`Date Reported` <- format(IN_cases$`Date Reported`, format = "%b-%d")
IN_cases <- IN_cases %>% arrange(`Total Cases`, `Date Reported`)
IN_cases_vec <- IN_cases[["Total Cases"]]

IN_cases <- as.data.table(IN_cases)
IN_pop <- as.numeric(IN_pop[STATE == 18 & CTYNAME == "Tippecanoe County",POPESTIMATE2019])

county <- as.character("Tippecanoe")

#Infected <- as.numeric(as.vector(IN_cases[County == paste(county,"County") ,6:42]))
Infected <- tipp
Day <- 1:(length(Infected))
N <- IN_pop
#N <- as.numeric(IN_pop[CTYNAME == paste(county,"County"),3]) # population of the county

old <- par(mfrow = c(1, 2))
#plot(Day, Infected, type ="b")
#plot(Day, Infected, log = "y")
#abline(lm(Infected) ~ Day)
#title("Confirmed infections COVID-19 in Adams County", outer = TRUE, line = -2)

SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta/N * I * S
    dI <- beta/N * I * S - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
    })
}

library(deSolve)
init <- c(S = N-Infected[1], I = Infected[1], R = 0)
RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = Day, func = SIR, parms = parameters)
  fit <- out[ , 3]
  sum((Infected - fit)^2)
}
 
Opt <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1)) # optimize with some sensible conditions
Opt$message
## [1] "CONVERGENCE: REL_REDUCTION_OF_F <= FACTR*EPSMCH"
 
Opt_par <- setNames(Opt$par, c("beta", "gamma"))
print(Opt_par)
 
t <- 1:120 # time in days
fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col <- 1:3 # colour
print(fit) 
#matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
#matplot(fit$time, fit[ , 2:4], type = "l", xlab = "Day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")

#points(Day, Infected)
#legend("bottomright", c("Susceptible", "Infected", "Recovered"), lty = 1, lwd = 2, col = col, inset = 0.05)
#title("Predicted Cases 2019-nCoV Adams Cty (worst case)", outer = TRUE, line = -2)

base <- as.Date("2020-03-06")
peak_day <- fit[which.max(fit$I),1]
peak_date <- base + peak_day
caption <- paste("Peak Date =",peak_date)
caption2 <- paste("# Cases at Peak =",round(fit[which.max(fit$I),3]))
ggplot(fit, aes(x = time)) +
  geom_line(aes(y = fit$S, color = "Susceptible"),size = 1.5) +
  geom_line(aes(y = fit$I, color = "Infected"), size = 1.5) +
  geom_line(aes(y = fit$R, color = "Recovered"), size =1.5) +
  scale_y_log10() + 
  labs(title = paste("Predicted Cases COVID-19", county,"County (worst case)"),y="Population",x="Days since first COVID-19 case in the county",
       subtitle = "Model: SIR with no mitigation factors considered",
       caption = "Author: Cleary, Paul, COVID-19 Data = usafacts.org") +
  theme(legend.position = "right",
  legend.title = element_blank()) +
  theme(plot.caption = element_text(hjust = 0)) +
  theme(legend.background = element_rect("white")) +
  annotate(geom = "text", x = 75, y=8,label = caption) +
  annotate(geom = "text", x = 75, y=2,label = caption2)

#Plot key Days in Indiana
# IN_key_days <- IN_cases[`Date Reported` %chin% c("Friday, March 6, 2020", "Thursday, March 12, 2020",
#                                                  "Monday, March 16, 2020", "Thursday, March 19, 2020",
#                                                  "Tuesday, March 24, 2020", "Wednesday, March 25, 2020",
#                                                  "Wednesday, April 8, 2020")]
# # Rename the days for easier plotting
# IN_key_days <- IN_key_days %>% mutate(Date = recode(`Date Reported`, "Friday, March 6, 2020"="6MAR",
#                                                     "Thursday, March 12, 2020"="12MAR", "Monday, March 16, 2020"="16MAR", "Thursday, March 19, 2020"="19MAR",
#                                                     "Tuesday, March 24, 2020"="24MAR", "Wednesday, March 25, 2020"="25MAR",
#                                                     "Wednesday, April 8, 2020"="8APR"))
IN_key_days <- IN_cases[1:34,] 
ggplot(IN_key_days, aes(x = `Date Reported`, y = `Total Cases`)) + geom_col() +
  ggtitle("Indiana COVID-19 Cases: March 6th-April 8th") +
  labs(x = "Date", y = "Total Cases") + geom_smooth()
