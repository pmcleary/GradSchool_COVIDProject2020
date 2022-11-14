library(tidyverse)
library(data.table)
library(lubridate)

####################Import the data#########################
IN_cases <- fread("covid_cases_through_25MAY.csv")
IN_cases <- as.data.table(IN_cases)
IN_pop <-  fread("co-est2019-alldata.csv")
IN_cases <- IN_cases %>% 
  filter(State == "IN" & `County Name` != "Statewide Unallocated") %>%  
  select(State, County = `County Name`,c(`3/6/2020`:`5/25/2020`))
IN_cases <- as.data.table(IN_cases)
IN_pop <- IN_pop[STATE == 18 & CTYNAME != "Indiana",.(CTYNAME,POPESTIMATE2019)]
#Left join the two data tables
IN_cases_pop <- IN_cases[IN_pop, on = .(County = CTYNAME)]

###################Begin the for loop#####################

#Initialize df for the for loop
output_df <- data.frame(County=as.character(),
                        peak_date=as.numeric(),
                        cases_pk_date=as.numeric())

for(i in 1:dim(IN_cases_pop)[1]) {
  
  test <- IN_cases_pop[i,]
  # Cast DT to long format
  test <- melt(test, id.vars = c("State","County","POPESTIMATE2019"), 
               variable.name = "day", value.name = "cases")
  #Convert day column to character from factor
  test$day <- as.character(test$day)
  # filter out days with zero cases
  test <- test[cases != 0,]
  ###Variables for the SIR model
  # select date of first case
  datefirst <- as.Date(strptime(test$day[1],"%m/%d/%y"))
  N <-as.numeric(test[1,POPESTIMATE2019])
  Day <- 1:(as.numeric(length(test$day)))
  Infected <- as.numeric(as.vector(test[,cases]))
  county_name <- test[1,County]
  
  ###############Implement the SIR model###############
    
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
  
  t <- 1:90 # time in days
  fit <- data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
  
  ##################End of SIR model######################
  
  #Calculate the date of peak cases and the number of cases for the county
  peak_day <- fit[which.max(fit$I),1]
  peak_date <- datefirst + peak_day
  cases_pk_date <- round(fit[which.max(fit$I),3])
  Est_Total_Deaths <- round(N * 0.00037) 
  # Compile data into a date frame
  loop_data <- list(County = county_name, Est.Peak.Date = peak_date, Est.Cases.on.Peak = cases_pk_date, Est_Total_Deaths = Est_Total_Deaths)
  loop_output <- as.data.frame(loop_data)
  output_df <- rbind(output_df, loop_output)
}

output_df <- output_df %>% mutate(Est_Hospitalizations_on_Peak = round(Est.Cases.on.Peak * 0.05)) %>%
 mutate(Est_ICU_Beds_Required_on_Peak = round(Est_Hospitalizations_on_Peak * 0.4)) %>%
 mutate(Est_Ventilators_Required_on_Peak = round(Est_ICU_Beds_Required_on_Peak * 0.5)) %>%
 mutate("Est Peak Date of Deaths" = Est.Peak.Date + 18) %>%
 mutate("Est Deaths on Peak Date of Deaths (3)" = round(Est.Cases.on.Peak * 0.014))

#%>% mutate("Total Expected Deaths (3)" = IN_pop * 0.0003)
fwrite(output_df,"predictions_25MAY.csv")


# tipp <- IN_cases %>% filter(County == "Tippecanoe County")
# tipp <- as.data.table(tipp)
# tipp <- melt(tipp, id.vars = c("State","County"), 
#              variable.name = "day", value.name = "cases")
# tipp$day <- as.Date(strptime(tipp$day,"%m/%d/%y"))
# ggplot(tipp, aes(x = day, y = cases)) + geom_col() +
#   ggtitle("Tippecanoe Co. COVID-19 Cases: March 6th-May 3rd") +
#  labs(x = "Date", y = "Total Cases")
#for individual runs in the model
#tipp <- as.numeric(as.vector(tipp[,16:61]))