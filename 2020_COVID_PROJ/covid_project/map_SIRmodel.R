library(tidyverse)
library(data.table)
library(sf)

#Import the output from the prediction model 
preds <- fread("predictions_25MAY.csv")
# Import County Shapefile
counties <- st_read("cb_2017_us_county_500k/cb_2017_us_county_500k.shp", stringsAsFactors = FALSE)
# Filter for only Indiana
IN_cty <- counties %>% filter(STATEFP == "18") %>% select(STATEFP,NAME,ALAND,geometry)
rm(counties)

# Convert to date to use in the if statements below
preds$Est.Peak.Date <- as.Date(strptime(preds$Est.Peak.Date,"%m/%d/%Y"))
#Determine the week the county will see its peak date
preds <- preds %>% mutate(peak_week = ifelse(Est.Peak.Date %in% as.Date("2020-05-03"):as.Date("2020-05-09"), "-3",
                                              ifelse(Est.Peak.Date %in% as.Date("2020-05-10"):as.Date("2020-05-16"), "-2",
                                                      ifelse(Est.Peak.Date %in% as.Date("2020-05-17"):as.Date("2020-05-23"), "-1",
                                                              ifelse(Est.Peak.Date %in% as.Date("2020-05-24"):as.Date("2020-05-30"), "0",
                                                                      ifelse(Est.Peak.Date %in% as.Date("2020-05-31"):as.Date("2020-06-06"), "1",
                                                                              ifelse(Est.Peak.Date %in% as.Date("2020-06-07"):as.Date("2020-06-13"), "2",
                                                                                      ifelse(Est.Peak.Date %in% as.Date("2020-06-14"):as.Date("2020-06-20"), "3",
                                                                                              ifelse(Est.Peak.Date %in% as.Date("2020-06-21"):as.Date("2020-06-27"), "4",
                                                                                                      ifelse(Est.Peak.Date %in% as.Date("2020-06-28"):as.Date("2020-07-04"), "5",
                                                                                                              ifelse(Est.Peak.Date %in% as.Date("2020-07-05"):as.Date("2020-07-11"), "6","NA")))))))))))



# Remove "County" from the preds County name column
preds$County <- str_remove_all(preds$County, " County")
#Join predictions to the shapefile
IN_cty <- IN_cty %>% inner_join(preds, by = c("NAME" = "County"))
IN_cty$peak_week <- as.factor(IN_cty$peak_week)
IN_cty$peak_week <- factor(IN_cty$peak_week, levels = c("-2","-1","0","1", "2", "3","4","5","6","7"))
prediction_map <- ggplot(data = IN_cty, aes(fill = peak_week, )) + geom_sf() +
  ggtitle("Predicted weeks until peak of COVID-19 cases \nby county in Indiana: Zero Week is 24-30 May 2020") +
  theme(panel.background = element_blank(), panel.grid = element_blank(), 
        axis.ticks = element_blank(), axis.text=element_blank(),
        plot.title = element_text(hjust = 0.6),
        legend.title.align = 0.3) +
  labs(fill = "Weeks until \nCases Peak") +
  scale_fill_brewer(palette = "RdYlBu")
  
prediction_map
