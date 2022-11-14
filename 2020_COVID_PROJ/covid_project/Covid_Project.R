####################LIBRARIES################
library(tidyverse)
library(data.table)
library(sf)
library(classInt)
library(viridis)
library(biscale)
library(cowplot)
library(spdep)
library(mapview)
library(geosphere)
library(ggspatial)
####################DATA IMPORT##############

# Import County Shapefile
counties <- st_read("cb_2017_us_county_500k/cb_2017_us_county_500k.shp", stringsAsFactors = FALSE)
#Import data of covid cases
cases_usa <- fread("covid_cases_usafacts_through3MAY.csv", stringsAsFactors = FALSE)
cases_usa <- as.data.table(cases_usa)
pop_cnty_usa <- fread("co-est2019-alldata.csv")

#################Initial Filtering###########

################POPULATION DATA#############
#Filter for IL and remove excess columns
pop_IL <- pop_cnty_usa[STATE == 17,.(STATE,COUNTY,NAME = CTYNAME,POPESTIMATE2019)][COUNTY != 0,]
#Remove county from the name of the county for IL
pop_IL$NAME <- str_remove_all(pop_IL$NAME, " County")
#Filter for IN and remove excess columns
pop_IN <- pop_cnty_usa[STATE == 18,.(STATE,COUNTY,NAME = CTYNAME,POPESTIMATE2019)][COUNTY != 0,]
#Remove county from the name of the county for IL
pop_IN$NAME <- str_remove_all(pop_IN$NAME, " County")
pop_IN$NAME <- paste(pop_IN$NAME, "IN", sep = "")
pop_IL_IN <- rbind(pop_IL,pop_IN)
################COVID DATA######################
#Select IL from cases file 
cases_IL <- cases_usa[State == "IL" & countyFIPS != 0 & countyFIPS != 0, -c("countyFIPS","stateFIPS")]
cases_IL <- rename(cases_IL, NAME = `County Name`)
cases_IL$NAME <- str_remove_all(cases_IL$NAME, " County")
#Select IN from cases file 
cases_IN <- cases_usa[State == "IN" & countyFIPS != 0 & countyFIPS != 0, -c("countyFIPS","stateFIPS")]
cases_IN <- rename(cases_IN, NAME = `County Name`)
cases_IN$NAME <- str_remove_all(cases_IN$NAME, " County")
cases_IN$NAME <- paste(cases_IN$NAME, "IN", sep = "")
cases_IL_IN <- rbind(cases_IL,cases_IN)
cases_IL_IN <- cases_IL_IN[,!3:4] #take away two days with no cases at begining
#########Remove Intermediate Objects###########
rm(cases_usa)
rm(pop_cnty_usa)

########JOIN COVID and Population Data###########
# Previously added IN to end of Indiana counties to avoid 
# duplicate counting of same named counties during the join
cases_pop <- cases_IL_IN %>% left_join(pop_IL_IN, by = "NAME") %>% 
  select(-STATE, -COUNTY)
rm(pop_IL)
rm(pop_IN)
rm(pop_IL_IN)
rm(cases_IL)
rm(cases_IN)
####################Counties Shapefile##################
shp_IL <- counties %>% filter(STATEFP == 17) %>%
        select(STATEFP,NAME,ALAND,geometry)
shp_IN <- counties %>% filter(STATEFP == 18) %>%
  select(STATEFP,NAME,ALAND,geometry)
# Add IN to the end of Indiana counties to avoid confusion of same named counties
# when joining the shapefile to cases 
shp_IN$NAME[shp_IN$STATEFP == 18] <- paste(shp_IN$NAME,"IN",sep = "")
shp_IL_IN <- rbind(shp_IL,shp_IN)
rm(counties)
rm(shp_IL)
rm(shp_IN)
##########Join Shapefile to pop & covid data#############
# The mutate id = row_number is a trick to avoid duplicate matching during the join because there
#are counties that have the same name in both states;
shp_cases_pop <- shp_IL_IN %>% left_join(cases_pop, by = "NAME")%>%
                mutate(ALAND = ALAND/1000000) %>%
                mutate(DENSITY = POPESTIMATE2019/ALAND) %>%
                rename(ALAND_sq_km = ALAND)

shp_cases_pop <- shp_cases_pop %>% pivot_longer(c(`1/24/2020`:`5/3/2020`),names_to = "Day", values_to = "cases")
shp_cases_pop <- shp_cases_pop %>% select(-ALAND_sq_km,-State)
#Convert the Day Variable to as.Date
shp_cases_pop$Day <- as.Date(shp_cases_pop$Day, "%m/%d/%Y")
# Convert back to SF object
shp_cases_pop <- st_as_sf(shp_cases_pop)



# #################### Chloropleth with kmeans ##################
# Population Density
set.seed(100)
km_density <- classIntervals(shp_cases_pop$DENSITY, n= 9, style = "kmeans")
shp_cases_pop$km_density_pop <-cut(shp_cases_pop$DENSITY, breaks = km_density$brks, include.lowest = TRUE)
breaks_km_density <- levels(shp_cases_pop$km_density_pop)
#All cases
density_map <- ggplot(data = shp_cases_pop) + geom_sf() +
 ggtitle("Population Density of Illinois and Indiana") +
 aes(fill = km_density_pop) + theme(axis.text=element_blank()) +
 theme(#panel.background = element_blank(), panel.grid = element_blank(),
       axis.ticks = element_blank()) +
 scale_fill_manual(values = rev(magma(9)), breaks = rev(breaks_km_density),
                   name = "Population/SQkm", drop = FALSE, labels = c('2100','773','265','137','83,9','52.4','33.6','22.1','<12.8'),
                   guide = guide_legend(keyheight = unit(2, units = "mm"),
                                        keywidth = unit(8 / length(labels), units = "mm"))) +
  annotation_north_arrow(which_north = "true",   height = unit(.8, "cm"),
                         width = unit(.8, "cm"))

#Display the population density map
density_map


# ######### kmeans +1  for COVID Display ###################################
 # COVID Cases
set.seed(100)
km_cases <- classIntervals(shp_cases_pop$cases, n= 8, style = "kmeans")
shp_cases_pop$cases_km_pls1 <-cut(shp_cases_pop$cases, breaks = c(0,0.99,267,1256,3288,7736,15029,23863,32701,42324), include.lowest = TRUE)
breaks_cases_km_1 <- levels(shp_cases_pop$cases_km_pls1)
label_scale_km_1 <- rev(breaks_cases_km_1)
shp_cases_pop$Day <- as.character(shp_cases_pop$Day)
#filter for every 5th day
cov_cases_map <- ggplot(data = filter(shp_cases_pop,Day == '2020-03-06' | Day == '2020-03-13' |
               Day == '2020-03-20' | Day == '2020-03-27' | Day == '2020-04-03' |
               Day == '2020-04-10' | Day == '2020-04-17' | Day == '2020-04-22')) + geom_sf() +
 ggtitle("COVID cases in Illinois and Indiana March 6 - April 22") +
 aes(fill = cases_km_pls1) + theme(axis.text=element_blank()) +
 theme(panel.background = element_blank(), panel.grid = element_blank(),
       axis.ticks = element_blank()) +
 scale_fill_manual(values = rev(magma(9)), breaks = rev(breaks_cases_km_1),
                   name = "COVID Cases", drop = FALSE, labels = c('>42324','32701','23863','15029','7736','3288','1256','267','0'),
                   guide = guide_legend(keyheight = unit(2, units = "mm"),
                                        keywidth = unit(8 / length(labels), units = "mm"))) +
 facet_wrap('Day')

#Display the COVID Map
cov_cases_map

#COVID cases of most recent day

rec_cov_cases_map <- ggplot(filter(shp_cases_pop,Day == "2020-05-03")) + geom_sf() +
  ggtitle("COVID cases in Illinois and Indiana as of 03 May 2020") +
  aes(fill = cases_km_pls1) + theme(axis.text=element_blank()) +
  theme(#panel.background = element_blank(), panel.grid = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values = rev(magma(9)), breaks = rev(breaks_cases_km_1),
                    name = "COVID Cases", drop = FALSE, labels = c('>42324','32701','23863','15029','7736','3288','1256','267','0'),
                    guide = guide_legend(keyheight = unit(2, units = "mm"),
                                         keywidth = unit(8 / length(labels), units = "mm"))) +
  annotation_north_arrow(which_north = "true",   height = unit(.8, "cm"),
                         width = unit(.8, "cm"))
#Display the COVID Map
rec_cov_cases_map
#---------------------Correlation between density and cases--------------------
corr_plot <- ggplot(filter(shp_cases_pop, Day == "2020-04-22"), aes(x = DENSITY, y = cases)) + geom_point()
corr_nums <- cor.test(shp_cases_pop$DENSITY, shp_cases_pop$cases, method = "spearman")


########################Bivariate Plot########################
# Need to filter for one day to avaoid error using bi_class, 
# it cannot break the dat into quantiles due to repreats
biv_plot <- bi_class(filter(shp_cases_pop, Day == "2020-05-03"), x = DENSITY, y = cases, style = "quantile", dim = 3)
bivariate_map <- ggplot() + geom_sf(data = biv_plot, mapping = aes(fill = bi_class), color = "white",
                                    size = 0.1, show.legend = FALSE) +
  bi_scale_fill(pal = "DkViolet", dim = 3) +
  ggtitle("Population Density and COVID-19 \n in Illinois & Indiana") +
  bi_theme() +
  theme(plot.title = element_text(size = 16)) +
  annotation_north_arrow(which_north = "true",   height = unit(.8, "cm"),
                         width = unit(.8, "cm"))

biv_legend <- bi_legend(pal = "DkViolet", dim = 3,xlab = "Density",ylab = "COVID-19 Cases",
                    size = 8)

# combine map with legend
finalbivPlot <- ggdraw(bivariate_map) +
  draw_plot(bivariate_map, 0, 0, 1, 1) +
  draw_plot(biv_legend, 0.7, .4, 0.2, 0.2)


#Display the bivariate map
finalbivPlot

#---------------------Calculate Moran's I-------------------------
# Filter for most recent day of covid data in imported csv file

recent_cases_shp <- shp_cases_pop %>% filter(Day == max(shp_cases_pop$Day))

#Calculate the neighbors using the queen method
neighb_qn <- poly2nb(recent_cases_shp, queen=TRUE)
#Calculate the weight matrix
weight <- nb2listw(neighb_qn, style="B")
#Compute the avergae neighbor COVID cases and density
# They are called spatially lagged values
# COVID Cases lag
Cov.lag <- lag.listw(weight, recent_cases_shp$cases)
#Density lag
Dens.lag <- lag.listw(weight, recent_cases_shp$DENSITY)
#Moran's I of cases
cases_moran <- moran.test(recent_cases_shp$cases,weight)
# Run a monte carlo simulation to determine the p-value
cases_MC <- moran.mc(recent_cases_shp$cases, weight, nsim = 599)
#Visualize where the p-value falls on a density plot
#of the simulated monte carlo experiment
plot(cases_MC,main="",las=1)

#Moran's I of population density
density_moran <- moran.test(recent_cases_shp$DENSITY, weight)
# Run a monte carlo simulation to determine the p-value
density_MC <- moran.mc(recent_cases_shp$DENSITY, weight, nsim = 599)
#Visualize where the p-value falls on a density plot
#of the simulated monte carlo experiment
plot(density_MC,main="",las=1)

#-----------Traffic Experiment---------------------------

#Create centroids of IN counties
# centroid does not work for lat long unprojected data 
# project into epsg: 32616, utm zone 16n
shp_IL_IN <- st_transform(shp_IL_IN,crs  = 32616)
cents <- st_centroid(shp_IL_IN)


#-----------------------Commuting Data----------------------

commut_data <- fread("commuting_flow.csv")
#Extract the data for Indy
commut_data_marion <- commut_data[`State FIPS Code_WP` == 18 & `County Name_WP` == "Marion County" & `State FIPS Code_Res` == 18,]
commut_data_marion <- commut_data_marion[,-c("V11","V12","V13","V14")]
#Extract the data for Chicago
commut_data_cook <- commut_data[`State FIPS Code_WP` == 17 & `County Name_WP` == "Cook County",]
commut_data_cook <- commut_data_cook[,-c("V11","V12","V13","V14")]

#Convert column to numeric and remove commas
commut_data_marion$`Workers in Commuting Flow` <- as.numeric(gsub(",","",commut_data_marion$`Workers in Commuting Flow`))
commut_data_cook$`Workers in Commuting Flow` <- as.numeric(gsub(",","",commut_data_cook$`Workers in Commuting Flow`))
rm(commut_data)
#Find the 20 commuting counties to Marion and Cook
marion_top20 <- top_n(commut_data_marion,21,`Workers in Commuting Flow`)
cook_top_20 <- top_n(commut_data_cook,21,`Workers in Commuting Flow`)
#Remove the Wisconsin and Michigan Counties, were low numbers anyway
cook_top_16 <- cook_top_20[1:16,]
# Remove county from the column to join
marion_top20$`County Name_Residence` <- str_remove_all(marion_top20$`County Name_Residence`, " County")
cook_top_16$`County Name_Residence` <- str_remove_all(cook_top_16$`County Name_Residence`, " County")
#Give Indiana Counties IN identifier
cook_top_16 <- mutate(cook_top_16, `County Name_Residence` = ifelse(`State FIPS Code_Res` == "18", paste(cook_top_16$`County Name_Residence`,"IN", sep = ""), `County Name_Residence`))
# Select only neccessary columns
marion_top20 <- marion_top20 %>% select(`County Name_Residence`, `Workers in Commuting Flow`)
cook_top_16 <- cook_top_16 %>% select(`County Name_Residence`, `Workers in Commuting Flow`)
# Add IN to end of county for the join
marion_top20$`County Name_Residence` <- paste(marion_top20$`County Name_Residence`, "IN", sep = "")
#Determine the centroids of Indiana & Illinois
cents_IN <- cents %>% filter(STATEFP =="18")
# Join centroids of Iniana and marion top 20 and cook 
marion_top20 <- marion_top20 %>% left_join(cents_IN, by = c("County Name_Residence" = "NAME"))
cook_top_16 <- cook_top_16 %>% left_join(cents, by = c("County Name_Residence" = "NAME"))
#Make it a sf object, was just a df
marion_top20 <- st_as_sf(marion_top20)
cook_top_16 <- st_as_sf(cook_top_16)
#Arrange is decreasing order of commuting workers 
marion_top20 <- arrange(marion_top20, desc(`Workers in Commuting Flow`))
cook_top_16 <- arrange(cook_top_16, desc(`Workers in Commuting Flow`))
rm(cook_top_20)
#Execute loop for gcIntermediate
gclist <- vector(mode = "list", length = 16)
i <- 1
for(i in 1:dim(cook_top_16)[1]) {
  # project to long/lat
  m_top20 = st_transform(cook_top_16, crs = 4326)
  mcents = unlist(m_top20[1,5])
  temp = unlist(m_top20[i,5])
  temp2 = gcIntermediate(mcents,temp, n=20, addStartEnd=TRUE)
  gclist[[i]] <- temp2
  i = i + 1
}


#---------------Create sf object of desire lines----------------------
#Remove the marion county element, be careful, if this is run again it will remove the original list
gclist[[1]] <- NULL
sfgclist <- vector(mode = "list", length = 20)
i <- 1
for(i in 1:length(gclist)) {
  tmp <- as.data.frame(gclist[[i]]) 
  tmp <- st_as_sf(tmp, coords = c("lon","lat"))
  tmp <- tmp %>% st_set_crs(4326) %>% st_transform(crs=32616)
  tmp <- tmp %>% st_union(do_union = FALSE) %>% st_cast("LINESTRING")
  tmp <- tmp %>% as.data.frame() %>% st_as_sf()
  sfgclist[[i]] <- tmp
  i = i + 1
}


cook_desire_lines <- do.call(rbind, sfgclist)
#Shapefile of just Indiana
shp_cases_pop_IN <- shp_cases_pop %>% filter(STATEFP == "18")

#---------------Commuters to Indy--------------------------------
# Create classes for commuting data

comm_classes <- c(1,1,1,1,1,1,2,2,2,2,2,3,3,3,3,3,4,4,4,4,4)
marion_NAME <- as.vector(marion_top20$`County Name_Residence`)
c <- data.frame(comm_classes,NAME = marion_NAME)
# Add IN to end of county for the join
#c$NAME <- paste(c$NAME, "IN", sep = "")
shp_cases_pop_Indy <- c %>%  left_join(shp_cases_pop, by = "NAME")
commut_map <- ggplot() + geom_sf(data = shp_cases_pop_Indy, aes(fill = comm_classes)) + geom_sf(data = desire_lines)  +
  geom_sf(data = marion_top20) + geom_sf(shp_cases_pop_IN)
  theme(panel.background = element_blank(), panel.grid = element_blank(),
        axis.ticks = element_blank()) + theme(axis.text=element_blank()) +
  ggtitle("Top 20 Counties with Commuters to Indianapolis (Marion Co)")
commut_map

#----------------- facet wrap Indy Counties COVID Growth-----------------------
km_cases_Indy <- classIntervals(shp_cases_pop_IN$cases, n= 5, style = "kmeans")
shp_cases_pop_Indy$cases_km_pls1 <-cut(shp_cases_pop_Indy$cases, breaks = c(0,0.99,93,447,1480,3419,6176) , include.lowest = TRUE)
Indy_breaks_cases_km_1 <- levels(shp_cases_pop_Indy$cases_km_pls1)
Indy_label_scale_km_1 <- rev(Indy_breaks_cases_km_1)
shp_cases_pop_Indy$Day <- as.character(shp_cases_pop_Indy$Day)
shp_cases_pop_Indy <- st_as_sf(shp_cases_pop_Indy)
#filter for every week
Indy_cases_map <- ggplot(data = filter(shp_cases_pop_Indy,Day == '2020-03-06' | Day == '2020-03-13' |
                                         Day == '2020-03-20' | Day == '2020-03-27' | Day == '2020-04-03' |
                                         Day == '2020-04-10' | Day == '2020-04-17' | Day == '2020-04-24' | Day == '2020-05-01' |
                                         Day == '2020-05-03')) + geom_sf() +
  ggtitle("COVID-19 cases in Indianapolis Area March 6 - May 3") +
  aes(fill = cases_km_pls1) + theme(axis.text=element_blank()) +
  theme(panel.background = element_blank(), panel.grid = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values = rev(magma(9)), breaks = rev(Indy_breaks_cases_km_1),
                    name = "COVID Cases", drop = FALSE, labels = c('6176','3419','1480','447','93','0'),
                    guide = guide_legend(keyheight = unit(2, units = "mm"),
                                         keywidth = unit(8 / length(labels), units = "mm"))) +
  facet_wrap('Day')

Indy_cases_map
#---------------Commuters to Chicago--------------------------------
# Create classes for commuting data

cook_comm_classes <- c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
cook_NAME <- as.vector(cook_top_16$`County Name_Residence`)
cook_df <- data.frame(cook_comm_classes,NAME = cook_NAME)
cook_comm_sf <- shp_IL_IN %>% left_join(cook_df, by = "NAME")
cook_commut_map <- ggplot() + geom_sf(data = cook_comm_sf, aes(fill = cook_comm_classes)) + geom_sf(data = cook_desire_lines)  +
  geom_sf(data = cook_top_16) +
  theme(panel.background = element_blank(), panel.grid = element_blank(),
        axis.ticks = element_blank()) + theme(axis.text=element_blank()) +
  ggtitle("Top 16 Counties with Commuters to Chicago (Cook Co)")
cook_commut_map

#----------------- facet wrap Chicago Counties COVID Growth-----------------------
# Create Chicago only sf object
Chi_reg_sf <- cook_df %>% inner_join(shp_IL_IN, by = "NAME")
#Add cases to Chi sf object
cases_IL_IN <- cases_IL_IN %>% pivot_longer(c(`1/24/2020`:`5/3/2020`),names_to = "Day", values_to = "cases")
cases_IL_IN$Day <- as.Date(cases_IL_IN$Day, "%m/%d/%Y")
Chi_reg_sf <- Chi_reg_sf %>% left_join(cases_IL_IN, by = "NAME")
km_cases_Chi <- classIntervals(Chi_reg_sf$cases, n= 5, style = "kmeans")
Chi_reg_sf$cases_km_pls1 <-cut(Chi_reg_sf$cases, breaks = c(0,0.99,1144,6956,16814,28337,42324) , include.lowest = TRUE)
Chi_breaks_cases_km_1 <- levels(Chi_reg_sf$cases_km_pls1)
Chi_label_scale_km_1 <- rev(Chi_breaks_cases_km_1)
Chi_reg_sf$Day <- as.character(Chi_reg_sf$Day)
Chi_reg_sf <- st_as_sf(Chi_reg_sf)
#filter for every week
Chi_cases_map <- ggplot(data = filter(Chi_reg_sf,Day == '2020-01-24' | Day == '2020-02-04' |
                                         Day == '2020-02-14' | Day == '2020-02-24' | Day == '2020-03-05' |
                                         Day == '2020-03-15' | Day == '2020-03-25' | Day == '2020-04-04' | Day == '2020-04-14' |
                                         Day == '2020-04-24' | Day == '2020-05-03')) + geom_sf() +
  ggtitle("COVID-19 cases Chicago Area January 24 - May 3") +
  aes(fill = cases_km_pls1) + theme(axis.text=element_blank()) +
  theme(panel.background = element_blank(), panel.grid = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values = rev(magma(9)), breaks = rev(Chi_breaks_cases_km_1),
                    name = "COVID Cases", drop = FALSE, labels = c('42324','28337','16814','6956','1144','0'),
                    guide = guide_legend(keyheight = unit(2, units = "mm"),
                                         keywidth = unit(8 / length(labels), units = "mm"))) +
  facet_wrap('Day')

Chi_cases_map



#------------------------Cases around Indiana---------------------------------

set.seed(100)
km_cases_IN <- classIntervals(shp_cases_pop_IN$cases, n= 8, style = "kmeans")
shp_cases_pop_IN$cases_km_pls1 <-cut(shp_cases_pop_IN$cases, breaks = c(0,0.99,31.5,98,239,466,920,1858,3133,4263), include.lowest = TRUE)
IN_breaks_cases_km_1 <- levels(shp_cases_pop_IN$cases_km_pls1)
IN_label_scale_km_1 <- rev(IN_breaks_cases_km_1)
shp_cases_pop_IN$Day <- as.character(shp_cases_pop_IN$Day)
#filter for every week
Indy_cases_map <- ggplot(data = filter(shp_cases_pop_IN,Day == '2020-03-06' | Day == '2020-03-13' |
                                        Day == '2020-03-20' | Day == '2020-03-27' | Day == '2020-04-03' |
                                        Day == '2020-04-10' | Day == '2020-04-17' | Day == '2020-04-22')) + geom_sf() +
  ggtitle("COVID cases in Indiana March 6 - April 22") +
  aes(fill = cases_km_pls1) + theme(axis.text=element_blank()) +
  theme(panel.background = element_blank(), panel.grid = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values = rev(magma(9)), breaks = rev(IN_breaks_cases_km_1),
                    name = "COVID Cases", drop = FALSE, labels = c('4263','3133','1858','920','466','239','98','31','0'),
                    guide = guide_legend(keyheight = unit(2, units = "mm"),
                                         keywidth = unit(8 / length(labels), units = "mm"))) +
  facet_wrap('Day')


Indy_cases_map

#----------------------Facet Wrap All of Il amd IN
set.seed(100)
km_cases_IL_IN <- classIntervals(shp_cases_pop$cases, n= 8, style = "kmeans")
shp_cases_pop$cases_km_pls1 <-cut(shp_cases_pop$cases, breaks = c(0,0.99,267,1256,3288,7736,15029,23863,32701,42324), include.lowest = TRUE)
IL_IN_breaks_cases_km_1 <- levels(shp_cases_pop$cases_km_pls1)
IL_IN_label_scale_km_1 <- rev(IL_IN_breaks_cases_km_1)
shp_cases_pop$Day <- as.character(shp_cases_pop$Day)
#filter for every week
IL_IN_cases_map <- ggplot(data = filter(shp_cases_pop,Day == '2020-01-24' | Day == '2020-02-04' |
                                          Day == '2020-02-14' | Day == '2020-02-24' | Day == '2020-03-05' |
                                          Day == '2020-03-15' | Day == '2020-03-25' | Day == '2020-04-04' | 
                                          Day == '2020-04-14' | Day == '2020-04-24' | Day == '2020-05-03')) + geom_sf() +
  ggtitle("COVID cases in Illinois & Indiana January 24 - May 03") +
  aes(fill = cases_km_pls1) + theme(axis.text=element_blank()) +
  theme(panel.background = element_blank(), panel.grid = element_blank(),
        axis.ticks = element_blank()) +
  scale_fill_manual(values = rev(magma(9)), breaks = rev(IL_IN_breaks_cases_km_1),
                    name = "COVID Cases", drop = FALSE, labels = c('42324','32701','23863','15029','7736','3288','1256','267','0'),
                    guide = guide_legend(keyheight = unit(2, units = "mm"),
                                         keywidth = unit(8 / length(labels), units = "mm"))) +
  facet_wrap('Day')


#Indy_cases_map
IL_IN_cases_map

#----------------Plotting Illinois Key Days-------------------------
shp_cases_pop$Day <- as.Date(shp_cases_pop$Day)
key_days_IL <- shp_cases_pop %>% filter(STATEFP == "17")
key_days_IL$Day <- as.Date(strptime(key_days_IL$Day,"%Y-%m-%d"))
key_days_IL_plot <- ggplot(key_days_IL, aes(x = Day, y = cases)) + geom_col() +
  ggtitle("Illinois COVID-19 Cases: January 24th - May 3rd") +
  labs(x = "Date", y = "Total Cases")
key_days_IL_plot
key_days_lm <- lm(cases ~ Day, data = key_days_IL)

#------------Plotting Indiana Key Days-----------------
key_days_IN <- shp_cases_pop %>% filter(STATEFP == "18" & Day >= "2020-03-06")
key_days_IN$Day <- as.Date(strptime(key_days_IN$Day,"%Y-%m-%d"))
key_days_IN_plot <- ggplot(key_days_IN, aes(x = Day, y = cases)) + geom_col() +
  ggtitle("Indiana COVID-19 Cases: March 6th - May 3rd") +
  labs(x = "Date", y = "Total Cases") #+  scale_x_date(date_breaks = "7 days")
key_days_IN_plot

#---------Plotting trends of cases in top 10 counties-----------------
# Determine the 10 counties with the most cases 
IN_top10_cases <- shp_cases_pop_IN %>% filter(Day = "2020-04-20") %>% arrange(desc(cases))
IN_top10_cases <- IN_top10_cases[1:10,]
IN_top10_cases <- st_drop_geometry(IN_top10_cases)
#Join with IN cases
IN_top10_cases <- IN_top10_cases %>% left_join(shp_cases_pop_IN, by = "NAME") %>% select("NAME","Day.y","cases.y")
trend_top10 <- ggplot(data = IN_top10_cases, aes(x = Day.y, y = cases.y, color = NAME)) + geom_line(aes(group = NAME))
trend_top10
