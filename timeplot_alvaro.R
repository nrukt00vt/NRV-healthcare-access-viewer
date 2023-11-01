library(sf)
library(tidyverse)
library(ggplot2)


#Read in the shapefile as an "sf" object
#shapefile: contain special data needed to plot maps
#read_sf: will read simple features, including shapefile
#dsn: data source name
#layer: combination of data, stat and geometry with a potential position adjustment
shapefile = read_sf(dsn ="base_files", layer= "tl_2022_51_bg")

#read.csv: will read a certain file, data input
health_POIs = read.csv('HealthPOIs_Montgomery_VA 2.csv')

#Read in the time series data
time_data = read.csv("healthcarevisits_VA.csv")
#remove records where nobody visited the corresponding healthcare facility
#subset: will indicate which rows to keep
#is.na: not available (since ! is before it would be available)
time_data = subset(time_data, !is.na(time_data$visitor_home_cbg))
#merge the dataset with the locations & types of the healthcare facilities
time_data_merged = merge(time_data,health_POIs,
by.x="safegraph_place",by.y="safegraph_place_id")


#Aggregate by NAICS code; this means we will sum up all the different doctors for each NAICS code to get one result per NAICS code
# $ in between is the data set's specified field.
# FUN: function will be a sum.
#Aggregate: get the summary stats of the data group
NAICS_aggregate = aggregate(time_data_merged$number , 
by=list(time_data_merged$date,time_data_merged$naics_code), FUN = sum)

# these declare the names of each data set into vectors
names(NAICS_aggregate) = c("date","NAICS","num")

#Here, we will normalize the values per NAICS code. 
#If we tried to plot them without doing this, we would have some numbers way greater than others 

#For example, the average number of people visiting NAICS 621111 (physicians) is 1-2K, 
#while the average number visiting 621340 (physical/occupational/speech therapists) is more like 100

#If we don't normalize, we won't be able to see the therapists numbers because the physician numbers are so high.

#The way we are normalizing is by dividing each number by the median number of trips to that NAICS code; therefore the output number can be represented as "% of trips compared to median"
#2 means 2x as many trips as median, .5 means half as many, etc.
unique_codes = unique(NAICS_aggregate$NAICS)
NAICS_data = data.frame()
for (code in unique_codes){
  sub_data = subset(NAICS_aggregate, NAICS == code)
  sub_data$num_normalized = sub_data$num / median(sub_data$num)
  NAICS_data = rbind(NAICS_data, sub_data)
}

#Plotting
NAICS_data$date = as.Date(NAICS_data$date)

NAICS_data$NAICS = as.factor(NAICS_data$NAICS)

ggplot() + geom_line(data=NAICS_data, mapping = aes(x=date,
y = num_normalized, colour = NAICS , group = NAICS)) + scale_colour_brewer(palette="Set1")

# A few ideas pop to mind regarding the plot as a whole
# Could write a line of code that returns the lowest NAICS value, which "could" determine when people stopped going to each place
# With that line of could, an inference could be determined about whether people stopped going to each place at the same time or not.
mini <- NAICS_data$num_normalized

# This is just a start ill try to figure something out for this over the week.
