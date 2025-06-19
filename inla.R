install.packages("INLA",repos=c(getOption("repos"),INLA="https://inla.r-inla-download.org/R/stable"), dep=TRUE) 
#install.packages("corrplot")
library(corrplot)
library(sp)
library(INLA)
library(spdep)
library(stats)
library(tidyverse)
library(dplyr)
library(ggridges)

#read in geometry units and covariate data

shapefile = read_sf(dsn ="~/Downloads/tl_2019_51_bg", layer= "tl_2019_51_bg")
input_data <- read.csv("NRHDcovariate.csv")

#Then, read in new data
apr_2020_data = read.csv("april_2020_data.csv")[,-1]


#Attach new data to input_data
input_data = merge(input_data,apr_2020_data,by.x = "cbg2019",by.y="GEOID")
#Then, read in new data
jul_2020_data = read.csv("july_2020_data.csv")[,-1]
#Attach new data to input_data
input_data = merge(input_data,jul_2020_data,by.x = "cbg2019",by.y="GEOID")

#list covariate and responce variable column names
covariates <- c('Median.age..years.', 'Sex.ratio..males.per.100.females.', 'X65_and_over', 
                "Hispanic.or.Latino", "Bachelor.s.degree.or.higher", "Income.below..25.000", 
                 "prop_urban", "any_black", "White")
responce_var <- c("change_apr_2020", "change_jul_2020")


#------------------- INLA model --------------------------------------------------------------------------------
# Specify the adjacency matrix
shapefile_temp <- poly2nb(shapefile) #construct a temporary shapefile with a neighbor list
nb2INLA("shapefile.graph", shapefile_temp) # create the adjacency matrix in INLA format
shapefile.adj <- paste(getwd(),"/shapefile.graph",sep="") # name the object and save it

#set options
inla.setOption(scale.model.default = F) #Set and get global options for INLA ####QUESTION: what does this line do
H <- inla.read.graph(filename = "shapefile.graph")  #take graph created above nd store as variable H

#creating the INLA model
shapefile$ID = 1:dim(shapefile)[1] #Create new ID column
shapefile_ids = st_drop_geometry(shapefile[,c("GEOID","ID")]) #create a data frame with only GEOID and ID columns #@ acesses data information in shpaefile
input_data = merge(input_data,shapefile_ids, by.x = 'cbg2019', by.y = 'GEOID') #combine data frames

# Plot adjacency matrix
image(inla.graph2matrix(H), xlab = "", ylab = "")

#set formula for spatial model
spatial_formula_april <- change_apr_2020 ~ 1 + Median.age..years. + 
  Sex.ratio..males.per.100.females. + X65_and_over +  Hispanic.or.Latino +
  Bachelor.s.degree.or.higher + Income.below..25.000 + prop_urban+ 
  any_black + White +
  f(ID, model = "bym",graph = shapefile.adj)      

#set formula for non spatial model
nonspatial_formula_april <- change_apr_2020 ~ 1 + Median.age..years. + 
  Sex.ratio..males.per.100.females. + X65_and_over +  Hispanic.or.Latino +
  Bachelor.s.degree.or.higher + Income.below..25.000 + prop_urban+ 
  any_black + White +
  f(ID, model = "iid",graph = shapefile.adj) 

#set formula for spatial model
spatial_formula_july <- change_jul_2020 ~ 1 + Median.age..years. + 
  Sex.ratio..males.per.100.females. + X65_and_over +  Hispanic.or.Latino +
  Bachelor.s.degree.or.higher + Income.below..25.000 + prop_urban+ 
  any_black + White +
  f(ID, model = "bym",graph = shapefile.adj)      

#set formula for non spatial model
nonspatial_formula_july <- change_jul_2020 ~ 1 + Median.age..years. + 
  Sex.ratio..males.per.100.females. + X65_and_over +  Hispanic.or.Latino +
  Bachelor.s.degree.or.higher + Income.below..25.000 + prop_urban+ 
  any_black + White +
  f(ID, model = "iid",graph = shapefile.adj)    

#run INLA model
#spatial model april
spatial_result_april<-inla(spatial_formula_april,data=input_data,family = "gaussian", 
             control.compute=list(dic=TRUE,cpo=TRUE))
summary(spatial_result_april)

#non spatial model April
non_spatial_result_april<-inla(nonspatial_formula_april,data=input_data,family = "gaussian",
             control.compute=list(dic=TRUE,cpo=TRUE))
summary(non_spatial_result_april)

#spatial model july
spatial_result_july<-inla(spatial_formula_july,data=input_data,family = "gaussian", 
                           control.compute=list(dic=TRUE,cpo=TRUE))
summary(spatial_result_july)

#non spatial model july
non_spatial_result_july<-inla(nonspatial_formula_july,data=input_data,family = "gaussian",
                               control.compute=list(dic=TRUE,cpo=TRUE))
summary(non_spatial_result_july)

change_df = input_data[,c("cbg2019","change_apr_2020","change_jul_2020")]

shapefile = merge(shapefile,change_df, by.x = "GEOID", by.y = "cbg2019")

shapefile$april_predicted = spatial_result_april$summary.fitted.values$mean
shapefile$july_predicted = spatial_result_july$summary.fitted.values$mean

shapefile$april_predicted_nonspatial = non_spatial_result_april$summary.fitted.values$mean
shapefile$july_predicted_nonspatial = non_spatial_result_july$summary.fitted.values$mean

#------------------- Change plots w/ covariates --------------------------------------------------------------------------------

plot(shapefile$change_apr_2020,shapefile$april_predicted,xlab="Actual movement, April 2020", ylab="Predicted movement, April 2020")
shapefile_data_only = subset(shapefile, !is.na(shapefile$change_apr_2020))
cor(shapefile_data_only$change_apr_2020,shapefile_data_only$april_predicted)


plot(shapefile$change_jul_2020,shapefile$july_predicted,xlab="Actual movement, July 2020", ylab="Predicted movement, July 2020")
shapefile_data_only = subset(shapefile, !is.na(shapefile$change_jul_2020))
cor(shapefile_data_only$change_jul_2020,shapefile_data_only$july_predicted)


plot(shapefile$change_apr_2020,shapefile$april_predicted_nonspatial)
shapefile_data_only = subset(shapefile, !is.na(shapefile$change_apr_2020))
cor(shapefile_data_only$change_apr_2020,shapefile_data_only$april_predicted_nonspatial)


plot(shapefile$change_jul_2020,shapefile$july_predicted)
shapefile_data_only = subset(shapefile, !is.na(shapefile$change_jul_2020))
cor(shapefile_data_only$change_jul_2020,shapefile_data_only$july_predicted)

ggplot() + geom_sf(data = shapefile, mapping = aes(fill = change_apr_2020),colour="NA") + 
  scale_fill_gradient2(low = "#2166ac",mid="#f7f7f7",high="#b2182b" ,midpoint = 1, name="% of baseline",labels = scales::percent_format())+
  ggtitle("Observed mobility, April 2020") + theme_void()

ggplot() + geom_sf(data = shapefile, mapping = aes(fill = change_jul_2020),colour="NA")  + 
  scale_fill_gradient2(low = "#2166ac",mid="#f7f7f7",high="#b2182b" ,midpoint = 1, name="% of baseline",labels = scales::percent_format())+
  ggtitle("Observed mobility, July 2020") + theme_void()

ggplot() + geom_sf(data = shapefile, mapping = aes(fill = april_predicted ),colour="NA")  + 
  scale_fill_gradient2(low = "#2166ac",mid="#f7f7f7",high="#b2182b" ,midpoint = 1, name="% of baseline",labels = scales::percent_format())+
  ggtitle("Predicted mobility, April 2020") + theme_void()
ggplot() + geom_sf(data = shapefile, mapping = aes(fill = july_predicted),colour="NA")  + 
  scale_fill_gradient2(low = "#2166ac",mid="#f7f7f7",high="#b2182b" ,midpoint = 1, name="% of baseline",labels = scales::percent_format())+
  ggtitle("Predicted mobility, July 2020") + theme_void()

shapefile$april_diff = shapefile$april_predicted - shapefile$change_apr_2020 
shapefile$july_diff = shapefile$july_predicted - shapefile$change_jul_2020 

ggplot() + geom_sf(data = shapefile, mapping = aes(fill = april_diff ),colour="NA")  + 
  scale_fill_gradient2(low = "#b35806",mid="#f7f7f7",high="#542788" ,midpoint = 0, name="Pred - obs",labels = scales::percent_format())+
  ggtitle("Predicted - observed mobility, April 2020") + theme_void()
ggplot() + geom_sf(data = shapefile, mapping = aes(fill = july_diff),colour="NA")  + 
  scale_fill_gradient2(low = "#b35806",mid="#f7f7f7",high="#542788" ,midpoint = 0, name="Pred - obs",labels = scales::percent_format())+
  ggtitle("Predicted - observed mobility, July 2020") + theme_void()
ggplot() + geom_sf(data = shapefile, mapping = aes(fill = july_predicted_nonspatial),colour="NA") + scale_fill_gradient2(low = "#2166ac",mid="#f7f7f7",high="#b2182b" ,midpoint = 1)

outdat = data.frame()

prepdat = data.frame(name = "Intercept", values=inla.rmarginal(10000, spatial_result_april$marginals.fixed$`(Intercept)`))
medage = data.frame(name = "Median Age", values=inla.rmarginal(10000, spatial_result_april$marginals.fixed$Median.age..years.))
sexrat = data.frame(name = "Sex Ratio (Males per 100 females)", values=inla.rmarginal(10000, spatial_result_april$marginals.fixed$Sex.ratio..males.per.100.females.))
older = data.frame(name = "% 65 and Older", values=inla.rmarginal(10000, spatial_result_april$marginals.fixed$X65_and_over))
hispanic =data.frame(name = "% Hispanic",values=inla.rmarginal(10000, spatial_result_april$marginals.fixed$Hispanic.or.Latino))
bachelors =  data.frame(name = "% Bachelors or greater",values=inla.rmarginal(10000, spatial_result_april$marginals.fixed$Bachelor.s.degree.or.higher))
income = data.frame(name = "% Income <25K",values=inla.rmarginal(10000, spatial_result_april$marginals.fixed$Income.below..25.000))
urban_april = data.frame(name = "% urbanized",values=inla.rmarginal(10000, spatial_result_april$marginals.fixed$prop_urban))
black = data.frame(name = "% Black",values=inla.rmarginal(10000, spatial_result_april$marginals.fixed$any_black))
white = data.frame(name = "% White",values=inla.rmarginal(10000, spatial_result_april$marginals.fixed$White))

all_data_april_numeric = rbind(medage,sexrat)
all_data_april_numeric$month = "April"
all_data_april_proportion = rbind(older,hispanic,bachelors,income,black,white,urban_april)
all_data_april_proportion$month = "April"
urban_april$month = "April"

prepdat = data.frame(name = "Intercept", values=inla.rmarginal(10000, spatial_result_july$marginals.fixed$`(Intercept)`))
medage = data.frame(name = "Median Age", values=inla.rmarginal(10000, spatial_result_july$marginals.fixed$Median.age..years.))
sexrat = data.frame(name = "Sex Ratio (Males per 100 females)", values=inla.rmarginal(10000, spatial_result_july$marginals.fixed$Sex.ratio..males.per.100.females.))
older = data.frame(name = "% 65 and Older", values=inla.rmarginal(10000, spatial_result_july$marginals.fixed$X65_and_over))
hispanic =data.frame(name = "% Hispanic",values=inla.rmarginal(10000, spatial_result_july$marginals.fixed$Hispanic.or.Latino))
bachelors =  data.frame(name = "% Bachelors or greater",values=inla.rmarginal(10000, spatial_result_july$marginals.fixed$Bachelor.s.degree.or.higher))
income = data.frame(name = "% Income <25K",values=inla.rmarginal(10000, spatial_result_july$marginals.fixed$Income.below..25.000))
urban_july = data.frame(name = "% urbanized",values=inla.rmarginal(10000, spatial_result_july$marginals.fixed$prop_urban))
black = data.frame(name = "% Black",values=inla.rmarginal(10000, spatial_result_july$marginals.fixed$any_black))
white = data.frame(name = "% White",values=inla.rmarginal(10000, spatial_result_july$marginals.fixed$White))

all_data_july_numeric = rbind(medage,sexrat)
all_data_july_numeric$month = "July"

all_data_july_proportion = rbind(older,hispanic,bachelors,income,black,white,urban_july)
urban_july$month = "July"
all_data_july_proportion$month = "July"
all_data_urban = rbind(urban_april,urban_july)

all_data_urban$group = paste(all_data_urban$name,all_data_urban$month)

all_data_proportion= rbind(all_data_april_proportion, all_data_july_proportion)
all_data_proportion$group = paste(all_data_proportion$name,all_data_proportion$month)

all_data_numeric= rbind(all_data_april_numeric, all_data_july_numeric)
all_data_numeric$group = paste(all_data_numeric$name,all_data_numeric$month)


ggplot() +  geom_density_ridges(data = all_data_proportion,mapping=aes(y=name,x=values,group=group, fill = month),alpha=0.6) + geom_vline(xintercept = 0, colour = "red") + theme_ridges() +
  scale_x_continuous(name="") + scale_y_discrete(name = "Variable") + scale_fill_discrete(name="Month")

ggplot() +  geom_density_ridges(data = all_data_numeric,mapping=aes(y=name,x=values,group=group, fill = month),alpha=0.6) + geom_vline(xintercept = 0, colour = "red") + theme_ridges() +
  scale_x_continuous(name="") + scale_y_discrete(name = "Variable") + scale_fill_discrete(name="Month")
ggplot() +  geom_density_ridges(data = all_data_numeric,mapping=aes(y=name,x=values,group=group, fill = month),alpha=0.6) + geom_vline(xintercept = 0, colour = "red") + theme_ridges() +
  scale_x_continuous(name="") + scale_y_discrete(name = "Variable") + scale_fill_brewer(name="Month")

ggplot() +  geom_density_ridges(data = all_data_urban,mapping=aes(y=name,x=values,group=group, fill = month),alpha=0.6) + geom_vline(xintercept = 0, colour = "red") + theme_ridges() +
  scale_x_continuous(name="") + scale_y_discrete(name = "Variable") + scale_fill_discrete(name="Month")


