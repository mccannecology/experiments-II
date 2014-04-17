##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Importing data                         #
# Do all calculations with R             #
# Reads "singlespecies_area_raw.csv"     #
##########################################
library(plyr)

data_raw <- read.csv("singlespecies_area_raw.csv") # import area data 

# add a new variable that combines plate and well to use as an ID 
data_raw$id <- paste(data_raw$Plate,data_raw$Row,data_raw$col,sep="")

#############################################
# add additional variables to the dataframe #  
#############################################
# final area - initial area 
data_raw$finalminusinitial <- data_raw$area12 - data_raw$area0

# final area / initial area 
data_raw$finaldivideinitial <- data_raw$area12 / data_raw$area0

# RGR 
data_raw$rgr1.5 <- (log(data_raw$area3)-log(data_raw$area0))/3
data_raw$rgr4 <- (log(data_raw$area5)-log(data_raw$area3))/2
data_raw$rgr6 <- (log(data_raw$area7)-log(data_raw$area5))/2
data_raw$rgr8.5 <- (log(data_raw$area10)-log(data_raw$area7))/3
data_raw$rgr11 <- (log(data_raw$area12)-log(data_raw$area10))/2

# RGR max - within each replicate
data_raw$rgr_max <- ave(data_raw$rgr1.5, data_raw$rgr4,data_raw$rgr6,data_raw$rgr8.5,data_raw$rgr11,FUN=max,na.rm=T)

# RGR avg - within each replicate
data_raw$rgr_avg <- ave(data_raw$rgr1.5, data_raw$rgr4,data_raw$rgr6,data_raw$rgr8.5,data_raw$rgr11,FUN=mean,na.rm=T)

head(data_raw)

############################# 
# reshape data              #
# area data                 #
# repeated measures (days)  #
# occur as separate rows    #
#############################
# new variable headings be day and area 
data_area <- reshape(data_raw, 
                        idvar="id",
                        varying = c("area0","area3","area5","area7","area10","area12"),
                        times = c(0,3,5,7,10,12),
                        timevar = "day",
                        v.names = "area",
                        direction = "long")

# clean-up
row.names(data_area) <- seq(nrow(data_area)) # Re-name the rows so they're not so ugly

# check it out 
head(data_area)

# re-order my treatments so they go from low to high
data_area$Nutr <- factor(data_area$Nutr , levels=c("low","med","high"))

# remove unwanted columns 
data_area$finalminusinitial <- NULL
data_area$finaldivideinitial <- NULL
data_area$rgr1.5 <- NULL
data_area$rgr4 <- NULL
data_area$rgr6 <- NULL
data_area$rgr8.5 <- NULL
data_area$rgr11 <- NULL
data_area$rgr_max <- NULL
data_area$rgr_avg <- NULL

# check it out 
head(data_area)

############################# 
# reshape data              #
# rgr data                  #
# repeated measures (days)  #
# occur as separate rows    #
#############################
# new variable headings be day and area 
data_rgr <- reshape(data_raw, 
                     idvar="id",
                     varying = c("rgr1.5","rgr4","rgr6","rgr8.5","rgr11"),
                     times = c(1.5,4,6,8.5,11),
                     timevar = "day",
                     v.names = "rgr",
                     direction = "long")

# clean-up
row.names(data_rgr) <- seq(nrow(data_rgr)) # Re-name the rows so they're not so ugly

# check it out 
head(data_rgr)

# re-order my treatments so they go from low to high
data_rgr$Nutr <- factor(data_rgr$Nutr , levels=c("low","med","high"))

# remove unwanted columns 
data_rgr$finalminusinitial <- NULL
data_rgr$finaldivideinitial <- NULL
data_rgr$area0 <- NULL
data_rgr$area3 <- NULL
data_rgr$area5 <- NULL
data_rgr$area7 <- NULL
data_rgr$area10 <- NULL
data_rgr$area12 <- NULL
data_rgr$rgr_max <- NULL
data_rgr$rgr_avg <- NULL

# check it out 
head(data_rgr)

#######################
# Mean area           #
# by treatment combo  #
# Use for plotting    #
#######################
# Area
summary_data_area <- ddply(data_area, c("species","Temp","Nutr","day"), summarise, 
                           N = length(area),
                           mean = mean(area),
                           sd = sd(area),
                           se = sd / sqrt(N) )
colnames(summary_data_area)[6] <- "area"
head(summary_data_area)

summary_data_area$Nutr <- factor(summary_data_area$Nutr , levels=c("low","med","high"))

#######################
# Mean rgr            #
# by treatment combo  #
# Use for plotting    #
#######################
# rgr
summary_data_rgr <- ddply(data_rgr, c("species","Temp","Nutr","day"), summarise, 
                           N = length(rgr),
                           mean = mean(rgr),
                           sd = sd(rgr),
                           se = sd / sqrt(N) )
colnames(summary_data_rgr)[6] <- "rgr"
head(summary_data_rgr)

summary_data_rgr$Nutr <- factor(summary_data_rgr$Nutr , levels=c("low","med","high"))

#######################
# Mean rgr_max        #
# by treatment combo  #
# Use for plotting    #
#######################
# rgr_max
summary_data_rgr_max <- ddply(data_raw, c("species","Temp","Nutr"), summarise, 
                          N = length(rgr_max),
                          mean = mean(rgr_max),
                          sd = sd(rgr_max),
                          se = sd / sqrt(N) )
colnames(summary_data_rgr_max)[5] <- "rgr_max"
head(summary_data_rgr_max)
summary_data_rgr_max$Nutr <- factor(summary_data_rgr_max$Nutr , levels=c("low","med","high"))

#######################
# Mean rgr_avg        #
# by treatment combo  #
# Use for plotting    #
#######################
# rgr_max
summary_data_rgr_avg <- ddply(data_raw, c("species","Temp","Nutr"), summarise, 
                              N = length(rgr_avg),
                              mean = mean(rgr_avg),
                              sd = sd(rgr_avg),
                              se = sd / sqrt(N) )
colnames(summary_data_rgr_avg)[5] <- "rgr_avg"
head(summary_data_rgr_avg)
summary_data_rgr_avg$Nutr <- factor(summary_data_rgr_avg$Nutr , levels=c("low","med","high"))

##########################
# Mean finalminusinitial #
# by treatment combo     #
# Use for plotting       #
##########################
# finalminusinitial
# Area day 12 - Area day 0
summary_data_finalminusinitial <- ddply(data_raw, c("species","Temp","Nutr"), summarise, 
                          N = length(finalminusinitial),
                          mean = mean(finalminusinitial),
                          sd = sd(finalminusinitial),
                          se = sd / sqrt(N) )
colnames(summary_data_finalminusinitial)[5] <- "finalminusinitial"
head(summary_data_finalminusinitial)

summary_data_finalminusinitial$Nutr <- factor(summary_data_finalminusinitial$Nutr , levels=c("low","med","high"))

###########################
# Mean finaldivideinitial #
# by treatment combo      #
# Use for plotting        #
###########################
# finaldivideinitial
# Area day 12 / Area day 0
summary_data_finaldivideinitial <- ddply(data_raw, c("species","Temp","Nutr"), summarise, 
                                        N = length(finaldivideinitial),
                                        mean = mean(finaldivideinitial),
                                        sd = sd(finaldivideinitial),
                                        se = sd / sqrt(N) )
colnames(summary_data_finaldivideinitial)[5] <- "finalminusinitial"
head(summary_data_finaldivideinitial)

summary_data_finaldivideinitial$Nutr <- factor(summary_data_finaldivideinitial$Nutr , levels=c("low","med","high"))












################ old stuff done here 


# calculate rgr 
rgr1 <- (log(subset(data_area$area_mm2,data_area$day==2))-log(subset(data_area$area_mm2,data_area$day==0)))/2
rgr3 <- (log(subset(data_area$area_mm2,data_area$day==4))-log(subset(data_area$area_mm2,data_area$day==2)))/2
rgr5 <- (log(subset(data_area$area_mm2,data_area$day==6))-log(subset(data_area$area_mm2,data_area$day==4)))/2
rgr7 <- (log(subset(data_area$area_mm2,data_area$day==8))-log(subset(data_area$area_mm2,data_area$day==6)))/2
rgr9 <- (log(subset(data_area$area_mm2,data_area$day==10))-log(subset(data_area$area_mm2,data_area$day==8)))/2


# add standardized area 
initial <- subset(data_area$area_mm2,data_area$day == 0) # creates a vector of initial areas 
data_area$area_stand <- data_area$area_mm2 / initial # divide area_mm2 by that initial area & create a new variable
rm(initial) # cleanup your environment


# combine each day's rgr into a single vector
rgr <- c(rgr1,rgr3,rgr5,rgr7,rgr9)

# Calculate density-standardized RGR
# make a temporary data frame with all rows with species = TOT 
temptotals <- subset(data_area,data_area$species=="TOT")
temptotals <- temptotals[,c("id","day","area_mm2")]
colnames(temptotals)[3] <- "area_total"
temptotals
                 
# merge the temporary data frame holding your total areas with the original data_area data frame 
data_area <- merge(data_area,temptotals,by=c("id","day"))

# clean up 
data_area <- data_area[with(data_area, order(day,id)),] # re-order - by id and day
row.names(data_area) <- seq(nrow(data_area)) # Re-name the rows so they're not so ugly

# clean up your environment
rm(temptotals)

# total area of a well plate
diameter <- 35 
wellarea <- pi*(diameter/2)^2

# this standardized by the density of all species in the previous time step
# I'm not sure if this standardization makes any sense at all 
# currently, if you have high RGR and high density you get a smaller # than if you have the same RGR and low density -- this seems wrong!
rgr1_stand <- rgr1/(subset(data_area$area_total,data_area$day==0)/wellarea)
rgr3_stand <- rgr3/(subset(data_area$area_total,data_area$day==2)/wellarea)
rgr5_stand <- rgr5/(subset(data_area$area_total,data_area$day==4)/wellarea)
rgr7_stand <- rgr7/(subset(data_area$area_total,data_area$day==6)/wellarea)
rgr9_stand <- rgr9/(subset(data_area$area_total,data_area$day==8)/wellarea)

# combine each day's density-standarized rgr into a single vector
rgr_stand  <- c(rgr1_stand,rgr3_stand,rgr5_stand,rgr7_stand,rgr9_stand)

# set-up the dataframe to hold the rgrs
data_rgr <- subset(data_area, data_area$day != 10) # exclude day 10
data_rgr$day <- data_rgr$day+1 # add one to day so it corresponds to the midpoint for RGR calculations 
data_rgr <- data_rgr[-11] # get rid of columns you don't need 
data_rgr <- data_rgr[-10]
data_rgr <- data_rgr[-9]
data_rgr <- data_rgr[-8]

# add the rgr and rgr_stand vectors the the data frame 
data_rgr$rgr <- rgr
data_rgr$rgr_stand <- rgr_stand

# cleanup your environment
rm(list = c("rgr","rgr1","rgr3","rgr5","rgr7","rgr9","rgr_stand","rgr1_stand","rgr3_stand","rgr5_stand","rgr7_stand","rgr9_stand","diameter","wellarea"))

# add a second id that is specific to a species in the well 
data_area$id2 <- paste(data_area$id,data_area$species,sep="")
data_rgr$id2 <- paste(data_rgr$id,data_rgr$species,sep="")

#######################
# Means and variances #
# Use for plotting    #
#######################
# Area
summary_data_area <- ddply(data_area, c("nutrients","treatment","species","day"), summarise, 
                           N = length(area_mm2),
                           mean = mean(area_mm2),
                           sd = sd(area_mm2),
                           se = sd / sqrt(N) )
colnames(summary_data_area)[6] <- "area"

# Standardized area (Current/Initial)
summary_data_area_stand <- ddply(data_area, c("nutrients","treatment","species","day"), summarise, 
                                 N = length(area_stand),
                                 mean = mean(area_stand),
                                 sd = sd(area_stand),
                                 se = sd / sqrt(N) )
colnames(summary_data_area_stand)[6] <- "area_stand"

# Relative % composition
summary_data_comp_rel <- ddply(data_area, c("nutrients","treatment","species","day"), summarise, 
                               N = length(comp_rel),
                               mean = mean(comp_rel),
                               sd = sd(comp_rel),
                               se = sd / sqrt(N) )
colnames(summary_data_comp_rel)[6] <- "comp_rel"

# Relative growth rate 
summary_data_rgr <- ddply(data_rgr, c("nutrients","treatment","species","day"), summarise, 
                          N = length(rgr),
                          mean = mean(rgr),
                          sd = sd(rgr),
                          se = sd / sqrt(N) )
colnames(summary_data_rgr)[6] <- "rgr"

# Relative growth rate - same as above, but organized by each species in each well and finds the maximum too 
summary_data_rgr02 <- ddply(data_rgr, c("nutrients","treatment","species","id2"), summarise, 
                            N = length(rgr),
                            mean = mean(rgr),
                            max = max(rgr),
                            sd = sd(rgr),
                            se = sd / sqrt(N) )
summary_data_rgr02 <- subset(summary_data_rgr02, summary_data_rgr02$species != "TOT")

# Relative growth rate - takes the mean of the mean growth rates by species in each well 
summary_data_meanrgr <- ddply(summary_data_rgr02, c("nutrients","treatment","species"), summarise,
                              N = length(mean),
                              mean2 = mean(mean),
                              sd = sd(mean),
                              se = sd / sqrt(N) )
colnames(summary_data_meanrgr)[5] <- "average_RGR"

# Relative growth rate - the mean of the maximum growth rates by species in each well 
summary_data_maxrgr <- ddply(summary_data_rgr02, c("nutrients","treatment","species"), summarise,
                             N = length(max),
                             max2 = mean(max),
                             sd = sd(max),
                             se = sd / sqrt(N) )
colnames(summary_data_maxrgr)[5] <- "maximum_RGR"

# Relative growth rate - standardized by density
summary_data_rgr_stand <- ddply(data_rgr, c("nutrients","treatment","species","day"), summarise, 
                          N = length(rgr_stand),
                          mean = mean(rgr_stand),
                          sd = sd(rgr_stand),
                          se = sd / sqrt(N) )
colnames(summary_data_rgr_stand)[6] <- "rgr_stand"

# average the Relative growth rate - standardized by density
summary_data_avg_rgr_stand <- ddply(summary_data_rgr_stand, c("nutrients","treatment","species"), summarise, 
                                N = length(rgr_stand),
                                mean = mean(rgr_stand),
                                sd = sd(rgr_stand),
                                se = sd / sqrt(N) )
colnames(summary_data_avg_rgr_stand)[5] <- "rgr_stand"




