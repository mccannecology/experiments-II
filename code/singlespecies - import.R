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

# remove outliers 
data_raw <- data_raw[data_raw$outlier==0,]

# add a new variable that combines plate and well to use as an ID 
data_raw$id <- paste(data_raw$plate,data_raw$row,data_raw$col,sep="")

# look at the class of your different treatments 
class(data_raw$species)
class(data_raw$Temp)
class(data_raw$Nutr)

# make temperatuer a factor 
data_raw$Temp <- as.factor(data_raw$Temp)

# move turion data to its own data frame 
data_turion <- cbind(data_raw[,1:6], data_raw[,20:25], data_raw[,27:33])
head(data_turion)

# add turions_per_day
data_turion$turions_per_day <- data_turion$turionsTOT / 12
data_turion$turion_area_per_day <- data_turion$turions_areaTOT / 12

############################################
# Convert turion number to turion area 
############################################
# I couldn't get this to work, so I did it in the Excel
#data_turion$turions3_area[data_turion$species=="WB" & data_turion$turions3>0] <- (0.183589*data_turion$turions3-0.001849)
#data_turion$turions3_area[data_turion$species=="SP" & data_turion$turions3>0] <- (data_turion$turions3 * 2.793108)
#data_turion$turions3_area[data_turion$turions3==0 | data_turion$species == "LM"] <- 0
#data_turion$turions3_area


#############################################
# add additional variables to the dataframe #  
#############################################
# final area - initial area 
data_raw$final_minus_initial <- data_raw$area12 - data_raw$area0

# final area / initial area 
data_raw$final_divide_initial <- data_raw$area12 / data_raw$area0

# RGR 
# don't need this - I calculated this in Excel already
# data_raw$rgr1.5 <- (log(data_raw$area3)-log(data_raw$area0))/3
# data_raw$rgr4 <- (log(data_raw$area5)-log(data_raw$area3))/2
# data_raw$rgr6 <- (log(data_raw$area7)-log(data_raw$area5))/2
# data_raw$rgr8.5 <- (log(data_raw$area10)-log(data_raw$area7))/3
# data_raw$rgr11 <- (log(data_raw$area12)-log(data_raw$area10))/2

# RGR max - within each replicate
# don't need this - I calculated this in Excel already
# data_raw$maxRGR <- ave(data_raw$rgr1.5, data_raw$rgr4,data_raw$rgr6,data_raw$rgr8.5,data_raw$rgr11,FUN=max,na.rm=T)

# RGR avg - within each replicate
# don't need this - I calculated this in Excel already
# data_raw$avgRGR <- ave(data_raw$rgr1.5, data_raw$rgr4,data_raw$rgr6,data_raw$rgr8.5,data_raw$rgr11,FUN=mean,na.rm=T)

# re-order my treatments so they go from low to high
data_raw$Nutr <- factor(data_raw$Nutr , levels=c("low","med","high"))

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
data_area$final_minus_initial <- NULL
data_area$final_divide_initial <- NULL
data_area$rgr1.5 <- NULL
data_area$rgr4 <- NULL
data_area$rgr6 <- NULL
data_area$rgr8.5 <- NULL
data_area$rgr11 <- NULL
data_area$maxRGR <- NULL
data_area$avgRGR <- NULL

# check it out 
head(data_area)

# check the class of "area"
class(data_area$area)
data_area$area <- as.numeric(data_area$area)
data_area$area

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
data_rgr$final_minus_initial <- NULL
data_rgr$final_divide_initial <- NULL
data_rgr$area0 <- NULL
data_rgr$area3 <- NULL
data_rgr$area5 <- NULL
data_rgr$area7 <- NULL
data_rgr$area10 <- NULL
data_rgr$area12 <- NULL
data_rgr$maxRGR <- NULL
data_rgr$avgRGR <- NULL

# check it out 
head(data_rgr)

# check the class of "rgr"
class(data_rgr$rgr)

#######################
# Mean area           #
# by treatment combo  #
# Use for plotting    #
#######################
# Area
summary_data_area <- ddply(data_area, 
                           c("species","Temp","Nutr","day"), 
                           summarise, 
                           N = length(area),
                           mean = mean(area),
                           sd = sd(area),
                           se = sd / sqrt(N) )
colnames(summary_data_area)[6] <- "area"
head(summary_data_area)

summary_data_area$Nutr <- factor(summary_data_area$Nutr , levels=c("low","med","high"))

# check the class of "area"
class(summary_data_area$area)

#######################
# Mean rgr            #
# by treatment combo  #
# Use for plotting    #
#######################
# rgr
summary_data_rgr <- ddply(data_rgr, 
                          c("species","Temp","Nutr","day"), 
                          summarise, 
                          N = length(rgr),
                          mean = mean(rgr),
                          sd = sd(rgr),
                          se = sd / sqrt(N) )
colnames(summary_data_rgr)[6] <- "rgr"
head(summary_data_rgr)

summary_data_rgr$Nutr <- factor(summary_data_rgr$Nutr , levels=c("low","med","high"))

#######################
# Mean maxRGR        #
# by treatment combo  #
# Use for plotting    #
#######################
# maxRGR
summary_data_maxRGR <- ddply(data_raw, 
                             c("species","Temp","Nutr"), 
                             summarise, 
                             N = length(maxRGR),
                             mean = mean(maxRGR),
                             sd = sd(maxRGR),
                             se = sd / sqrt(N) )
colnames(summary_data_maxRGR)[5] <- "maxRGR"
head(summary_data_maxRGR)
summary_data_maxRGR$Nutr <- factor(summary_data_maxRGR$Nutr , levels=c("low","med","high"))

#######################
# Mean avgRGR        #
# by treatment combo  #
# Use for plotting    #
#######################
# maxRGR
summary_data_avgRGR <- ddply(data_raw, 
                             c("species","Temp","Nutr"), 
                             summarise, 
                             N = length(avgRGR),
                             mean = mean(avgRGR),
                             sd = sd(avgRGR),
                             se = sd / sqrt(N) )
colnames(summary_data_avgRGR)[5] <- "avgRGR"
head(summary_data_avgRGR)
summary_data_avgRGR$Nutr <- factor(summary_data_avgRGR$Nutr , levels=c("low","med","high"))

############################
# Mean final_minus_initial #
# by treatment combo       #
# Use for plotting         #
############################
# final_minus_initial
# Area day 12 - Area day 0
summary_data_final_minus_initial <- ddply(data_raw, 
                                          c("species","Temp","Nutr"), 
                                          summarise, 
                                          N = length(final_minus_initial),
                                          mean = mean(final_minus_initial),
                                          sd = sd(final_minus_initial),
                                          se = sd / sqrt(N) )
colnames(summary_data_final_minus_initial)[5] <- "final_minus_initial"
head(summary_data_final_minus_initial)
summary_data_final_minus_initial$Nutr <- factor(summary_data_final_minus_initial$Nutr , levels=c("low","med","high"))

#############################
# Mean final_divide_initial #
# by treatment combo        #
# Use for plotting          #
#############################
# final_divide_initial
# Area day 12 / Area day 0
summary_data_final_divide_initial <- ddply(data_raw, 
                                           c("species","Temp","Nutr"), 
                                           summarise, 
                                           N = length(final_divide_initial),
                                           mean = mean(final_divide_initial),
                                           sd = sd(final_divide_initial),
                                           se = sd / sqrt(N) )
colnames(summary_data_final_divide_initial)[5] <- "final_divide_initial"
head(summary_data_final_divide_initial)
summary_data_final_divide_initial$Nutr <- factor(summary_data_final_divide_initial$Nutr , levels=c("low","med","high"))



#############################
# Mean turionsTOT           #
# by treatment combo        #
# Use for plotting          #
#############################
# turionsTOT
# Area day 12 / Area day 0
summary_data_turions <- ddply(data_turion, 
                              c("species","Temp","Nutr"), 
                               summarise, 
                               N = length(turionsTOT),
                               mean = mean(turionsTOT),
                               sd = sd(turionsTOT),
                               se = sd / sqrt(N) )
colnames(summary_data_turions)[5] <- "turionsTOT"
head(summary_data_turions)
summary_data_turions$Nutr <- factor(summary_data_turions$Nutr , levels=c("low","med","high"))

#############################
# Mean turions_per_day      #
# by treatment combo        #
# Use for plotting          #
#############################
# turionsTOT
# Area day 12 / Area day 0
summary_data_turions_per_day <- ddply(data_turion, 
                                      c("species","Temp","Nutr"), 
                                      summarise, 
                                      N = length(turions_per_day),
                                      mean = mean(turions_per_day),
                                      sd = sd(turions_per_day),
                                      se = sd / sqrt(N) )
colnames(summary_data_turions_per_day)[5] <- "turions_per_day"
head(summary_data_turions_per_day)
summary_data_turions_per_day$Nutr <- factor(summary_data_turions_per_day$Nutr , levels=c("low","med","high"))

#############################
# Mean turions_areaTOT      #
# by treatment combo        #
# Use for plotting          #
#############################
# turions_areaTOT
summary_data_turions_area <- ddply(data_turion, 
                              c("species","Temp","Nutr"), 
                              summarise, 
                              N = length(turions_areaTOT),
                              mean = mean(turions_areaTOT),
                              sd = sd(turions_areaTOT),
                              se = sd / sqrt(N) )
colnames(summary_data_turions_area)[5] <- "turions_areaTOT"
head(summary_data_turions_area)
summary_data_turions_area$Nutr <- factor(summary_data_turions_area$Nutr , levels=c("low","med","high"))

#############################
# Mean turion_area_per_day  #
# by treatment combo        #
# Use for plotting          #
#############################
# turion_area_per_day
summary_data_turion_area_per_day <- ddply(data_turion, 
                                      c("species","Temp","Nutr"), 
                                      summarise, 
                                      N = length(turion_area_per_day),
                                      mean = mean(turion_area_per_day),
                                      sd = sd(turion_area_per_day),
                                      se = sd / sqrt(N) )
colnames(summary_data_turion_area_per_day)[5] <- "turion_area_per_day"
head(summary_data_turion_area_per_day)
summary_data_turion_area_per_day$Nutr <- factor(summary_data_turion_area_per_day$Nutr , levels=c("low","med","high"))

