############################################
# Turion # to Turion area                  #
# Conversion                               #
# Data from Fall 2012 Stoichiometry exp.   #
############################################
library(plyr)
library(ggplot2)

# import data 
data_turion_area <- read.csv("turion_area-FOR_CONVERSION_ONLY.csv")
head(data_turion_area)

# add a new variable that combines plate and well to use as an ID 
data_turion_area$id <- paste(data_turion_area$plate,data_turion_area$row,data_turion_area$col,sep="")

# reshape the data 
a <- reshape(data_turion_area, 
             idvar="id",
             varying = c("turion_area5","turion_area7","turion_area10","turion_area12"),
             times = c(5,7,10,12),
             timevar = "day",
             v.names = "turion_AREA",
             direction = "long")

b <- reshape(data_turion_area, 
             idvar="id",
             varying = c("turions5","turions7","turions10","turions12"),
             times = c(5,7,10,12),
             timevar = "day",
             v.names = "turion_NUMB",
             direction = "long")

# remove un-needed variables 
a[9:28] <- list(NULL)
b[9:28] <- list(NULL)

# merge 
data_turion_area_long <- merge(a,b)

# clean-up
row.names(data_turion_area_long) <- seq(nrow(data_turion_area_long)) # Re-name the rows so they're not so ugly

# check it out 
head(data_turion_area_long)

# Wolffia
turion_area_WB <- ggplot(subset(data_turion_area_long, data_turion_area_long$species=="W"), aes(x=turion_NUMB,y=turion_AREA))
turion_area_WB <- turion_area_WB + geom_point(size=3)
turion_area_WB <- turion_area_WB + ggtitle("Wolffia brasiliensis")
turion_area_WB <- turion_area_WB + xlab("Number of turions")
turion_area_WB <- turion_area_WB + ylab("Turion area (sq. mm)")
turion_area_WB <- turion_area_WB + theme_bw(base_size=18)
turion_area_WB <- turion_area_WB + geom_smooth(method='lm')
turion_area_WB

# get the slope and intercept 
lm(turion_AREA ~ turion_NUMB, data=subset(data_turion_area_long, data_turion_area_long$species=="W"))
summary(lm(turion_AREA ~ turion_NUMB, data=subset(data_turion_area_long, data_turion_area_long$species=="W")))
# Intercept: -0.001849
# Slope: 0.183589 
# adj R sq: 0.9524

# Spirodela
turion_area_SP <- ggplot(subset(data_turion_area_long, data_turion_area_long$species=="S"), aes(x=turion_NUMB,y=turion_AREA))
turion_area_SP <- turion_area_SP + geom_point(size=3)
turion_area_SP <- turion_area_SP + ggtitle("Spirodela polyrhiza")
turion_area_SP <- turion_area_SP + xlab("Number of turions")
turion_area_SP <- turion_area_SP + ylab("Turion area (sq. mm)")
turion_area_SP <- turion_area_SP + theme_bw(base_size=18)
turion_area_SP <- turion_area_SP + geom_smooth(method='lm')
turion_area_SP

# get the subset of spirodela data where theere is a single turion 
single_spirodel_turions <- subset(data_turion_area_long, data_turion_area_long$species=="S" & data_turion_area_long$turion_NUMB==1)
# plot a histogram 
hist(single_spirodel_turions$turion_AREA)

# summary statistics
numb_area <- length(single_spirodel_turions$turion_AREA)
mean_area <- mean(single_spirodel_turions$turion_AREA) # 2.793108
sd_area <- sd(single_spirodel_turions$turion_AREA)
se_area <- sd_area/sqrt(numb_area)
error <- qnorm(0.975)*sd_area/sqrt(numb_area)
lower <- mean_area - error
upper <- mean_area + error
