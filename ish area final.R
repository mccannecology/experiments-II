##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# Final (day 12)                         #
##########################################
library(ggplot2)

# check out the data you will use
head(data_area)
head(summary_data_area)

# possible response variables
# Y = final area (day 12)
# Y = final area (day 12) / initial area (day 0)
# Y = final area (day 12) - initial area (day 0)

###########################
# Y = final area (day 12) #
# Raw data                #
###########################
# colour
raw_area_day12_plot <- ggplot(subset(data_area, data_area$day == 12), aes(x=species,y=area))
raw_area_day12_plot <- raw_area_day12_plot + geom_point() 
raw_area_day12_plot <- raw_area_day12_plot + facet_grid(Nutr ~ Temp)
raw_area_day12_plot <- raw_area_day12_plot + scale_x_discrete(breaks=c(0,3,5,7,10,12),labels=c(0,3,5,7,10,12))
raw_area_day12_plot <- raw_area_day12_plot + ylab("area (sq. mm")
raw_area_day12_plot 

###########################
# Y = final area (day 12) #
# Average                 #
###########################
# colour
mean_area_day12_plot <- ggplot(subset(summary_data_area, summary_data_area$day == 12), aes(x=species,y=area)) 
mean_area_day12_plot <- mean_area_day12_plot + geom_point() 
mean_area_day12_plot <- mean_area_day12_plot + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
mean_area_day12_plot <- mean_area_day12_plot + facet_grid(Nutr ~ Temp)
mean_area_day12_plot <- mean_area_day12_plot + scale_x_discrete(breaks=c(0,3,5,7,10,12),labels=c(0,3,5,7,10,12))
mean_area_day12_plot <- mean_area_day12_plot + ylab("area (sq. mm")
mean_area_day12_plot 

#################################
# Y = final area - initial area #       # still need to finish this 
# Raw data                      #
#################################

# colour
raw_finalminusinitial_day12_plot <- ggplot(data_area, aes(x=species,y=area))
raw_finalminusinitial_day12_plot <- raw_finalminusinitial_day12_plot + geom_point() 
raw_finalminusinitial_day12_plot <- raw_finalminusinitial_day12_plot + facet_grid(Nutr ~ Temp)
raw_finalminusinitial_day12_plot <- raw_finalminusinitial_day12_plot + scale_x_discrete(breaks=c(0,3,5,7,10,12),labels=c(0,3,5,7,10,12))
raw_finalminusinitial_day12_plot <- raw_finalminusinitial_day12_plot + ylab("area (sq. mm")
raw_finalminusinitial_day12_plot 

#################################
# Y = final area - initial area #       # still need to finish this 
# Average                       #
#################################
# colour
mean_finalminusinitial_day12_plot <- ggplot(subset(summary_data_area, summary_data_area$day == 12), aes(x=species,y=area)) 
mean_finalminusinitial_day12_plot <- mean_finalminusinitial_day12_plot + geom_point() 
mean_finalminusinitial_day12_plot <- mean_finalminusinitial_day12_plot + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
mean_finalminusinitial_day12_plot <- mean_finalminusinitial_day12_plot + facet_grid(Nutr ~ Temp)
mean_finalminusinitial_day12_plot <- mean_finalminusinitial_day12_plot + scale_x_discrete(breaks=c(0,3,5,7,10,12),labels=c(0,3,5,7,10,12))
mean_finalminusinitial_day12_plot <- mean_finalminusinitial_day12_plot + ylab("area (sq. mm")
mean_finalminusinitial_day12_plot 

