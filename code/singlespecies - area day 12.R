##################################################
# Analysis of single duckweed experiment         #
# Conducted by Ish - Summer 2013                 #
#                                                #
# Plotting data                                  #
# Final area (day 12)                            #
#                                                #
# possible response variables                    #
# Y = final area (day 12)                        #  
# Y = final area (day 12) / initial area (day 0) #
# Y = final area (day 12) - initial area (day 0) #
##################################################
library(ggplot2)

# check out the data you will use
head(data_area)
head(summary_data_area)

###########################
# Y = final area (day 12) #
# Raw data                #
# Plot                    #
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
# Plot                    #
###########################
# colour
mean_area_day12_plot <- ggplot(subset(summary_data_area, summary_data_area$day == 12), aes(x=species,y=area)) 
mean_area_day12_plot <- mean_area_day12_plot + geom_point() 
mean_area_day12_plot <- mean_area_day12_plot + geom_errorbar(aes(ymin=area-se, ymax=area+se), width=0.1)
mean_area_day12_plot <- mean_area_day12_plot + facet_grid(Nutr ~ Temp)
mean_area_day12_plot <- mean_area_day12_plot + scale_x_discrete(breaks=c(0,3,5,7,10,12),labels=c(0,3,5,7,10,12))
mean_area_day12_plot <- mean_area_day12_plot + ylab("area (sq. mm")
mean_area_day12_plot 

###########################
# Y = final area (day 12) #
# Preliminary anova       #
# Treatments:             #
# species,                #
# nutrients,              #
# temperature,            #
# day                     #    
###########################
area_day12_anova <- aov(area ~ species*Temp*Nutr, data=subset(data_area, data_area$day == 12))
summary(area_day12_anova)
TukeyHSD(area_day12_anova)


#################################
# Y = final area - initial area #       
# Raw data                      #
# Plot                          #
#################################
# colour
raw_finalminusinitial_day12_plot <- ggplot(data_raw, aes(x=species,y=finalminusinitial))
raw_finalminusinitial_day12_plot <- raw_finalminusinitial_day12_plot + geom_point() 
raw_finalminusinitial_day12_plot <- raw_finalminusinitial_day12_plot + facet_grid(Nutr ~ Temp)
raw_finalminusinitial_day12_plot <- raw_finalminusinitial_day12_plot + ylab("final area - initial area (sq. mm)")
raw_finalminusinitial_day12_plot 

#################################
# Y = final area - initial area #      
# Average                       #
# Plot                          #
#################################
# colour
mean_finalminusinitial_day12_plot <- ggplot(summary_data_finalminusinitial, aes(x=species,y=finalminusinitial)) 
mean_finalminusinitial_day12_plot <- mean_finalminusinitial_day12_plot + geom_point() 
mean_finalminusinitial_day12_plot <- mean_finalminusinitial_day12_plot + geom_errorbar(aes(ymin=finalminusinitial-se, finalminusinitial=area+se), width=0.1)
mean_finalminusinitial_day12_plot <- mean_finalminusinitial_day12_plot + facet_grid(Nutr ~ Temp)
mean_finalminusinitial_day12_plot <- mean_finalminusinitial_day12_plot + ylab("final area - initial area (sq. mm)")
mean_finalminusinitial_day12_plot 

#################################
# Y = final area - initial area #   
# Preliminary anova             #
# Treatments:                   #
# species,                      #
# nutrients,                    #
# temperature                   #
#################################
area_finalminusinitial_anova <- aov(finalminusinitial ~ species*Temp*Nutr, data=data_raw)
summary(area_finalminusinitial_anova)
TukeyHSD(area_finalminusinitial_anova)




#################################
# Y = final area / initial area #       
# Raw data                      #
# Plot                          #
#################################
# colour
raw_finaldivideinitial_day12_plot <- ggplot(data_raw, aes(x=species,y=finaldivideinitial))
raw_finaldivideinitial_day12_plot <- raw_finaldivideinitial_day12_plot + geom_point() 
raw_finaldivideinitial_day12_plot <- raw_finaldivideinitial_day12_plot + facet_grid(Nutr ~ Temp)
raw_finaldivideinitial_day12_plot <- raw_finaldivideinitial_day12_plot + ylab("final area / initial area")
raw_finaldivideinitial_day12_plot 

#################################
# Y = final area - initial area #      
# Average                       #
# Plot                          #
#################################
# colour
mean_finaldivideinitial_day12_plot <- ggplot(summary_data_finaldivideinitial, aes(x=species,y=finaldivideinitial)) 
mean_finaldivideinitial_day12_plot <- mean_finaldivideinitial_day12_plot + geom_point() 
mean_finaldivideinitial_day12_plot <- mean_finaldivideinitial_day12_plot + geom_errorbar(aes(ymin=finaldivideinitial-se, finaldivideinitial=area+se), width=0.1)
mean_finaldivideinitial_day12_plot <- mean_finaldivideinitial_day12_plot + facet_grid(Nutr ~ Temp)
mean_finaldivideinitial_day12_plot <- mean_finaldivideinitial_day12_plot + ylab("final area / initial area")
mean_finaldivideinitial_day12_plot 

#################################
# Y = final area - initial area #   
# Preliminary anova             #
# Treatments:                   #
# species,                      #
# nutrients,                    #
# temperature                   #
#################################
area_finaldivideinitial_anova <- aov(finaldivideinitial ~ species*Temp*Nutr, data=data_raw)
summary(area_finaldivideinitial_anova)
TukeyHSD(area_finaldivideinitial_anova)


