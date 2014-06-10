##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# turionsTOT                             #
##########################################
library(ggplot2)

# check out the data you will use
head(data_turion)
head(summary_data_turions)

summary_data_turions_WB <- subset(summary_data_turions, summary_data_turions$species=="WB")

##############
# turionsTOT #
# Average    #
##############
# import the post-hoc test labels 
summary_data_turionsII <- read.csv("turions_posthoc.csv")
summary_data_turionsII$Nutr <- factor(summary_data_turionsII$Nutr , levels=c("low","med","high"))
summary_data_turionsII$Temp <- as.factor(summary_data_turionsII$Temp)
summary_data_turionsII$Temp <- factor(summary_data_turionsII$Temp , levels=c("18","24","30"))

# labelling the facet variables
nutrient_names <- list("low"="Low nutrients","med"="Medium nutrients","high"="High nutrients")
temperature_names <- list("18"=expression(paste("18 ",degree,"C")),"24"=expression(paste("24 ",degree,"C")),"30"=expression(paste("30 ",degree,"C")))

labeller_function <- function(variable,value){
  if (variable=="Temp") {
    return(temperature_names[value])
  } else {
    return(nutrient_names[value])
  }
}

# making the plot 
mean_summary_data_turions_plot <- ggplot(summary_data_turionsII, aes(x=species,y=turionsTOT)) + geom_point() 
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + geom_errorbar(aes(ymin=turionsTOT-se, ymax=turionsTOT+se), width=0.1)
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + ylab("Total turions produced")
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + xlab("Species")
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + theme_bw(base_size=18)
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + geom_text(data=summary_data_turionsII,aes(x=species, y=turionsTOT+se+25,label=label))
mean_summary_data_turions_plot 

# save it 
ggsave(filename = "mean_summary_data_turions_plot.jpg", mean_summary_data_turions_plot, height=11, width=11)




###################
# turions_per_day #
# Average         #
###################
summary_data_turions_per_day_WB <- subset(summary_data_turions_per_day, summary_data_turions_per_day$species=="WB")


# import the post-hoc test labels 
summary_data_turionsII <- read.csv("turions_posthoc.csv")
summary_data_turionsII$Nutr <- factor(summary_data_turionsII$Nutr , levels=c("low","med","high"))
summary_data_turionsII$Temp <- as.factor(summary_data_turionsII$Temp)
summary_data_turionsII$Temp <- factor(summary_data_turionsII$Temp , levels=c("18","24","30"))

# labelling the facet variables
nutrient_names <- list("low"="Low nutrients","med"="Medium nutrients","high"="High nutrients")
temperature_names <- list("18"=expression(paste("18 ",degree,"C")),"24"=expression(paste("24 ",degree,"C")),"30"=expression(paste("30 ",degree,"C")))

labeller_function <- function(variable,value){
  if (variable=="Temp") {
    return(temperature_names[value])
  } else {
    return(nutrient_names[value])
  }
}

# making the plot 
mean_turions_per_day_plot <- ggplot(summary_data_turions_per_day_WB, aes(x=species,y=turions_per_day)) + geom_point() 
mean_turions_per_day_plot <- mean_turions_per_day_plot + geom_errorbar(aes(ymin=turions_per_day-se, ymax=turions_per_day+se), width=0.1)
mean_turions_per_day_plot <- mean_turions_per_day_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
mean_turions_per_day_plot <- mean_turions_per_day_plot + ylab("Turions per day")
mean_turions_per_day_plot <- mean_turions_per_day_plot + xlab("Species")
mean_turions_per_day_plot <- mean_turions_per_day_plot + theme_bw(base_size=18)
# mean_turions_per_day_plot <- mean_turions_per_day_plot + geom_text(data=turions_per_dayII,aes(x=species, y=turions_per_day+se+25,label=label))
mean_turions_per_day_plot 

# save it 
ggsave(filename = "mean_turions_per_day_plot.jpg", mean_turions_per_day_plot, height=11, width=11)

