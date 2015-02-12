##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# turionsTOT                             #
##########################################
library(ggplot2)

# check out the data you will use
head(data_turions)
head(summary_data_turions)

summary_data_turions_WB <- subset(summary_data_turions, summary_data_turions$species=="WB")

##############
# turionsTOT #
# Average    #
##############
# import the post-hoc test labels 
summary_data_turions <- read.csv("turions_area_per_day_posthoc.csv")
summary_data_turions$Nutr <- factor(summary_data_turions$Nutr , levels=c("low","med","high"))
summary_data_turions$Temp <- as.factor(summary_data_turions$Temp)
summary_data_turions$Temp <- factor(summary_data_turions$Temp , levels=c("18","24","30"))

# labelling the facet variables
nutrient_names <- list("low"="0.5 mg N/L \n 0.08 mg P/L","med"="5 mg N/L \n 0.8 mg P/L","high"="10 mg N/L \n 1.6 mg P/L")
temperature_names <- list("18"=expression(paste("18 ",degree,"C")),"24"=expression(paste("24 ",degree,"C")),"30"=expression(paste("30 ",degree,"C")))

labeller_function <- function(variable,value){
  if (variable=="Temp") {
    return(temperature_names[value])
  } else {
    return(nutrient_names[value])
  }
}

# making the plot 
mean_summary_data_turions_plot <- ggplot(summary_data_turions, aes(x=species,y=turion_area_per_day)) + geom_point(size=3) 
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + geom_errorbar(aes(ymin=turion_area_per_day-se, ymax=turion_area_per_day+se), width=0.1)
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + ylab(expression(paste("Turion production (", mm^2,day^-1,")",sep="")))
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + xlab("Species")
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + theme_bw(base_size=18)
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + geom_text(data=summary_data_turions,aes(x=species, y=turion_area_per_day+se+0.2,label=label))
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + geom_hline(aes(intercept=0),linetype="dashed")


# save it 
ggsave(filename = "turion_production_plot_area_per_day.jpg", mean_summary_data_turions_plot, height=11, width=11)


# re-making the plot for Hydrobiologia
mean_summary_data_turions_plot <- ggplot(summary_data_turions, aes(x=species,y=turion_area_per_day)) + geom_point(size=3) 
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + geom_errorbar(aes(ymin=turion_area_per_day-se, ymax=turion_area_per_day+se), width=0.1)
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + ylab(expression(paste("Turion production (", mm^2,day^-1,")",sep="")))
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + xlab("Species")
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + theme_bw(base_size=12)
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + geom_text(data=summary_data_turions,aes(x=species, y=turion_area_per_day+se+0.2,label=label))
mean_summary_data_turions_plot <- mean_summary_data_turions_plot + geom_hline(aes(intercept=0),linetype="dashed")
mean_summary_data_turions_plot 

# save it 
ggsave(filename = "Fig02-turion_production_plot_area_per_day.jpg", mean_summary_data_turions_plot, dpi=1200, units="mm", width=174)
ggsave(filename = "Fig02-turion_production_plot_area_per_day.eps", mean_summary_data_turions_plot, dpi=1200, units="mm", width=174)



################
# turionsTOT   #
# Average      #
# Wolffia only #
################
# subset - just WB data 
summary_data_turion_area_per_day_WB <- subset(summary_data_turion_area_per_day, summary_data_turion_area_per_day$species=="WB")

# import the post-hoc test labels 
summary_data_turion_area_per_day_WB <- read.csv("turions_area_per_day_posthoc.csv")
summary_data_turion_area_per_day_WB$Nutr <- factor(summary_data_turion_area_per_day_WB$Nutr , levels=c("low","med","high"))
summary_data_turion_area_per_day_WB$Temp <- as.factor(summary_data_turion_area_per_day_WB$Temp)
summary_data_turion_area_per_day_WB$Temp <- factor(summary_data_turion_area_per_day_WB$Temp , levels=c("18","24","30"))

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
turion_area_per_day_plot <- ggplot(summary_data_turion_area_per_day_WB, aes(x=species,y=turion_area_per_day)) 
turion_area_per_day_plot <- turion_area_per_day_plot + geom_point() 
turion_area_per_day_plot <- turion_area_per_day_plot + geom_errorbar(aes(ymin=turion_area_per_day-se, ymax=turion_area_per_day+se), width=0.1)
turion_area_per_day_plot <- turion_area_per_day_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
turion_area_per_day_plot <- turion_area_per_day_plot + ylab(expression(paste("Turion production (", mm^2,day^-1,")",sep="")))
turion_area_per_day_plot <- turion_area_per_day_plot + xlab("Species")
turion_area_per_day_plot <- turion_area_per_day_plot + theme_bw(base_size=18)
turion_area_per_day_plot <- turion_area_per_day_plot + geom_text(data=summary_data_turion_area_per_day_WB,aes(x=species, y=turion_area_per_day+se+0.35,label=label))
turion_area_per_day_plot 

# save it 
ggsave(filename = "turion_area_per_day_plot.jpg", turion_area_per_day_plot, height=11, width=11)




################
# turionsTOT   #
# Average      #
# all species  #
################
# import the post-hoc test labels 
summary_data_turion_area_per_day <- read.csv("turions_area_per_day_posthoc.csv")
summary_data_turion_area_per_day$Nutr <- factor(summary_data_turion_area_per_day$Nutr , levels=c("low","med","high"))
summary_data_turion_area_per_day$Temp <- as.factor(summary_data_turion_area_per_day$Temp)
summary_data_turion_area_per_day$Temp <- factor(summary_data_turion_area_per_day$Temp , levels=c("18","24","30"))

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
turion_area_per_day_plot <- ggplot(summary_data_turion_area_per_day, aes(x=species,y=turion_area_per_day)) 
turion_area_per_day_plot <- turion_area_per_day_plot + geom_point() 
turion_area_per_day_plot <- turion_area_per_day_plot + geom_errorbar(aes(ymin=turion_area_per_day-se, ymax=turion_area_per_day+se), width=0.1)
turion_area_per_day_plot <- turion_area_per_day_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
turion_area_per_day_plot <- turion_area_per_day_plot + ylab(expression(paste("Turion production (", mm^2,day^-1,")",sep="")))
turion_area_per_day_plot <- turion_area_per_day_plot + xlab("Species")
turion_area_per_day_plot <- turion_area_per_day_plot + theme_bw(base_size=18)
turion_area_per_day_plot <- turion_area_per_day_plot + geom_text(data=summary_data_turion_area_per_day,aes(x=species, y=turion_area_per_day+se+0.35,label=label))
turion_area_per_day_plot 

# save it 
ggsave(filename = "turion_area_per_day_plot.jpg", turion_area_per_day_plot, height=11, width=11)

