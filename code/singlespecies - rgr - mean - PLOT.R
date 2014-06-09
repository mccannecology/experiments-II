##########################################
# Analysis of single duckweed experiment #
# Conducted by Ish - Summer 2013         #
#                                        #
# Plotting data                          #
# avgRGR                                 #
# average within a replicate             #
##########################################
library(ggplot2)

# check out the data you will use
head(data_raw)
head(summary_data_avgRGR)


############
# avgRGR   #
# Average  #
############
# Second version 
# Did a Bonferroni adjust for multiple anovas 
# three anovas are now non-significant 
# should not do post-hoc tests then 
# removed labels 
# import the post-hoc test labels 
summary_data_avgRGRII <- read.csv("avgRGR_posthocII.csv")
summary_data_avgRGRII$Nutr <- factor(summary_data_avgRGRII$Nutr , levels=c("low","med","high"))
summary_data_avgRGRII$Temp <- as.factor(summary_data_avgRGRII$Temp)
summary_data_avgRGRII$Temp <- factor(summary_data_avgRGRII$Temp , levels=c("18","24","30"))

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
mean_avgRGR_plot <- ggplot(summary_data_avgRGRII, aes(x=species,y=avgRGR)) + geom_point() 
mean_avgRGR_plot <- mean_avgRGR_plot + geom_errorbar(aes(ymin=avgRGR-se, ymax=avgRGR+se), width=0.1)
mean_avgRGR_plot <- mean_avgRGR_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
mean_avgRGR_plot <- mean_avgRGR_plot + ylab(expression(paste("Average Relative Growth Rate (", mm^2,"/",mm^-2,"/",day^-1,")",sep="")))
mean_avgRGR_plot <- mean_avgRGR_plot + xlab("Species")
mean_avgRGR_plot <- mean_avgRGR_plot + ylim(0,0.4)
mean_avgRGR_plot <- mean_avgRGR_plot + theme_bw(base_size=18)
mean_avgRGR_plot <- mean_avgRGR_plot + geom_text(data=summary_data_avgRGRII,aes(x=species, y=avgRGR+se+0.025,label=label))
mean_avgRGR_plot 

# save it 
ggsave(filename = "mean_avgRGR_plot.jpg", mean_avgRGR_plot, height=11, width=11)

############
# avgRGR   #
# Average  #
############
# import the post-hoc test labels 
summary_data_avgRGR <- read.csv("avgRGR_posthoc.csv")
summary_data_avgRGR$Nutr <- factor(summary_data_avgRGR$Nutr , levels=c("low","med","high"))
summary_data_avgRGR$Temp <- as.factor(summary_data_avgRGR$Temp)
summary_data_avgRGR$Temp <- factor(summary_data_avgRGR$Temp , levels=c("18","24","30"))

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
mean_avgRGR_plot <- ggplot(summary_data_avgRGR, aes(x=species,y=avgRGR)) + geom_point() 
mean_avgRGR_plot <- mean_avgRGR_plot + geom_errorbar(aes(ymin=avgRGR-se, ymax=avgRGR+se), width=0.1)
mean_avgRGR_plot <- mean_avgRGR_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
mean_avgRGR_plot <- mean_avgRGR_plot + ylab("Average Relative Growth Rate")
mean_avgRGR_plot <- mean_avgRGR_plot + xlab("Species")
mean_avgRGR_plot <- mean_avgRGR_plot + ylim(0,0.4)
mean_avgRGR_plot <- mean_avgRGR_plot + theme_bw(base_size=18)
mean_avgRGR_plot <- mean_avgRGR_plot + geom_text(data=summary_data_avgRGR,aes(x=species, y=avgRGR+se+0.025,label=label))
mean_avgRGR_plot 

# save it 
ggsave(filename = "mean_avgRGR_plot.jpg", mean_avgRGR_plot, height=11, width=11)
