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
head(summary_data_maxRGR)

# re-format the data frames for plotting both on the same plot 
summary_data_maxRGR$RGR <- "maximum"
colnames(summary_data_maxRGR)[5] <- "RGRvalue"
summary_data_avgRGR$RGR <- "average"
colnames(summary_data_avgRGR)[5] <- "RGRvalue"

# combine into a single data frame 
summary_data_RGR <- rbind(summary_data_maxRGR,summary_data_avgRGR)
summary_data_RGR

############
# maxRGR   #
# Average  #
############
summary_data_RGRII <- read.csv("RGR_posthoc.csv")
summary_data_RGRII$Nutr <- factor(summary_data_RGRII$Nutr , levels=c("low","med","high"))
summary_data_RGRII$Temp <- factor(summary_data_RGRII$Temp , levels=c("18","24","30"))

combo_RGR_plot <- combo_RGR_plot + geom_text(data=summary_data_RGRII,aes(x=species, y=RGRvalue+se+0.025,label=label))

############
# RGR      #
# both max #
# and avg  #
############
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
combo_RGR_plot <- ggplot(summary_data_RGR, aes(x=species,y=RGRvalue,group=RGR)) 
combo_RGR_plot <- combo_RGR_plot + geom_point(aes(shape=RGR),size=3) 
combo_RGR_plot <- combo_RGR_plot + scale_shape_manual(values=c(1,16))
combo_RGR_plot <- combo_RGR_plot + geom_errorbar(aes(ymin=RGRvalue-se, ymax=RGRvalue+se), width=0.1)
combo_RGR_plot <- combo_RGR_plot + facet_grid(Temp ~ Nutr, labeller=labeller_function)
combo_RGR_plot <- combo_RGR_plot + xlab("Species")
combo_RGR_plot <- combo_RGR_plot + ylab(expression(paste("Relative Growth Rate (", mm^2,"/",mm^-2,"/",day^-1,")",sep="")))
combo_RGR_plot <- combo_RGR_plot + theme_bw(base_size=18)
combo_RGR_plot 

# save it 
ggsave(filename = "combo_RGR_plot.jpg", combo_RGR_plot, height=11, width=11)
