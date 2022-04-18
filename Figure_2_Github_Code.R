library(ggplot2)
library(ggExtra)
library(ggsci)

## import data
dolphin.data <- read.csv("Filename_here.csv")

## convert the sampling year group to a factor with levels 

dolphin.data$Year_Group <- as.factor(dolphin.data$Year_Group)

# change the sex to a factor with two levels
dolphin.data$Sex <- as.factor(dolphin.data$Sex)
levels(dolphin.data$Sex)

#change the order of the levels to appear in the order needed for the legend

dolphin.data$Year_Group <- factor(dolphin.data$Year_Group, levels = c("Pre_2008","2010-11","2015-16","2020", "2021"))

#add a colour blind friendly colour palette
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

png("Output_Filename_here.png", units = "in", width = 7, height = 5.5, res = 300 )

p <- ggplot(dolphin.data)+
  geom_point(aes(x = d13C_Suess, 
                 y=d15N, 
                 color = Year_Group,
                 shape = Sex,
                 size=0.05))+
  theme(plot.title = element_text(hjust = 0.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  theme(legend.background = element_rect(fill="white", 
                                         size=0.2, linetype="solid"))+
  theme(legend.direction = "vertical")+
  theme(legend.position = "bottom")+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19))+
  theme_classic()+
  theme(axis.text = element_text(size=12))+
  theme(axis.title = element_text(size=12))+
  scale_size(guide="none")+
  theme(legend.text = element_text(size=8))+
  guides(shape=guide_legend(override.aes = list(size=4)))+
  guides(color=guide_legend(override.aes = list(size=4)))+
  theme(legend.text.align = 0)+
  theme(legend.title = element_text(size=10))+
  scale_shape_discrete(name = "Sex", labels=c("Female","Male"))+
  theme(legend.position = c(0.8,0.3))+
  scale_color_npg()

ggMarginal(p, type = "histogram")

dev.off()
