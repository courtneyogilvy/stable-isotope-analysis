


library(ggplot2)
library(ggrepel)

resample.data <- read.csv("Maui_Isotope_Resampling_Data.csv")
dolphin.data <- read.csv("Dolphin_Master_CSV_File.csv")

#remove the neonates from the dataset 

dolphin.data <- dolphin.data[-which(dolphin.data$Individual_ID =="50"),]
dolphin.data <- dolphin.data[-which(dolphin.data$Individual_ID =="52"),]


dolphin.data$cluster <- as.factor(dolphin.data$cluster)

str(resample.data)

resample.data$ID <- as.factor(resample.data$ID)
resample.data$Year <- as.factor(resample.data$Year)
resample.data$Year_Group <- as.factor(resample.data$Year_Group)

levels(resample.data$Year_Group)
resample.data$Year_Group <- factor(resample.data$Year_Group, levels = c("pre-2008","2010-11","2015-16","2020","2021"))


# Plot 1 - Resample Data,  All Years --------------------------------------

png("Resample_Maui_Data_All_Years.png", units="in", width=5, height=5, res=300)

ggplot(resample.data, aes(d13C, d15N, group = ID)) + 
geom_line()+
  geom_point(aes(colour = Year_Group, size=1))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID))

dev.off()


# Plot 2, Resample Data, All Years, With ID Labels -------------------------------------------------

png("Resample_Maui_Data_All_Years_With_ID_Labels.png", units="in", width=5, height=5, res=300)

ggplot(resample.data, aes(d13C, d15N, group = ID)) + 
  geom_line()+
  geom_point(aes(colour = Year_Group, size=1))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C, y=d15N, label=ID))
  #geom_text_repel(aes(x=d13C, y=d15N, label=ID))

dev.off()


# Plot 3 - Resample Data,  Upper Right Corner Only ------------------------

upper.right.data <- subset(resample.data, Year_Group =="2015-16")

selected <- c("17",
              "19",
              "17",
              "52",
              "32",
              "57",
              "60",
              "61",
              "84",
              "86",
              "91",
              "102",
              "104",
              "111",
              "115")
              
upper.right <- resample.data[resample.data$ID %in% selected,]

png("Resample_Maui_Data_Uppper_Right_With_ID_Labels.png", units="in", width=5, height=5, res=300)

ggplot(upper.right, aes(d13C, d15N, group = ID)) + 
  geom_line()+
  geom_point(aes(colour = Year_Group, size=1))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C, y=d15N, label=ID))+
 

dev.off()


# Resample data: Individual 19 (p1) --------------------------------------------


#png("Resample_Maui_Data_Individual_19_v3.png", units="in", width=5, height=5, res=300)

p1 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "19",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year_Group, size=50), size=7)+
  scale_colour_manual(values=c("#e64b35"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID))+
  theme(legend.position = "none")

#dev.off()

# Resample data - Individual 57 (p2) -------------------------------------------

#png("Resample_Maui_Data_Individual_57_v3.png", units="in", width=5, height=5, res=300)

p2 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "57",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year_Group), size=10)+
  scale_colour_manual(values=c("#4dbbd5","#01a087"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID), colour="white")
  #theme(legend.position="none")

#dev.off()


# Resample data - Individual 61 (p3) -------------------------------------------

#png("Resample_Maui_Data_Individual_61_v2.png", units="in", width=5, height=5, res=300)

p3 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "61",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year_Group), size=10)+
  scale_colour_manual(values=c("#4dbbd5","#01a087"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID),colour="white")
  #theme(legend.position="none")

#dev.off()


# Resample data - Individual 84 (p4) -------------------------------------------

#png("Resample_Maui_Data_Individual_84_v2.png", units="in", width=5, height=5, res=300)

p4 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "84",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year_Group), size=10)+
  scale_colour_manual(values=c("#4dbbd5","#01a087"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID), colour="white")
  #theme(legend.position="none")

#dev.off()


# Resample data - Individual 86 (p5)-------------------------------------------

#png("Resample_Maui_Data_Individual_86_v2.png", units="in", width=5, height=5, res=300)

p5 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "86",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year_Group),size=10)+
  scale_colour_manual(values=c("#4dbbd5","#01a087"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID),colour="white")
  #theme(legend.position="none")

#dev.off()


# Resample data - Individual 91 (p6) -------------------------------------------

#png("Resample_Maui_Data_Individual_91_v2.png", units="in", width=5, height=5, res=300)

p6 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "91",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year_Group),size=10)+
  scale_colour_manual(values=c("#01a087", "#3c5488","#f39b7f"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID), colour="white")
  #theme(legend.position = "none")

#dev.off()


# Resample data - Individual 102 (p7) ------------------------------------------

#png("Resample_Maui_Data_Individual_102_v2.png", units="in", width=5, height=5, res=300)

p7 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "102",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year_Group),size=10)+
  scale_colour_manual(values=c("#01a087", "#3c5488","#f39b7f"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID), colour="white")
  #theme(legend.position="none")

#dev.off()


# Resample data - Individual 104 (p8) ------------------------------------------


#png("Resample_Maui_Data_Individual_104_v2.png", units="in", width=5, height=5, res=300)

p8 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "104",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year_Group),size=10)+
  scale_colour_manual(values=c("#01a087", "#3c5488","#f39b7f"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID), colour="white")
  #theme(legend.position = "none")

#dev.off()


# Resample data - Individual 115 (p9)------------------------------------------

#png("Resample_Maui_Data_Individual_115_v2.png", units="in", width=5, height=5, res=300)

p9 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "115",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year_Group),size=10)+
  scale_colour_manual(values=c("#01a087", "#3c5488"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID), colour="white")
  #theme(legend.position="none")

#dev.off()


# Resample data - Individual 32 (p10)-------------------------------------------

#png("Resample_Maui_Data_Individual_32_v2.png", units="in", width=5, height=5, res=300)

p10 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "32",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year_Group),size=10)+
  scale_colour_manual(values=c("#4dbbd5", "#f39b7f"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID), colour="white")
  #theme(legend.position = "none")

#dev.off()


# Resample Data - Individual 19 (p11)-------------------------------------------

png("Resample_Maui_Data_Individual_19_v3.png", units="in", width=5, height=5, res=300)

dolphin.data$Year <- as.factor(dolphin.data$Year)

p11 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "19",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year),size=10)+
  #scale_colour_manual(values=c("#00B0F6", "#E76BF3"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID), colour="white")

dev.off()

# Resample data: Individual 60 (p12) --------------------------------------------



#png("Resample_Maui_Data_Individual_60_v2.png", units="in", width=5, height=5, res=300)

p12 <- ggplot(dolphin.data[dolphin.data$Individual_ID == "60",], aes(d13C_Suess, d15N)) + 
  geom_point(data=dolphin.data, aes(d13C_Suess, d15N, shape=cluster), colour="grey35")+
  scale_shape_manual(values=c(15,0))+
  geom_line()+
  geom_point(aes(colour = Year_Group),size=10)+
  scale_colour_manual(values=c("#4dbbd5","#01a087"))+
  scale_size(guide="none")+
  guides(colour = guide_legend(override.aes = list(size=3)))+
  labs(colour="Year")+
  theme_classic()+
  coord_cartesian(xlim =c(-18.5, -12.5), ylim = c(13.5, 19.5))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  geom_text(aes(x=d13C_Suess, y=d15N, label=Individual_ID), colour="white")
  #theme(legend.position = "none")

#dev.off()


# Combination Plot --------------------------------------------------------

library(ggpubr)


combined.figure <- ggarrange(p11, p2, p3,p4, p5, p6, p7, p8, p9, p10,p12,
                             ncol=3, nrow=4,
                             common.legend = TRUE, legend="bottom")

png("Combined_Resample_Plots_v3.png", units="in", width=10, height=12, res=300)

combined.figure

dev.off()
