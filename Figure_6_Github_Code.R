
library(ggplot2)
library(dplyr)
library(plyr)
library(tidyverse)
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(rphylopic)

source.data <- read.csv("insert_source_data_here.csv")

# source data should contain the mean d13C, d15N and sd for each source
# my data has sources grouped by the heading "Cluster"

source.data$Cluster <- as.factor(source.data$Cluster)

# correct the source data for TEF

source.data$d15N.TEF <- source.data$d15N+1.68
source.data$d13C.TEF <- source.data$d13C+1.60

dolphin.data <- read.csv("insert_consumer_data_here.csv")

png("Output_Filename_here.png", units = "in", width = 7, height = 5.5, res = 300 )

dolphin.data$cluster <- as.factor(dolphin.data$cluster)

ggplot(source.data, aes(x=d13C.TEF, y=d15N.TEF))+
  geom_point(aes(x = d13C.TEF, 
                 y=d15N.TEF, 
                 color = Cluster))+
  scale_colour_discrete(name="Source", labels=c("Source 1",
                                                "Source 2",
                                                "Source 3",
                                                "Source 4",
                                                "Source 5"))+
  labs(y=expression({delta}^15*N~'(\u2030)'),
       x=expression({delta}^13*C~'(\u2030)'))+
  theme_classic()+
  geom_errorbarh(aes(xmin=d13C.TEF-sd.d13C,
                     xmax=d13C.TEF+sd.d13C, color=factor(Cluster)),
                 height=0.1, show.legend = FALSE)+
  geom_errorbar(aes(ymin=d15N.TEF-sd.d15N,
                    ymax=d15N.TEF+sd.d15N,color=factor(Cluster)),
                width=0.1, show.legend = FALSE)+
  geom_point(data=dolphin.data, aes(x=d13C_Suess, y=d15N, shape=cluster), colour="grey50")+
  scale_shape_manual(name="Cluster",values = c(15,0))

dev.off()


