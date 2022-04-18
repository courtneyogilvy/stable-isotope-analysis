
library(SIBER)
library(rjags)

#import siber-appropriate data

#to colour the boxplots:
#make a matrix of hex codes. Need 3 shades per colour/box

siber.data <- read.csv("insert_siber_data_here.csv")

#create the siber object
#siber object  converts raw data into object that has information
#summary stats useful for plotting, and a z-score transformed version of the data 
# used in the model fitting process

siberobject<-createSiberObject(siber.data)


# Create lists of plotting arguments to be passed onwards to each 
# of the three plotting functions.

community.hulls.args<-list(col=1,lty=1,lwd=1)
group.ellipses.args<-list(n=100,p.interval=0.95,lty=1,lwd=2)
group.hull.args<-list(lty=2,col="grey20")

my.palette <- c("#E64B35","#4DBBD5","#01A087","#3C5488","#F39B7F")

png("filename_here.png", units = "in", width = 7, height = 5.5, res = 300 )

par(mfrow=c(1,1))

plotSiberObject(siberobject,
                ax.pad = 2,
                iso.order = c(1,2),
                hulls = F,community.hulls.args,
                ellipses = T,group.ellipses.args,
                group.hulls = F,group.hull.args,
                bty="L",
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030'),
                cex=10,
                #palette(value = cbPalette),
                points.order = c(16,16,16,16))
                legend("topleft",
                as.character(paste(c("Group_Name_1","Group_Name_2","Group_Name_3","Group_Name_4","Group_Name_5"))),
                pch=19,
                col=1:length(unique(siber.data$group)))
dev.off()

# Calculate summary statistics for each group: TA, SEA and SEAc
group.ML <- groupMetricsML(siberobject)
print(group.ML)

#fit the bayesian model to the data
#the model is fitted using the package rjags
# there should be one block of output for each group in the dataset

##options for running jags
#parameters
parms <- list()
parms$n.iter <- 1 * 10^6   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

#define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the 
# means. Fitting is via the JAGS method.

ellipses.posterior<-siberMVN(siberobject,parms,priors)

# The posterior estimates of the ellipses for each group can be used to
# calculate the SEA.B for each group.

col.matrix = matrix(c("#EE8172","#E64B35","#A13525",
                      "#B8E4EE","#82CFE2","#4DBBD5",
                      "#67C6B7","#01A087","#016051",
                      "#8A98B8","#3C5488","#243252",
                      "#F8C3B2","#F39B7F","#C27C66)"), nrow=3, ncol=5)

SEA.B <- siberEllipses(ellipses.posterior)



png("filename_here.png", units = "in", width = 7, height = 5.5, res = 300 )

siberDensityPlot(SEA.B, clr = col.matrix,
                 #xticklabels = colnames(group.ML), 
                 xlab = c("x axis label title here"),
                 ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                 bty = "L",
                 las = 1,
                 ylims = c(0,2.75),
                 xticklabels = c("x tick label1","x tick label2","x tick label3","x tick label4","x tick label5")
                 #main = "Bayesian Standard Ellipse Area for \n Maui dolphin samples collected pre and post 2008"
)

dev.off()